---
title: Req 1.0.0, HTTP client, and streaming
desc: In this post I describe essential changes to Req in version 1.0.0, dig into the low-level API of http-client, how it does streaming, and how it is used in Req.
date:
  published: December 2, 2017
---

A year ago I first published
[`req`](https://hackage.haskell.org/package/req)—an HTTP client library on
top of the venerable
[`http-client`](https://hackage.haskell.org/package/http-client). The past
year added some useful functionality to the package, such as automatic
retrying on response timeouts and such. Reviewing the code recently and
digging into the low-level details of `http-client` showed me that some
things could have been done better, and so I decided to spend some time to
get all the details right and release a new major version of the package.

This post is about changes in `req-1.0.0`, some details of how `http-client`
works, in particular how it streams response bodies. Even if you do not want
to use `req` for some reason, I hope the post still may be interesting.

## Banishing `IO` from configuration

Let's warm up discussing changes in type signatures of some fields in the
`HttpConfig` record before we get to more tricky stuff.

First, I found the signature of `httpConfigCheckResponse` unsatisfactory.
Here it is:

```haskell
httpConfigCheckResponse :: forall r. HttpResponse r => L.Request -> r -> IO ()
```

And for comparison, here is also what `http-client` has:

```haskell
checkResponse :: Request -> Request -> Response BodyReader -> IO ()
```

This function is used to throw an exception on non-2xx status codes right
after receiving status code and headers, before streaming of the response
body.

`httpConfigCheckResponse` from `req` sucks because it's defined in terms of
`HttpResponse`. `HttpResponse` is a type class that defines how to consume
response body and interpret it as that `r` thing. So it turns out that we
are bound to first consume the whole response body and interpret it and only
after that we can use `httpConfigCheckResponse` to actually check if we're
OK with response status code, etc. This is not very good, and I'll talk more
about that later in the post.

Let's see now why `checkResponse` could be better as well. First off, the
signature is a bit funny, because it takes `Request` twice. This happens
because `checkResponse` is actually a field of the `Request` record itself:

```haskell
data Request = Requset
  { -- …
  , checkResponse :: Request -> Response BodyReader -> IO ()
  }
```

Although the data constructor of `Request` is not exported, `checkResponse`
is still a thing attached to a `Request`. So you first get it from request
by feeding your request to `checkResponse` and then you feed it your request
again together with `Response BodyReader` and then it results in `IO ()`. So
clearly, the only purpose of the function (as highlighted in the docs) is to
throw something (or not to throw…).

The first uncomfortable thing for me is the `IO` monad. By living in `IO`,
this function, which has a clear purpose—checking response before streaming
of response body starts, can do pretty arbitrary stuff. Of course, by
convention, nobody probably puts anything exotic there, but if we start to
follow this line of thought, then what all this talk about separation of
effects and concerns all about?

The `checkResponse` function can also just start streaming of response body
on its own because `type BodyReader = IO ByteString` and this is the
low-level device via which actual consumption of response body happens, but
more about that latter.

Clearly, it's not so hard to come up with a better signature. Let's see, we
want it to take `Request` (may be useful to know the original request after
all), then `Response`, but `Response` is a functor. What should be inside?
Intuitively, we have the response body inside in some form, but the concrete
form should not matter for us because the function is not about doing
anything with the body:

```haskell
httpConfigCheckResponse
  :: forall b.
     Request
  -> Response b
  -> …
```

However, `StatusCodeException (Response ()) ByteString` (the thing we want
to throw in case of non-2xx status code) allows to attach beginning of
response body as the second argument of the data constructor. That may be
useful, so we'll just take the “preview” of the body (first 1024 bytes) as
the third argument (how we get those should not concern us right now):

```haskell
httpConfigCheckResponse
  :: forall b.
     Request
  -> Response b
  -> ByteString
  -> …
```

If we just want to decide whether we want to throw or not, then why not
return `HttpExceptionContent` (looks like the right thing to return here)
inside `Maybe`:

```haskell
httpConfigCheckResponse
  :: forall b.
     Request
  -> Response b
  -> ByteString
  -> Maybe HttpExceptionContent
```

Hey, *the function is pure!* It only can do what it's supposed to do:
checking status code, headers, and possibly indicating a failure.

While I'm at it, I do think throwing on non-2xx response codes is a good
thing. Older versions of `http-client` did that, then Michael Snoyman made a
poll asking the community whether `http-client` should throw on non-2xx
response codes or not. The community chose not to throw and let users check
the status code instead. I guess this is because *we like purity too much*.

Nobody really makes an HTTP request to get 404 or 500. Especially when
`http-client` implements following of redirects and `req` adds retrying, if
you get a non-2xx status code, something certainly went wrong, it's not what
you expected, so it's an exceptional situation that should be treated as
such.

OK, next we have the two fields that have to do with retrying (I first show
the old signatures):

```haskell
httpConfigRetryPolicy :: RetryPolicyM IO
httpConfigRetryJudge
  :: forall r. HttpResponse r => RetryStatus -> r -> IO Bool
```

We use the [`retry`](https://hackage.haskell.org/package/retry) package for
retrying, and the types come from there.

I could not find a reason to keep `IO` in these as well, because even if the
retrying logic should depend on some values that are to be obtained via
`IO`, then this can be done before construction of `HttpConfig` and the
functions themselves can stay pure:

```haskell
httpConfigRetryPolicy :: RetryPolicy -- forall m. RetryPolicyM m
httpConfigRetryJudge  :: forall b. RetryStatus -> Response b -> Bool
-- ^ The function is used to decide whether to retry a request. 'True'
-- means that the request should be retried.
```

(I'm willing to bring `IO` back if someone opens an issue with a compelling
use-case.)

Similarly, to `httpConfigCheckResponse`, we can't use `HttpResponse` here
because we don't have it yet: streaming and interpretation happen when we're
finished with retrying. So we replace it with a more concrete type `forall
b. Response b`.

## Making an HTTP request with `http-client`

Streaming of response bodies popped up a couple times already and it'll be
the main topic for the rest of the post. So it makes sense to refresh how we
do a request and consume response with `http-client`. After all, many of us
do not use the low-level API preferring the interface that `http-conduit`
provides or functions like `httpLbs` that do the whole thing hiding the
details.

So here we go:

1. First we run `responseOpen :: Request -> Manager -> IO (Response
   BodyReader)`. This initiates an HTTP request and allocates some
   resources.

2. In the previous step we have obtained `type BodyReader = IO ByteString`.
   This is a poor-man streaming tool. We can call the action repeatedly
   until it returns the empty `ByteString`, which, by convention, indicates
   that we have consumed the entire response body. Once we get the empty
   string, we can also assume that the connection has been closed, so don't
   call `BodyReader` again after it has returned the empty `ByteString`!

3. Now it's a good idea to call `responseClose :: Response a -> IO ()` to
   make sure that we've closed the connection or returned it to the
   `Manager` so it can re-use it.

It makes sense that to avoid leaking connections `responseOpen` and
`responseClose` calls should go in pairs, thus we have the `withResponse`
helper:

```haskell
withResponse req man = bracket (responseOpen req man) responseClose
```

The documentation says:

> It is recommended that you use this function in place of explicit calls to
> `responseOpen` and `responseClose`.

We'll see however that we'll have to resort to manual messing with
`responseOpen` and `responseClose` to support retrying properly.

Finally, as an example of getting response body as a lazy `ByteString`, here
is how `httpLbs` is implemented:

```haskell
httpLbs :: Request -> Manager -> IO (Response L.ByteString)
httpLbs req man = withResponse req man $ \res -> do
  bss <- brConsume $ responseBody res
  return res { responseBody = L.fromChunks bss }
```

`brConsume` just calls the given `BodyReader` as I described above and
returns all the chunks it has received.

## Problems with `HttpResponse`

Previously the `HttpResponse` type class made the process of performing a
request not granular enough:

```haskell
-- | A type class for response interpretations. It allows to fully control
-- how request is made and how its body is parsed.

class HttpResponse response where

  -- | The associated type is the type of body that can be extracted from an
  -- instance of 'HttpResponse'.

  type HttpResponseBody response :: *

  -- | The method describes how to get the underlying 'Response' record.

  toVanillaResponse :: response -> Response (HttpResponseBody response)

  -- | This method describes how to make an HTTP request given 'Request'
  -- (prepared by the library) and 'Manager'.

  getHttpResponse :: Request -> Manager -> IO response

  -- | Construct a “preview” of response body. It is recommend to limit the
  -- length to 1024 bytes. This is mainly used for inclusion of response
  -- body fragments in exceptions.
  --
  -- __Note__: in versions 0.3.0–0.4.0 this function returned @'IO'
  -- 'ByteString'@.
  --
  -- @since 0.5.0

  makeResponseBodyPreview :: response -> ByteString
```

In English: you can either made a request and consume/interpret the entire
response body or you can choose not to make a request. No third option. By
now it should be clear that we would like to make a request and inspect (at
least) response status code before we decide to consume the response body.

Not only this new behavior is desirable, it is necessary. The old `req`
versions forced interpretation of response even when status code indicated a
failed request: in the case of JSON it meant that response was parsed even
when it probably did not contain valid JSON that we expected, because we
first did parsing, and only after that we checked the status code.

So what signature `getHttpResponse` should have? What is the purpose of the
method? Well, now that we know how to stream response bodies, let's have
`getHttpResponse` do just that:

```haskell
getHttpResponse :: Response BodyReader -> IO response
```

That's what we really want from it, right?

And another question is about this `makeResponseBodyPreview` method. If
`HttpResponse` just describes how to consume the whole body and interpret
it, wouldn't it be possible to “preview” the first 1024 bytes before
`getHttpResponse` gets called? Indeed, I found a way to do that, so why
implementers of instances of `HttpResponse` should concern themselves with
`makeResponseBodyPreview` at all? To hell with it, let's remove it
altogether!

## `reqBr`

Exception handling, retrying, and all that stuff `req` does for us is really
handy, we don't want to lose it if we want to do something less standard
with response body, so I guess it's a good idea to add a function that would
work just like the familiar `req`, but would allow to consume `BodyReader`
manually, in a custom way, without defining new instances of `HttpResponse`:

```haskell
-- | A version of 'req' that does not use one of the predefined instances of
-- 'HttpResponse' but instead allows the user to consume @'Response'
-- 'BodyReader'@ manually, in a custom way.
--
-- @since 1.0.0

reqBr
  :: ( MonadHttp    m
     , HttpMethod   method
     , HttpBody     body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body) )
  => method            -- ^ HTTP method
  -> Url scheme        -- ^ 'Url'—location of resource
  -> body              -- ^ Body of the request
  -> Option scheme     -- ^ Collection of optional parameters
  -> (Response BodyReader -> IO a) -- ^ How to consume response
  -> m a               -- ^ Result
```

And then we can have `req` just as:

```haskell
req method url body Proxy options =
  reqBr method url body options getHttpResponse
```

`reqBr` is an intermediate step between the most user-friendly function
`req` and the more low-level `req'`:

```haskell
-- | Mostly like 'req' with respect to its arguments, but accepts a callback
-- that allows to perform a request in arbitrary fashion.
--
-- This function /does not/ perform handling\/wrapping exceptions, checking
-- response (with 'httpConfigCheckResponse'), and retrying. It only prepares
-- 'Request' and allows you to use it.
--
-- @since 0.3.0

req'
  :: forall m method body scheme a.
     ( MonadHttp  m
     , HttpMethod method
     , HttpBody   body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body) )
  => method            -- ^ HTTP method
  -> Url scheme        -- ^ 'Url'—location of resource
  -> body              -- ^ Body of the request
  -> Option scheme     -- ^ Collection of optional parameters
  -> (Request -> Manager -> m a) -- ^ How to perform request
  -> m a               -- ^ Result
```

We'll see how `reqBr` can be used for streaming with `conduit` in the next
section, but right now we still have to write it!

Let's put everything together:

```haskell
reqBr method url body options consume =
  req' method url body options $ \request manager -> do
    HttpConfig {..}  <- getHttpConfig -- (1)
    let wrapVanilla = handle (throwIO . VanillaHttpException) -- (2)
        wrapExc     = handle (throwIO . LI.toHttpException request) -- (3)
        withRRef    = bracket -- (4)
          (newIORef Nothing)
          (readIORef >=> mapM_ L.responseClose)
    (liftIO . try . wrapVanilla . wrapExc) (withRRef $ \rref -> do
      let openResponse = mask_ $ do -- (6)
            r  <- readIORef rref
            mapM_ L.responseClose r
            r' <- L.responseOpen request manager
            writeIORef rref (Just r')
            return r'
      r <- retrying -- (5)
        httpConfigRetryPolicy
        (\st r -> return $ httpConfigRetryJudge st r)
        (const openResponse)
      (preview, r') <- grabPreview bodyPreviewLength r -- (7)
      mapM_ LI.throwHttp (httpConfigCheckResponse request r' preview) -- (8)
      consume r') -- (9)
      >>= either handleHttpException return -- (10)
```

All right, there are a lot of gotcha moments and details to take care of.
Let's see:

1. First we get `HttpConfig` with `getHttpConfig`, which is a method of
   `MonadHttp`, where we currently are. Next we extract individual fields
   with help of the `RecordWildcards` language extension.

2. This exception wrapper catches `http-client`'s `HttpException`s and wraps
   them in `req`'s `HttpException`s.

3. Similarly, this is a lower level wrapper that turns
   `HttpExceptionContentWrapper` into `HttpException` that is then re-thrown
   (which in turn will be wrapped once more by `wrapVanilla`). And
   `HttpExceptionContentWrapper` is thrown via `LI.throwHttp` from (8).

     If you're wondering WTF (like I did), here a quote of the docs of
     `HttpExceptionContentWrapper`:

     > A newtype wrapper which is not exported from this library but is an
     > instance of `Exception`. This allows `HttpExceptionContent` to be
     > thrown (via this wrapper), but users of the library can't accidentally
     > try to catch it (when they *should* be trying to catch
     > `HttpException`).

     Ah, exceptions…

4. We're going to use `IORef` to keep track of currently open response,
   because there is no way to use `withResponse` with `retrying` (5) or pass
   `Response BodyReader` around in a satisfactory way remaining
   exception-safe. (Or I'm not smart enough to figure how to do that.) So
   here we just create a new `IORef (Maybe (Response BodyReader))` inside
   `bracket` and read it/close, if it's not `Nothing`. `Nothing` is what we
   start with, because there is no response yet.

5. `retrying` is from the `retry` package that I've mentioned previously. We
   feed it `httpConfigRetryPolicy`, `httpConfigRetryJudge` (we've got to
   lift it a little bit because it should be in the hosting monad), then we
   provide the action we want to run/retry `openResponse` ignoring
   `RetryStatus` by wrapping it with `const`.

6. It's essential that we keep the response in that `IORef` up-to-date,
   always, in exception-safe manner. For that we need to suspend delivery of
   asynchronous exceptions with `mask_`. First we read the response from
   `IORef` and close it if it's not `Nothing` (that would be the response
   from the last attempt, it's of no use by now). After that we can run
   `responseOpen` and then update the `IORef` writing the new value there.

     Note that even throw we have interruptible masking here, it's OK.
     `responseClose` and `responseOpen` are most probably blocking and thus
     inturruptible, so the mask can be pierced when we're running these
     actions, but immediately after them, it cannot be pierced, so `IORef`
     stays up-to-date. The enclosed `bracket` thus always gets a chance to
     release the connection.

7. `grabPreview` uses `BodyReader` to get first 1024 bytes of response body
   without streaming more than that. It also returns the updated response
   `r'` which has `BodyReader` that will stream like we did not mess with it
   at all. I won't spend time describing how this is done, the curious ones
   can read the code.

8. Now we can use the `httpConfigCheckResponse` and throw what it says to
   throw, if anything.

9. Finally we call the provided `consume` function giving it `Response
   BodyReader` for streaming of response body.

10. Here we just let `handleHttpException` (another method of `MonadHttp`)
    do its thing when we catch exceptions. Otherwise the final result is
    just `return`ed.

## Streaming with `reqBr`

`BodyReader` can be turned into a `conduit` source rather straightforwardly:

```haskell
-- | Turn @'Response' 'BodyReader'@ into a 'Producer'.
--
-- @since 1.0.0

responseBodySource :: MonadIO m
  => Response BodyReader   -- ^ Response with body reader
  -> Producer m ByteString -- ^ Response body as a 'C.Producer'
responseBodySource = bodyReaderSource . responseBody

bodyReaderSource :: MonadIO m => BodyReader -> Producer m ByteString
bodyReaderSource br = go
  where
    go = do
      bs <- liftIO (brRead br)
      unless (B.null bs) $ do
        yield bs
        go
```

With this, we can do streaming without losing benefits of `req` if the whole
pipeline can be run from plain `IO`, for example:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Conduit ((.|), runConduitRes)
import Data.Default.Class
import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import qualified Data.Conduit.Binary as CB

main :: IO ()
main = runReq def $ do
  let size = 100000 :: Int
  reqBr GET (https "httpbin.org" /: "bytes" /~ size) NoReqBody mempty $ \r ->
    runConduitRes $
      responseBodySource r .| CB.sinkFile "my-file.bin"
```

Otherwise the older workflow involving `req'` should be adopted:

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit
import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Network.HTTP.Client as L

instance MonadHttp (ConduitM i o (ResourceT IO)) where
  handleHttpException = liftIO . throwIO

main :: IO ()
main = runConduitRes $ do
  let size = 100000 :: Int
  req' GET (https "httpbin.org" /: "bytes" /~ size) NoReqBody mempty
    (\request manager ->
      bracketP (L.responseOpen request manager) L.responseClose
        responseBodySource)
    .| CB.sinkFile "my-file.bin"
```

`req'` does not open/close connections, handle exceptions, and does not
perform retrying though, so you're on your own with that stuff.

`responseBodySource` as well as other helpers for using `req` with `conduit`
are available in the (tiny)
[`req-conduit`](https://hackage.haskell.org/package/req-conduit) package,
which was updated to be used with `req-1.0.0`.

## Conclusion

That's it! I hope that in the version 1.0.0 `req` has become a bit better at
what it tries to do. I also want to thank people who say kind words about my
open source work, I really appreciate that, and it certainly motivates me
contribute more to the Haskell ecosystem :-)
