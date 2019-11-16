---
title: Putting together a form validation library
desc: A write-up about motivation and design of the forma library.
date:
  published: May 2, 2017
  updated:   June 6, 2017
tag: haskell
---

This time I want to talk about something as boring as parsing and validating
of a form on a web site. That's a pretty common thing in web development,
yet for a long time I was not satisfied with Haskell solutions we have so
far. So recently a I've published
the [`forma`](https://github.com/mrkkrp/forma) package that solves
server-side part of the task.

## Vanilla forms vs sending JSON

I think that serializing a form in JSON format and sending that via an AJAX
request is a better strategy than the traditional form submission mechanism,
for several reasons:

* Smoother user experience: no need to reload the whole page.

* Form rendering is separated and lives only in GET handler, POST (or
  whatever method you deem appropriate for your use case) handler only
  handles validation and actual effects that form submission should
  initiate.

* You get a chance to organize form input just like you want.

## Templates, frontend… defining scope

Of course this approach requires a bit of front-end coding. There are many
alternatives:

* Vanilla JavaScript and jQuery—still an option if you need just some
  lightweight helpers.

* For typed fronted: GHCJS, PureScript, Elm, etc.

* Who knows what else is happening in the JavaScript world right now.

So I decided that I'll leave writing that tiny bit of front end code to the
users of `forma`, so they could use whatever they prefer, and will
concentrate on parsing and validation of JSON data itself.

I also do not want to generate the forms themselves as it tends to be not
flexible enough. With `yesod-form`,
I
[could not even render a checkbox properly with Bootstrap 3](https://github.com/yesodweb/yesod/issues/1197),
not to mention more custom stuff. If I remember correctly Yesod's form
system separates what you can tweak in a field and what can be tweaked in a
function like `renderBootstrap3`, and it does get in your way, as this
simple checkbox example shows. The types do not allow you pass a hint of
what type of field you have so `renderBootstrap3` could wrap different
fields differently (you could still hack, but it's not worth it). So I
decided I'll write the markup myself, or at least define widgets per field
type (parametrized in a nice typed way) and construct forms from that (but
it's a topic for a different blog post).

If you have
used [`digestive-functors-heist`](https://hackage.haskell.org/package/digestive-functors-heist),
it's pretty close to constructing forms from per-field widgets in Yesod, but
be careful not to make a typo or use a wrong type of input, just like most
part of Snap ecosystem, it's all too dynamic and error-prone (not a fun of
Snap!).

So I think it's best not to constrain end user and let him/her choose how to
render forms and what frontend solution to use. With the scope of `forma`
defined, we can start.

## The aim

I'd like to grab a `Value` and parse it, validate the parsed values in
parallel, then I want to return another `Value` to send as response body
back. It's easy to get it wrong though.

## Parsing and validation

Immediately, there seem to be two types of errors that should be treated
differently:

1. Parse errors (when we fail to deconstruct given `Value`)—these are fatal,
   and should cause short-circuiting behavior.

2. Validation errors—these should be collected “in parallel” from all
   fields, because we want to return all errors at once.

The point 2 immediately limits us to `Applicative`, because we want to
continue after a failure. It's easy to understand this because to define
`Monad` you need to define `(>>=)`, which captures the essence of a
computation with context that depends on a previously computed value. With 2
in mind, if validation fails, we would have nothing to give to `f` in `m >>=
f`.

So the first thing to define is the state of a branch of parsing:

```haskell
-- | State of a parsing branch.

data BranchState a
  = ParsingFailed String
    -- ^ Parsing of JSON failed, this is fatal, we shut down and report the
    -- parsing error.
  | ValidationFailed FieldError
    -- ^ Validation of a field failed. This is also fatal but we still try
    -- to validate other branches (fields) to collect as many validation
    -- errors as possible.
  | Succeeded a
    -- ^ Success, we've got a result to return.
  deriving Functor
```

GHC can define `Functor` instance for us since it's trivial. `Applicative`
looks like this:

```haskell
instance Applicative BranchState where
  pure                                            = Succeeded
  (ParsingFailed msg)   <*> _                     = ParsingFailed msg
  (ValidationFailed _)  <*> (ParsingFailed msg)   = ParsingFailed msg
  (ValidationFailed e0) <*> (ValidationFailed e1) = ValidationFailed (e0 <> e1)
  (ValidationFailed e)  <*> Succeeded _           = ValidationFailed e
  Succeeded _           <*> (ParsingFailed msg)   = ParsingFailed msg
  Succeeded _           <*> (ValidationFailed e)  = ValidationFailed e
  Succeeded f           <*> Succeeded x           = Succeeded (f x)
```

The reader may notice similarities
with [`Validation`](https://hackage.haskell.org/package/Validation), but I
also needed `ParsingFailed` to terminate the whole thing, so this is a sort
of hybride between `Validation` and `Either`.

The next thing is to define the applicative parser itself, easily done:

```haskell
newtype FormParser m a
  = FormParser (Value -> m (BranchState a))

instance Functor m => Functor (FormParser m) where
  fmap f (FormParser x) = FormParser (fmap (fmap f) . x)

instance Applicative m => Applicative (FormParser m) where
  pure x = (FormParser . const . pure) (Succeeded x)
  (FormParser f) <*> (FormParser x) = FormParser $ \v ->
    pure (<*>) <*> f v <*> x v

instance Applicative m => Alternative (FormParser m) where
  empty = (FormParser . const . pure) (ParsingFailed "empty")
  (FormParser x) <|> (FormParser y) = FormParser $ \v ->
    let g x' y' =
          case x' of
            ParsingFailed    _ -> y'
            ValidationFailed _ -> x'
            Succeeded        _ -> x'
    in pure g <*> x v <*> y v
```

Just think what such a parser should be? It should take a `Value` and return
result, which is already modelled by `BranchState`, so we just wrap that
function, call it `FormParser` and let the types guide us.

An important thing to note is that with the short-circuiting behavior of
`ParsingFailed`, we can define a meaningful `Alternative`, which means we
will be able to use `optional` and `(<|>)` in our parsers instead of what
`digestive-functors` do (they have `text` and then `optionalText`, and so
for every field type, yuck).

## Field names

I hate typos, so I always thought that it would be nice if something could
catch them for me. Also I want something to force me into updating field
names everywhere, should I decide to rename them. Since field names for
forms are usually known at compile time, I decided to keep them at the type
level.

If we start by defining a collection of field names like this:

```haskell
type LoginFields = '["username", "password", "remember_me"]
```

Then it's easy to force the user to pick from those names only. This solves
the typo problem, and also will force to update fields everywhere in the
case of renaming. So here we go:

```haskell
newtype SelectedName (names :: [Symbol])
  = SelectedName Text
  deriving (Eq, Show)

type family InSet (n :: Symbol) (ns :: [Symbol]) :: Constraint where
  InSet n '[]    = TypeError
    ('Text "The name " ':<>: 'ShowType n ':<>: 'Text " is not in the given set."
     ':$$:
     'Text "Either it's a typo or you need to add it to the set first.")
  InSet n (n:ns) = ()
  InSet n (m:ns) = InSet n ns

-- | Pick a name from a given collection of names.
--
-- Typical usage:
--
-- > type Fields = '["foo", "bar", "baz"]
-- >
-- > myName :: SelectedName Fields
-- > myName = pick @"foo" @Fields
--
-- It's a good idea to use 'pick' to get field names not only where this
-- approach is imposed by the library, but everywhere you need to use the
-- field names, in your templates for example.

pick :: forall (name :: Symbol) (names :: [Symbol]).
  ( KnownSymbol name
  , InSet name names )
  => SelectedName names
pick = (SelectedName . T.pack . symbolVal) (Proxy :: Proxy name)

-- | Extract a 'Text' value from 'SelectedName'.

unSelectedName :: SelectedName names -> Text
unSelectedName (SelectedName txt) = txt
```

I just don't export the `SelectedName` constructor and the only way to get a
`SelectedName` is via the `pick` smart constructor which also attaches the
tag in the form of a set of field names at the type level to that value. We
now statically know that given `Text` value is in that set.

We need now to index `BranchState` and `FormParser` by that collection of
names too:

```haskell
data BranchState (names :: [Symbol]) a = …
newtype FormParser (names :: [Symbol]) m a = …
```

## Field errors

We can now apply the machinery to define `FieldError` and a smart
constructor for it:

```haskell
-- | Error info in JSON format associated with a particular form field.
-- Parametrized by @names@, which is a collection of field names (on type
-- level) the target field belongs to. 'FieldError' is an instance of
-- 'Semigroup' and that's how you combine values of that type. Note that
-- it's not a 'Monoid', because we do not want to allow empty 'FieldError's.

data FieldError (names :: [Symbol])
  = FieldError (Map Text Value)
  deriving (Eq, Show)

instance Semigroup (FieldError names) where
  (FieldError x) <> (FieldError y) = FieldError (M.union x y)

-- | This is a smart constructor for the 'FieldError' type, and the only way
-- to obtain values of that type.
--
-- Typical usage:
--
-- > type Fields = '["foo", "bar", "baz"]
-- >
-- > myError :: FieldError Fields
-- > myError = mkFieldError (pick @"foo" @Fields) "That's all wrong."
--
-- See also: 'pick' (to create 'SelectedName').

mkFieldError :: ToJSON e
  => SelectedName names -- ^ The field name
  -> e                 -- ^ Data that represents error
  -> FieldError names
mkFieldError name x =
  FieldError (M.singleton (unSelectedName name) (toJSON x))
```

## Field parser

Now we can define a parser for a single field:

```haskell
field :: forall (name :: Symbol) (names :: [Symbol]) m e s a.
  ( KnownSymbol name
  , InSet name names
  , Monad m
  , ToJSON e
  , FromJSON s )
  => (s -> ExceptT e m a)
     -- ^ Checker that performs validation and possibly transformation of
     -- the field value
  -> FormParser names m a
field check = FormParser $ \v -> do
  let name = pick @name @names
      f :: Value -> A.Parser s
      f = withObject "form field" (.: unSelectedName name)
      r = A.parseEither f v
  case r of
    Left parseError -> pure (ParsingFailed parseError)
    Right r' -> do
      e <- runExceptT (check r')
      return $ case e of
        Left verr ->
          (ValidationFailed (mkFieldError name verr))
        Right x ->
          (Succeeded x)
```

So this assumes that the top level `Value` is a dictionary and every its
properly or key-vaule pair is a field. That's the format of input we want to
parse.

Typical usage makes a good use of a newer GHC feature called
`TypeApplications`, it looks like this:

```haskell
myFieldParser = field @"username" myChecker
```

This lightweight `@"username"` syntax unifies the type variable `name` (of
the kind `Symbol`) with `"username"` and `InSet` constraint imposed by the
use of `mkFieldError` makes sure it's in the set of field names `names`.

Note the `myChecker` thing of the type `s -> ExceptT e m a`. `s` is the type
of thing that we parse from `Value` initially, then we can transform it and
validate within the `EitherT` monad transformer, which may contain any monad
`m`, such as a monad that allows you to lookup things in a database or
anything else. Since I'd often like to transform the `s` value to other
type, resulting value has a different type `a`. Finally as long as `e` is
convert-able to JSON (note the `ToJSON e` constraint), we're OK with
accepting it as an error message.

This approach allows to grow a vocabulary of checkers-validators and since
they are Kleisli arrows, they are easily composed with `(>=>)`. Note how we
have a single way to do all validation, while `digestive-functors` has
`validate`, `validateOptional`, and `validateM`. And when I had to use it, I
also had to define `validateOptionalM`.

Sometimes we don't want to validate anything, but `e` type variable will
tend to be ambiguous, so it's handy to define another version of `field`
that does not require a checker at all:

```haskell
-- | The same as 'field', but does not require a checker.

field' :: forall (name :: Symbol) (names :: [Symbol]) m a.
  ( KnownSymbol name
  , InSet name names
  , Monad m
  , FromJSON a )
  => FormParser names m a
field' = field @name check
  where
    check :: a -> ExceptT () m a
    check = return
```

Such parsers can be combined using the applicative notation like so:

```haskell
loginForm :: Monad m => FormParser LoginFields m LoginForm
loginForm = LoginForm
  <$> field @"username" notEmpty
  <*> field @"password" notEmpty
  <*> field' @"remember_me"
```

## Running the form

If we call the function that runs our form `runForm`, what its type should
be? An obvious idea:

```haskell
runForm :: (Monad m, ToJSON b)
  => FormParser names m a -- ^ The form parser to run
  -> Value             -- ^ Input for the parser
  -> (a -> m b)        -- ^ Callback that is called on success
  -> m Value           -- ^ The result to send back to the client
```

This is however is not good enough. `yesod-form` and `digestive-functors`
show us a shortcoming to be aware of: validation is not only about checking
individual fields, there may be a need to validate a field using values of
other fields. So we need to provide a way to signal validation errors even
when individual fields look OK.

A typical example would be the same login form. I'd like to signal a
validation error if password is incorrect, but I can only check that if I
already know the username. It's not possible with `yesod-from` (you can only
check that in handler and display a message but it won't appear under the
password field), but we can do better:

```haskell
-- | This a type that user must return in the callback passed to the
-- 'runForm' function. Quite simply, it allows you either report a error or
-- finish successfully.

data FormResult (names :: [Symbol]) a
  = FormResultError (FieldError names)
    -- ^ Form submission failed, here are the validation errors.
  | FormResultSuccess a
    -- ^ Form submission succeeded, send this info.
  deriving (Eq, Show)

runForm :: (Monad m, ToJSON b)
  => FormParser names m a -- ^ The form parser to run
  -> Value             -- ^ Input for the parser
  -> (a -> m (FormResult names b)) -- ^ Callback that is called on success
  -> m Value           -- ^ The result to send back to the client
runForm (FormParser p) v f = do
  r <- p v
  case r of
    ParsingFailed parseError -> return . toJSON $
      def { responseParseError = pure parseError }
    ValidationFailed validationError -> return . toJSON $
      def { responseFieldError = pure validationError }
    Succeeded x -> do
      r' <- f x
      return . toJSON $ case r' of
        FormResultError validationError ->
          def { responseFieldError = pure validationError }
        FormResultSuccess result ->
          def { responseResult = toJSON result }
```

To construct a `FieldError`, the user is still forced to use `pick` and then
`(<>)` from `Data.Semigroup` to merge several `FieldError`s.

The rest is a simple boilerplate that renders the final `Value`:

```haskell
data Response (names :: [Symbol]) = Response
  { responseParseError :: Maybe String
  , responseFieldError :: Maybe (FieldError names)
  , responseResult     :: Value }

instance Default (Response names) where
  def = Response
    { responseParseError = Nothing
    , responseFieldError = Nothing
    , responseResult     = Null }

instance ToJSON (Response names) where
  toJSON Response {..} = object
    [ "parse_error"  .= responseParseError
    , "field_errors" .= maybe (Object HM.empty) toJSON responseFieldError
    , "result"       .= responseResult ]

instance ToJSON (FieldError names) where
  toJSON (FieldError m) = (object . fmap f . M.toAscList) m
    where
      f (name, err) = name .= err
```

A complete example of `forma` usage would be something like this:

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import Control.Monad.Except
import Data.Aeson
import Data.Text (Text)
import Web.Forma
import qualified Data.Text as T

type LoginFields = '["username", "password", "remember_me"]

data LoginForm = LoginForm
  { loginUsername   :: Text
  , loginPassword   :: Text
  , loginRememberMe :: Bool
  }

loginForm :: Monad m => FormParser LoginFields m LoginForm
loginForm = LoginForm
  <$> field @"username" notEmpty
  <*> field @"password" notEmpty
  <*> field' @"remember_me"

notEmpty :: Monad m => Text -> ExceptT Text m Text
notEmpty txt =
  if T.null txt
    then throwError "This field cannot be empty."
    else return txt

myInput :: Value
myInput = object
  [ "username"    .= ("Bob" :: Text)
  , "password"    .= ("123" :: Text)
  , "remember_me" .= True
  ]

main :: IO ()
main = do
  r <- runForm loginForm myInput $ \LoginForm {..} -> do
    print loginUsername
    print loginPassword
    print loginRememberMe
    return (FormResultSuccess ())
  print r
```

A good toy to play with to get a taste of the package.

## Conclusion

I'm yet to use `forma` in a large project. I'm playing with replacing
`yesod-form` with `forma` in a personal project, and chances are guys at
work will use it for a new greenfield project we have, so after a while I
should have some feedback about this design.
