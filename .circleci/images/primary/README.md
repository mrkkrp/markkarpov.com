# How to generate a new image

Run the `build-image.sh` script from this directory, like so:

```console
$ ./build-image.sh X.X.X
```

Where `X.X.X` is the version of image we're generating.

Then push it:

```console
$ docker push mrkkrp/mk-com:X.X.X
```

Then update image names in `.circleci/config.yml`.
