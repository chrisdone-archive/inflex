# Building docker images

## sky-above/inflex/build

Build the `sky-above/inflex/build` image.

    $ docker image build -t registry.gitlab.com/sky-above/inflex/build . -f docker/build.Dockerfile

Push it to the registry:

    $ docker push registry.gitlab.com/sky-above/inflex/build

## sky-above/inflex/release

Release the `sky-above/inflex/release` image.

    $ docker image release -t registry.gitlab.com/sky-above/inflex/release docker -f docker/release.Dockerfile

Push it to the registry:

    $ docker push registry.gitlab.com/sky-above/inflex/release
