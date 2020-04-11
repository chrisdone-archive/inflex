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

## Grab k8s auth context

This sets up kubectl to be ready for action.

    $ doctl kubernetes cluster kubeconfig save inflex-k8s

```
$ doctl kubernetes cluster kubeconfig save inflex-k8s
Notice: Adding cluster credentials to kubeconfig file found in "/home/chris/.kube/config"
Notice: Setting current-context to do-lon1-inflex-k8s
```

## Create secret for k8s to access gitlab registry

This lets the kubernetes cluster get access to the Gitlab registry,
needed to deploy inflex.

    $ kubectl create secret docker-registry gitlab-registry --docker-email="chris@skyabove.io" --docker-username="chrisdone-skyabove" --docker-server="https://registry.gitlab.com/" --docker-password=$(cat k8s-token.txt)
    secret/gitlab-registry created
