# Setup kubectl context

export KUBECONFIG=~/.az-kube/config

# Make docker images

```
docker image build . -f docker/sky-above/inflex/src.Dockerfile -t registry.gitlab.com/sky-above/inflex/src:2020-09-13
docker image build docker -f docker/sky-above/inflex/base.Dockerfile -t registry.gitlab.com/sky-above/inflex/base:2020-09-13
```

Gitlab builds the rest (patch, release, etc.).
