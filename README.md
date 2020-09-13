```
docker image build . -f docker/sky-above/inflex/src.Dockerfile -t registry.gitlab.com/sky-above/inflex/src:2020-09-13
docker image build docker -f docker/sky-above/inflex/base.Dockerfile -t registry.gitlab.com/sky-above/inflex/base:2020-09-13
```

Gitlab builds the rest (patch, release, etc.).

```
docker push registry.gitlab.com/sky-above/inflex/runtime:2020-09-13
```

# Azure

docker run -it -v ${HOME}/.ssh:/root/.ssh mcr.microsoft.com/azure-cli

az aks get-credentials --resource-group inflex-prod --name prod-inflex-k8s

export KUBECONFIG=~/.az-kube/config
