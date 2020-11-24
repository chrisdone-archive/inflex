```
docker image build . -f docker/sky-above/inflex/src.Dockerfile -t registry.gitlab.com/sky-above/inflex/src:2020-09-13
docker image build docker -f docker/sky-above/inflex/base.Dockerfile -t registry.gitlab.com/sky-above/inflex/base:2020-09-13
```

Gitlab builds the rest (patch, release, etc.).

```
docker push registry.gitlab.com/sky-above/inflex/runtime:2020-09-13
```

# Azure with kubectl

    docker run -it -v ${HOME}/.ssh:/root/.ssh mcr.microsoft.com/azure-cli

    az aks get-credentials --resource-group inflex-prod --name prod-inflex-k8s

    export KUBECONFIG=~/.az-kube/config

# Creating service principal

    az account set --subscription a48ba72b-f0f3-4638-bcd5-3981a491c27c

    bash-5.0#  az ad sp create-for-rbac --name gitlab
    Changing "gitlab" to a valid URI of "http://gitlab", which is the required format used for service principal names
    Creating a role assignment under the scope of "/subscriptions/a48ba72b-f0f3-4638-bcd5-3981a491c27c"
      Retrying role assignment creation: 1/36
    {
      "appId": "..",
      "displayName": "gitlab",
      "name": "http://gitlab",
      "password": "..",
      "tenant": "..."
    }

# Login to az with service principal

Get these details from lastpass Note 'inflex azure gitlab service principal' .

    bash-5.0# export AZ_SP_ID=...
    bash-5.0# export AZ_SP_SECRET=...
    bash-5.0# export AZ_TENANT_ID=...
    bash-5.0# az login --service-principal --username "$AZ_SP_ID" --password "$AZ_SP_SECRET" --tenant "$AZ_TENANT_ID"
    [
      {
        "cloudName": "AzureCloud",
        "homeTenantId": "...",
        "id": "...",
        "isDefault": true,
        "managedByTenants": [],
        "name": "Pay-As-You-Go",
        "state": "Enabled",
        "tenantId": "...",
        "user": {
          "name": "...",
          "type": "servicePrincipal"
        }
      }

# ngrok

    ngrok http -subdomain=inflex 3031
