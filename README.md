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

# Installing prometheus

    helm install prometheus prometheus-community/prometheus --namespace monitoring

    [bat,exa,fd,procs,ytop,rg] $ helm install prometheus prometheus-community/prometheus --namespace monitoring
    NAME: prometheus
    LAST DEPLOYED: Wed Dec  9 11:31:36 2020
    NAMESPACE: monitoring
    STATUS: deployed
    REVISION: 1
    TEST SUITE: None
    NOTES:
    The Prometheus server can be accessed via port 80 on the following DNS name from within your cluster:
    prometheus-server.monitoring.svc.cluster.local


    Get the Prometheus server URL by running these commands in the same shell:
      export POD_NAME=$(kubectl get pods --namespace monitoring -l "app=prometheus,component=server" -o jsonpath="{.items[0].metadata.name}")
      kubectl --namespace monitoring port-forward $POD_NAME 9090


    The Prometheus alertmanager can be accessed via port 80 on the following DNS name from within your cluster:
    prometheus-alertmanager.monitoring.svc.cluster.local


    Get the Alertmanager URL by running these commands in the same shell:
      export POD_NAME=$(kubectl get pods --namespace monitoring -l "app=prometheus,component=alertmanager" -o jsonpath="{.items[0].metadata.name}")
      kubectl --namespace monitoring port-forward $POD_NAME 9093
    #################################################################################
    ######   WARNING: Pod Security Policy has been moved to a global property.  #####
    ######            use .Values.podSecurityPolicy.enabled with pod-based      #####
    ######            annotations                                               #####
    ######            (e.g. .Values.nodeExporter.podSecurityPolicy.annotations) #####
    #################################################################################


    The Prometheus PushGateway can be accessed via port 9091 on the following DNS name from within your cluster:
    prometheus-pushgateway.monitoring.svc.cluster.local


    Get the PushGateway URL by running these commands in the same shell:
      export POD_NAME=$(kubectl get pods --namespace monitoring -l "app=prometheus,component=pushgateway" -o jsonpath="{.items[0].metadata.name}")
      kubectl --namespace monitoring port-forward $POD_NAME 9091

    For more information on running Prometheus, visit:
    https://prometheus.io/

# Installing grafana

    [bat,exa,fd,procs,ytop,rg] $ helm install grafana grafana/grafana --namespace monitoring
    NAME: grafana
    LAST DEPLOYED: Wed Dec  9 16:09:02 2020
    NAMESPACE: monitoring
    STATUS: deployed
    REVISION: 1
    NOTES:
    1. Get your 'admin' user password by running:

       kubectl get secret --namespace monitoring grafana -o jsonpath="{.data.admin-password}" | base64 --decode ; echo

    2. The Grafana server can be accessed via port 80 on the following DNS name from within your cluster:

       grafana.monitoring.svc.cluster.local

       Get the Grafana URL to visit by running these commands in the same shell:

         export POD_NAME=$(kubectl get pods --namespace monitoring -l "app.kubernetes.io/name=grafana,app.kubernetes.io/instance=grafana" -o jsonpath="{.items[0].metadata.name}")
         kubectl --namespace monitoring port-forward $POD_NAME 3000

    3. Login with the password from step 1 and the username: admin
    #################################################################################
    ######   WARNING: Persistence is disabled!!! You will lose your data when   #####
    ######            the Grafana pod is terminated.                            #####
    #################################################################################
    chris@precision:~

# Viewing

View grafana

    kubectl port-forward prometheus-deployment-5ccfc968dc-bk484 3000

..


# Local push to prod

Switch to a pristine copy of inflex, e.g. ../inflex-copy and git pull:

```
cd ../inflex-copy
git pull
```

Login:

```
docker logout registry.gitlab.com
docker login registry.gitlab.com
chrisdone-skyabove
```

Update patch if needed:

    docker image build . -f docker/sky-above/inflex/patch.Dockerfile -t registry.gitlab.com/sky-above/inflex/patch:2021-10-03

Copy the patch name to prod.Dockerfile.

Build prod with a commi name:

    docker image build . -f docker/sky-above/inflex/prod.Dockerfile -t registry.gitlab.com/sky-above/inflex/prod:$(git rev-parse --verify HEAD)

Switch back to `inflex/` real repo.

Update the `inflex-server.yaml` file with the output of

    git rev-parse --verify HEAD

Copy it to the prod:

    scp kube/* do-inflex-prod:kube/

Apply it:

    ssh do-inflex-prod ./kubectl apply -f kube

Observe success:

    ssh do-inflex-prod ./kubectl --namespace ingress-nginx get pods -w

# Emacs

``` lisp
(prodigy-define-service
  :name "inflex-kube-forward"
  :tags '(ssh-tunnel)
  :tunnel (list
           :localport  "38161"
           :tunnel-ip  "127.0.0.1"
           :tunnel-port  "38161"
           :host  "do-inflex-prod"))

(prodigy-define-service
  :name "inflex-pg-forward"
  :tags '(ssh-tunnel)
  :tunnel (list
           :localport  "54322"
           :tunnel-ip  "private-inflex-prod-do-user-7291156-0.b.db.ondigitalocean.com"
           :tunnel-port  "25060"
           :host  "do-inflex-prod"))
```
