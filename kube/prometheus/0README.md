# Deploy Prometheus on Kubernetes to get metrics from a single pod of interest

Deploying Prometheus on Kubernetes is not hard to do. But if you google it, you'll see no useful documentation on how to do it. You'll see [a spartan list of configuration options](https://prometheus.io/docs/prometheus/latest/configuration/configuration/#kubernetes_sd_config) with no guide on doing something trivial, and a bunch of heavy projects (like [prometheus-operator](https://github.com/prometheus-operator/kube-prometheus)) that generate a steaming pile of YAML for you, which is not good for understanding when things go wrong.

## Steps

Once you've set this up, you can setup Grafana separately and have it consult this Prometheus, or not. Don't forget that Prometheus has its own web interface to view the stats in a simple way. So you don't have to worry about setting up Grafana until you really want Grafana.

Create roles so that prometheus can find your pods dynamically

    kubectl create -f promrbac.yml 

(When updating config, these are handy delete commands. kubectl doesn't have a "recreate" for most things because that would be too convenient.)

    kubectl delete -f promdep.yml ; kubectl delete configmap prometheus-config
 
First time/update:

    kubectl create configmap prometheus-config --from-file kubeprom.yml; kubectl create -f promdep.yml
    
View web service:

    kubectl port-forward prometheus-deployment-<extra hash suffix here> 9090 # needs manual tweak

Use `kubectl get pods` to see the `prometheus-deployment` pod name.

Navigate to http://localhost:9090/ to view the prometheus web interface. You should see listed:

1. Prometheus itself.
2. Your pod listed.

Under Status.