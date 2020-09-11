Get registry-read-write from lastpass

[bat,exa,fd,procs,ytop,rg] $ docker login -u chrisdone-skyabove registry.gitlab.com
Password:
Login Succeeded
chris@precision:~/Work/skyabove/inflex/fly
[bat,exa,fd,procs,ytop,rg] $ docker push registry.gitlab.com/sky-above/inflex/prod


kubectl delete secret inflex-server-config --namespace ingress-basic
kubectl create secret generic inflex-server-config --from-file=$HOME/Work/skyabove/inflex/live/config.yml --namespace ingress-basic

kubectl apply -f kube/inflex-server.yaml --namespace ingress-basic
