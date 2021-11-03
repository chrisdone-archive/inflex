#!/bin/bash

scp kube/* do-inflex-prod:kube/
ssh do-inflex-prod ./kubectl apply -f kube

echo Now you can check. Hit C-c C-c to finish.
ssh do-inflex-prod ./kubectl --namespace ingress-nginx get pods -w
