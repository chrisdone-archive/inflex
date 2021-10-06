#!/bin/bash
set -e

cd ../inflex-copy
git pull

commit=$(git rev-parse --verify HEAD)

echo Building $commit ...

docker image build . -f docker/sky-above/inflex/prod.Dockerfile -t registry.gitlab.com/sky-above/inflex/prod:$commit

echo Pushing $commit ...

docker push registry.gitlab.com/sky-above/inflex/prod:$commit

echo Applying $comit ...

cd ../inflex
sed -E "s/\/prod:[0-9a-z]+$/\/prod:$commit/" kube/inflex-server.yaml -i
scp kube/* do-inflex-prod:kube/
ssh do-inflex-prod ./kubectl apply -f kube

echo Now you can check. Hit C-c C-c to finish.
ssh do-inflex-prod ./kubectl --namespace ingress-nginx get pods -w
