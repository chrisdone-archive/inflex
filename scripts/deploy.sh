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

sh scripts/apply.sh
