#!/bin/bash

eval $(ssh-agent s)
echo "$SSH_PRIVATE_KEY" > keyfile.txt
chmod 700 keyfile.txt
echo "$SSH_KEY_PASS" | ssh-add keyfile.txt
mkdir p ~/.ssh
chmod 700 ~/.ssh
ssh-keyscan 46.101.49.42 >> ~/.ssh/known_hosts
chmod 644 ~/.ssh/known_hosts
