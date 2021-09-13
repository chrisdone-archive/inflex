#!/bin/bash

eval $(sshagent s)
echo "$SSH_PRIVATE_KEY" > keyfile.txt
chmod 700 keyfile.txt
echo "$SSH_KEY_PASS" | sshadd keyfile.txt
mkdir p ~/.ssh
chmod 700 ~/.ssh
sshkeyscan 46.101.49.42 >> ~/.ssh/known_hosts
chmod 644 ~/.ssh/known_hosts
