#! /bin/bash

baseuser=mboyer16
basedir=$( dirname "$0")
if [ "$1" = "" ]; then username="$baseuser"; else username="$1" ; fi
keyname="$username@romulus_rsa"
read -p "Make new key-file? (Y/N) " response
if [ "$response" = "Y" ]
	then
		ssh-add -d "$keyname"
		mkdir -p ~/.ssh
		cd ~/.ssh
		chmod go-rwx ~/.ssh
		ssh-keygen -t rsa -f "$keyname" -N ""
		ssh-add "$keyname"
		chmod go-rwx "$keyname"
		chmod go-rwx "$keyname.pub"
		cd "$basedir"
	fi

echo "Configuring server... (enter your password)"
cat ~/.ssh/"$keyname.pub" | ssh -o PubkeyAuthentication=no "$username@romulus.amherst.edu" "mkdir -p ~/.ssh && cat >>  ~/.ssh/authorized_keys; chmod 700 ~/.ssh; chmod 600 ~/.ssh/authorized_keys" 
ssh "$username@romulus.amherst.edu"