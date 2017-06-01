#! /bin/bash

#Command used as DVRServerRun file serverShare newFileName

user = "$2"
serverShare = "$3"

scriptFile = "$3"

scp $scriptFile "$serverShare/~/$newFileName"

ssh -t "$serverShare"<<SSHHEREBLOCK

~/$newFileName

SSHHEREBLOCK