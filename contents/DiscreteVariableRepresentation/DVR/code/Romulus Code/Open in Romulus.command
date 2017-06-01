#! /bin/bash

username="mboyer16"
#change this to your username
chd="cd Mathematica/DiscreteVariableRepresentation"
mma='Mathematica test.nb &'
int="/bin/bash"

read -p "Open Mathematica? (Y/N): " response
ssh -t -Y "$username@romulus.amherst.edu" "$chd; test $response = 'Y' && $mma; $int" 