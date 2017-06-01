#! /bin/bash

baseuser=mboyer16
if [ "$1" = "" ]; then username=$baseuser; else username="$1"; fi;

remove="$2"
read -p "Remove from server? (Y/N): " response
	if [ "$response" = "Y" ];
		then
			if [ "$remove" = "everything"];
			then
				ssh "$username@romulus.amherst.edu" <<sshHEREblock 

rm -rf "~/Mathematica"

exit
sshHEREblock
			else
				ssh "$username@romulus.amherst.edu" <<sshHEREblock 
				
cd "Mathematica/DiscreteVariableRepresentation/DVR"
rm -rf "Kinetic Matrices"
rm -rf "Calculated Wavefunctions"

echo "Cleared"

exit
sshHEREblock
			fi
		fi
	