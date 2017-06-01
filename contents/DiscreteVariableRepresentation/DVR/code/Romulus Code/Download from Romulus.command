#! /bin/bash

username="mboyer16"
D=$(dirname "$0")
cd "$D"
cd ..
datetime=$(date)
downloadTo=$(dirname "$D")/"Downloads"/"$datetime"
#change this to your username
read -p "Download everything? (Y/N): " response
if [ "$reponse" = "Y" ]
	then
		echo "Downloading everything"
		scp -r "$username@romulus.amherst.edu:~/Mathematica" "~/Downloads/Romulus Download $date "
		source "$D/Remove from Romulus.command" "$username" "everything"
	else
		mkdir -p "$downloadTo/Kinetic Matrices"
		mkdir -p "$downloadTo/Calculated Wavefunctions"
		scp -r "$username"@romulus.amherst.edu:~/Mathematica/DiscreteVariableRepresentation/DVR/"Kinetic\ Matrices" "$downloadTo"
		scp -r "$username"@romulus.amherst.edu:~/Mathematica/DiscreteVariableRepresentation/DVR/"Calculated\ Wavefunctions" "$downloadTo"
		source "$D/Remove from Romulus.command" "$username" "selection"
fi
open "$downloadTo"