#! /bin/bash

#change this to your username
username=mboyer16

D=$(dirname "$0")
cd "$D"
E=$(dirname $(dirname $(dirname "$D")))
cd "$E"

read -p "(recommended, if exists) Remove old copy on Romulus? (Y/N): " response
if [ "$response" = 'Y' ]; 
	then 
		read -p "  Remove everything? (Y/N) (alternative is just DVR package): " response
		if [ "$response" = 'Y' ]; 
			then 
			ssh -t -Y "$username@romulus.amherst.edu" <<sshHERE
echo "    Removing everything"; 
rm -rf '~/DVR APP'; 
echo "$(ls)"
sshHERE
		else
			ssh -t -Y "$username@romulus.amherst.edu" <<sshHERE
echo "    Removing DVR"; 
cd '~/DVR APP/contents'; 
rm -rf DiscreteVariableRepresentation;
echo "$(ls)"
sshHERE
		fi

fi

read -p "Upload a new copy onto Romulus? (Y/N): " response
if [ "$response" = 'Y' ]; 
	then 
		read -p "  Upload everything? (Y/N): " response
		if [ "$response" = "Y" ];
			then
				echo "    Uploading copy"
				ssh -t "$username@romulus.amherst.edu" "mkdir -p '~/DVR APP'"
				scp -r "ObjectOrientedProgramming" "$username@romulus.amherst.edu:~/Mathematica/ObjectOrientedProgramming"
				scp -r "MolecularModeling" "$username@romulus.amherst.edu:~/Mathematica/MolecularModeling"
				ssh -t "$username@romulus.amherst.edu" "mkdir -p ~/Mathematica/DiscreteVariableRepresentation/DVR"
				base="DiscreteVariableRepresentation/DVR"
				scp -r "$base/code" "$base/Potential Files" "$base/DVR Files" "$username@romulus.amherst.edu:~/Mathematica/$base/"
				base="DiscreteVariableRepresentation"
				scp "$base/test.nb" "$base/init.m" "$username@romulus.amherst.edu:~/Mathematica/$base/"

			else
				echo "    Uploading DVR code"
				
				ssh -t "$username@romulus.amherst.edu" "mkdir -p ~/Mathematica/DiscreteVariableRepresentation"
				scp -r "DiscreteVariableRepresentation/DVR/code" "$username@romulus.amherst.edu:~/Mathematica/DiscreteVariableRepresentation/DVR/"
				scp -r "DiscreteVariableRepresentation/DVR/Potential Files" "$username@romulus.amherst.edu:~/Mathematica/DiscreteVariableRepresentation/DVR/"
				scp -r "DiscreteVariableRepresentation/DVR/DVR Files" "$username@romulus.amherst.edu:~/Mathematica/DiscreteVariableRepresentation/DVR/"
				
				scp -r "DiscreteVariableRepresentation/test.nb" "$username@romulus.amherst.edu:~/Mathematica/DiscreteVariableRepresentation/test.nb"
			 fi
		source "$D/Open in Romulus.command"
fi
