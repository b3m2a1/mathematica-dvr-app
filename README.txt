This mini-application was written to create an easier way to run discrete variable 
representation (DVR) calculations.

To load the application, simply open the nameless notebook above and click the load 
button. An icon for that notebook can be found in the contents folder, should you wish to 
use it.

All data can be found at contents/DiscreteVariableRepresentation/DVR/Resources. A Mac 
alias is provided to make that easier to get at.

This application runs by loading DVR configuration files from the Resources/DVR Files 
directory, and so any new configuration files should be saved there.

Data is by default stored in an SQL database built with sqlite, which can be found in the 
Resources directory. File based storage can also be used, in which case the program will
make two folders, "Kinetic Matrices" and "Calculated Wavefunctions" where data can be 
found. This is less full featured that using SQL storage, but is still functional.

When using this application, various interfaces are generated. The base forms for each of 
these interfaces can be found in Resources/Interfaces

One of the generated interfaces is a DVR session interface and these should be saved in 
the Resources/Saved DVRs folder. It will then store its interface instance in 
Saved DVRs/Interface Instances.

When configuring background runs, run scripts are stored in Resources/Run Scripts.

There are 5 core packages that make up this application, which are found in the contents 
folder. These are ObjectOrientedProgramming, DiscreteVariableRepresentation,
DatabaseWrapper, MolecularModeling, and ParallelProcess. Documentation and examples for 
each of these can be found in their Documentation/docs.nb files. A few Stylesheets 

Stylesheets for the interfaces and documentation can be found in contents/Stylesheets

A potentially more up-to-date version of this app can be found at on github at
github.com/b3m2a1/mathematica-dvr-app and other Mathematica code can be found at 
github.com/b3m2a1/mathematica-general.