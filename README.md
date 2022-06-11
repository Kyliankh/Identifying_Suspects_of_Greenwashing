# Identifying_Suspects_of_Greenwashing
This repository contains all data and code which is used in my master thesis "Identifying suspects of greenwashing" for Business Analytics and Management at the Rotterdam School of Management, Erasmus University. It contains multiple data and code files, and both R and Python were used. Therefore, please read the README file first. 

The Code folder contains all the programming codes.
Some of the code is written in R and ran in RStudio.
Other code is written in Python 3.9 language and are ran in Jupyter Notebooks in Anaconda 3 platform. 

The required packages for the R codes are loaded at the start of the script. 
When this package is not used before, install it by the command: install.packages("Package name")

The same accounts for the python code. However, for perfect reproducability, a text file named: "requirements.txt" is added to the code folder. 
Run pip install -r requirements.txt in your shell, to run the code in the exact same environment is was done initially.

The chronological order in which the code is produced:
1. exploration.R
2. preproc_split.R
3. GB_2.ipynb
4. Regs.R

The input and output csvs for each codefile are added to the data folder.
It is therefore possible run just 1 file instead of having to run all of the code. 
