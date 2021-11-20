# Code and Sample Data

This repository is to share the sample code and data used in "The Role of Slant and Message Consistency in Political Advertising Effectiveness: Evidence from the 2016 Presidential Election" by Beth Fossen, Donggwan Kim, David Schweidel, Raphael Thomadsen. These files may be used for replication only.

Notes:
- Both Python and R are utilized.
- The WOM and audience size data are not included in the data files due to our data usage agreements. The WOM data can be purchased from Brandwatch (https://www.brandwatch.com/) and the audience size data from Comscore (https://www.comscore.com/Products/Television).
- To avoid reposting public data, the voter preference variable is not included in the data files. The voter preference data can be downloaded from USC Dornsife/Los Angeles Times Daybreak poll (https://uasdata.usc.edu/index.php).
- For other publicly available datasets, we guide readers in the code to the website(s) where the data can be downloaded. 
 
[Build]
- Slant Measure.ipynb (implement the slant index/measure from Gentzkow and Shapiro (2010)).
- Doc2Vec.ipynb (implement the Doc2Vec algorithm from Le and Mikolov (2014)).
- Poll_Data_Cleaning.R (describes the procedure for cleaning the data from USC Dornsife / LA Times).
- Voter_Pref_Construct.R (describes the procedure for deriving the audience size weighted independent variables used in the voter preference analysis).

[Data]
- variable_description.txt (provides descriptions on variables)
- WOM_DATA.csv (main data for the WOM analysis).
- VOTER_PREFERENCE_DATA.csv (main data for the voter preference analysis).

[Code]
- Replication R Code.R (replicates the summary statistics and main results of the original paper)

Given NDAs with the data providers, some of the variables in this repository have been excluded, as noted above. As such, these files may not produce estimates identical to those in the manuscript.
