# Code and Sample Data

Notes:
- Both Python and R are utilized.
- Given NDAs, we disguise the WOM variables. Specifically, we draw random numbers from a joint Normal distribution with mean = 0, variance-covariance = 1/2 * (var(Log(Post-WOM)), cov(log(Post-WOM), log(Pre-WOM)), cov(log(Post-WOM), log(Pre-WOM)), var(log(Pre-WOM)))
- The audience size data are not included in the data files due to our data usage agreement. This data can be purchased from Comscore (https://www.comscore.com/Products/Television).
- To avoid reposting public data, we disguise the voter preference variable by adding random number drawn from N(0, 1/5 * var(voter preference)). The original data can be downloaded from https://uasdata.usc.edu/index.php.
- For other publicly available datasets, we guide readers in the code to the website(s) where the data can be downloaded. 

[Build]
- Slant Measure.ipynb (implement the slant measure from Gentzkow and Shapiro (2010) to transcribed advertising data).
- Doc2Vec.ipynb (implement the Doc2Vec algorithm from Le and Mikolov (2014)).
- Poll_Data_Cleaning.R (describes the procedure for cleaning the data from USC Dornsife / LA Times.

[Data]
- variable_description.txt (provides descriptions on variables)
- WOM_DATA.csv (main data for the WOM analysis).
- VOTER_PREFERENCE_DATA.csv (main data for for the voter preference analysis).


[Code]
- Replication R Code.R (replicates the summary statistics and main results of the analyses)


Given an NDA with one of the data providers, some of the variables in this repository have been disguised or excluded, as noted above. As such, these files may not produce estimates identical to those in the manuscript.
