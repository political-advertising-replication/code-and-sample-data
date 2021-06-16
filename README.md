# Replication
Repository for replication files 

These files may be used for replication only.

Notes:
- Both Python and R are utilized.
- Given NDAs, we disguise the WOM variables. When disguising a variable, we draw random numbers from a joint Normal distribution with mean = 0, variance-covariance = 1/2 * (var(Post-WOM), cov(var(Post-WOM), var(Pre-WOM)), cov(var(Post-WOM), var(Pre-WOM)), var(Pre-WOM))
- To avoid reposting public data, we disguise the voter preference variable, collected from USC Dornsife/Los Angeles Times Poll, by adding random number drawn from N(0, var(voter preference)/5). For other publicly available datasets, we guide readers in the code to the website(s) where the data can be downloaded. 

[Build]
- Slant Measure.ipynb (implement the slant measure from Gentzkow and Shapiro (2010) to transcribed advertising data).
- Doc2Vec.ipynb (implement the Doc2Vec algorithm from Le and Mikolov (2014)).

[Data]
- variable_description.txt (provides descriptions on variables)
- WOM_DATA.csv (main data for the WOM analysis).
- VOTER_PREFERENCE_DATA.csv (main data for for the voter preference analysis).

[Code]
- Replication R Code.R (replicates the summary statistics and main results of the analyses)
