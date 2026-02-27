# Replication package: "Learning to Win by Fearing to Lose" by Antal Ertl, Éva Holb and Barna Bakó


This is a replication package for a Field Experiment on loss-framed grading ran at Corvinus University of Budapest in 2023. This ReadMe file helps to reproduce our research article -- any feedback is welcome.

The repository consists of three main parts: the datasets, the codes used for the analyses (to reproduce tables as well as figures), and the plots contains the 4 figures shown in the article.



In the data folder, there are two databases:
1) data_anon -- the analysis data anonimized (we removed the unique identifier of students)
2) data_clean -- data with all newly created variables, used for the regression tables
3) followup_merged_anon -- we merged data_anon with our followup data based on the unique identifier, which we then removed from the data.


In the codes folder, the replication analyses can be found:
In the R codes, data cleaning, figures, descriptive statistics and followup analysis can be found (as well as the code for comparing full sample to our sample, i.e, comparing n = 461 with n = 321)

Finally, in the STATA do file, we include all regression tables in the final paper, with each regression table labeled in a comment above.


