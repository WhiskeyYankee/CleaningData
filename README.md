# Human Activity Data Cleaning
This Repo contains the R script to tidy the Human Activity data and the output for the Getting and Cleaning Data course.
The 'Data' csv has subject, activity, data source (test or training set) as well as an ID which is just the record number.
The other 67 variables correspond to values that were either directly measured or derived from measurements during the 
Human Activity research. I retained the variable names from the feature selection since they are in and of them selves 
rich in content. I removed excess symbols and replaced a few values to make them easier to reference. The 'DataSummary' CSV
groups the Data by activity and determines the mean (expected value) of each of the variable measurements. 

