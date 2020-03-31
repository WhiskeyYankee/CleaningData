library(tidyverse)
library(httr)

#set working directory
{
  MD <- "C:/Users/Whiskey/Desktop/Data_Science/Data-Cleaning/Week4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset"
  testFolder <- paste(MD,"test",sep = "/")
  testSignals <- paste(testFolder,"Inertial Signals",sep = "/")
  trainFolder <- paste(MD,"train",sep = "/")
  trainSignals <- paste(trainFolder,"Inertial Signals",sep = "/")
}

# Read in features 
{
  setwd(MD)
  feature <- readLines("features.txt")
}

# Import Training Data
{
  # 1. Set working directory to the training folder
  setwd(trainFolder)
  # 2. Import the Subject training data
  subject_train <- data.frame(subject =readLines("subject_train.txt"))
  # 3. Import the training labels
  labels_train <- data.frame(activity = readLines("y_train.txt"))
  # 4. Import the training data as a fixed width file with 16 character widths
  set_train <- read_fwf("X_train.txt",fwf_widths(rep(16,561),feature))
  # 5. Create the training data set by column binding the subjects, labels, and data
  # And create a source column to define the data set of origin
  Data.Train <- cbind(subject_train,labels_train,set_train)%>%
    mutate(source = "Train")
  # 6. Remove  the data that is no longer needed
  rm(list = c("subject_train","set_train","labels_train"))  
}

# Import Testing Data
{
  # Repeat steps 1 - 6 from the Import Training Data but using the test data 
  setwd(testFolder)
  subject_test <- data.frame(subject =readLines("subject_test.txt"))
  labels_test <- data.frame(activity = readLines("y_test.txt"))
  set_test <- read_fwf("X_test.txt",fwf_widths(rep(16,561),feature))
  Data.Test <- cbind(subject_test,labels_test,set_test)%>%
    mutate(source = "Test")
  rm(list = c("subject_test","set_test","labels_test"))  
}

# Merge Training and Testing Data And remove all other data elements
Data <- rbind(Data.Test,Data.Train)
rm(list = ls()[-(ls()== "Data")])

# Create variable called org_names to indicate the original names of the data set
org_names <- names(Data)

# Select mean and standard deviation measurements using the select and grep functions
#    and mutate the data set to set the activity to a description instead of a number. 
#    This could be accomplished by importing the text file and using a left join instead
#    of hard coding the values in a case statement. Also adding ID field to be used later.
{
  Data <- Data %>%
    select(subject,activity,source
           ,org_names[grepl("mean()",org_names)& !grepl("Freq()",org_names)],
           org_names[grepl("std()",org_names)])%>%
    mutate(activity = case_when(
      activity == 1 ~ "walking",
      activity == 2 ~ "walking_upstairs",
      activity == 3 ~ "walking_downstairs",
      activity == 4 ~ "sitting",
      activity == 5 ~ "standing",
      activity == 6 ~ "laying"
    ), ID = row_number())
}

# Remove the Number, space, and paren from  each variable
# and replace the - with _, Validate the new names are unique
# and rename the Data variables 
{
  new_names <- str_remove(names(Data), "^[0-9]+[ ]")
  new_names <- str_remove(new_names, "[(][)]")
  new_names <- str_replace_all(new_names,"[-]","_")
  length(new_names) == n_distinct(new_names)
  names(Data) <- new_names
}

#  Calulate the mean of each activity variable
{
  Data.Summary <- Data %>%
    select(-c(source,ID))%>%
    group_by(subject,activity) %>%
    summarise_each(mean
    )
}

# write tables 
setwd("C:/Users/Whiskey/Desktop/Data_Science/Data-Cleaning/Week4/devel")
write.csv(Data,"Data.csv",row.names = F)
write.csv(Data.Summary,"DataSummary.csv",row.names = F)
