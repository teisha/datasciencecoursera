run_analysis <- function() {

  #Read in the files that contain the lookup 
  #values for the activities and features
  activity.labels <- read.table ("activity_labels.txt", 
                                 sep = " ",
                                 row.names = 1,
                                 col.names = c("id","activity"))
  
  features <- read.table("features.txt", 
                         sep=" ", 
                         row.names=1,
                         col.names=c("id","feature"))
  
  #Read in the training data set
  train.x <- read.table ("train/X_train.txt", sep="", strip.white=TRUE)
  ## Set the column headings to match the features they measure
  colnames(train.x) <- features[,1]
  
  #Read in the data set that holds the data describing the
  #activity for each observation in the training set
  train.y <- read.table ("train/y_train.txt", sep=" ", strip.white=TRUE)
  
  #Read in the subject id for each training observation
  train.subjects <- read.table("train/subject_train.txt")
  
  #Read in the testing data set
  test.x <- read.table ("test/X_test.txt", sep="", strip.white=TRUE) 
  ## Set the column names to match the features being measured
  colnames(test.x) <- features[,1]
  
  #Read in the testing activity levels
  test.y <- read.table ("test/y_test.txt", sep=" ", strip.white=TRUE)
  
  #Read in the subject id for each test observation
  test.subjects <- read.table("test/subject_test.txt")
  
  
  #define function for identifying character patterns in strings
  #which will be used to filter the features for std() and mean()
  is.found <- function (x, word) {
    is.there <- FALSE
    res <- gregexpr(pattern = word, x, fixed=TRUE)
    if (res[[1]][1] != -1) {is.there <- TRUE}
    is.there
  }
  
  #use the function to find the required features
  features.filter <- sapply(features[,1], is.found, "mean()") | 
                     sapply(features[,1], is.found, "std()")
  
  
  #Add the id for the activity labels as a column in the data frame
  #to prepare for joining with the observations
  activity.labels <- cbind(activity.labels,row.names(activity.labels))
  colnames(activity.labels) <- c("activity","id")
  
  #Add the activity labels to the test and training observtions
  test.y <- merge(test.y,activity.labels, by.x="V1", by.y="id")
  train.y <- merge(train.y,activity.labels, by.x="V1", by.y="id")
  
  #Combine
  #  - subjects
  #  - fields containing "std()" and "mean()" measurements
  #  - activity label
  #  - an identifier to describe which set the data came from 
  # for both the test and training data sets
  test.data <- cbind(test.subjects, 
                     test.y$activity, 
                     rep("test", length(test.y$activity)), 
                     test.x[,features.filter] )
  train.data <-  cbind(train.subjects, 
                       train.y$activity, 
                       rep("train", length(train.y$activity)), 
                       train.x[,features.filter] )
  
  #Set the column names for the 2 added fields in each data frame
  #so the data frames can be combined
  colnames(test.data)[1]  <- "subjects"
  colnames(test.data)[2] <- "activity.label"
  colnames(test.data)[3] <- "data.source"
  colnames(train.data)[1]  <- "subjects"
  colnames(train.data)[2] <- "activity.label"
  colnames(train.data)[3] <- "data.source"
  
  #Combine test and training
  combined.data <- rbind(test.data, train.data)  

  #Create a second tidy data set that holds the
  #average of each field, 
  #grouped by subject and activity
  avg.data <- ddply(.data = combined.data, 
                    .(subjects, activity.label), 
                    function(df)mapply(mean,df[,4:69],SIMPLIFY=TRUE))
  
  #Clean the headers of the new dataset to remove
  #parentheses and dashes
  clean_names <- function (x) {
    x <- gsub("\\()","",gsub("-",".",x) )
  }
  colnames(avg.data) <- sapply(colnames(avg.data), clean_names)
  
  #write file for output to uplload
  write.csv(avg.data, "average_data.csv")
  
  #return data.frame
  return (avg.data)
  
}