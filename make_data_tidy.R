suppressPackageStartupMessages(library(dplyr))

#Download the dataset if not done so yet
setwd('datasciencecousera/GCD_week4/')
if(!file.exists('GCD_final')){
  download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip',
                destfile = 'GCD_final', method = 'curl')
  unzip('GCD_final')
}

setwd('UCI HAR Dataset/')

# read in activity labels and the features
activity_labels <- read.table(file = 'activity_labels.txt', header = FALSE,stringsAsFactors = FALSE)
features <- read.table(file = 'features.txt', head = FALSE, stringsAsFactors = FALSE)

#read in all information from the test data
activity_test <- read.table(file = 'test/y_test.txt', header = FALSE)
results_test <- read.table(file = 'test/X_test.txt', header = FALSE)
subject_test <- read.table(file = 'test/subject_test.txt', header = FALSE)

#Switch numeric encoding for activity for the more descriptive version
for(i in 1:(nrow(activity_test))){
  lable <- activity_labels[match(x = activity_test[i,1],table = activity_labels[,1]),2]
  activity_test[i,1] <- lable
}

#create a data table with all information from test, add column type which indicates it came from test data 
test <- cbind('test',subject_test, activity_test, results_test)
colnames(test) <- c('type','subject','activity',features[,2])

#read in all information from the train data
activity_train <- read.table(file = 'train/y_train.txt', header = FALSE)
results_train <- read.table(file = 'train/X_train.txt', header = FALSE)
subject_train <- read.table(file = 'train/subject_train.txt', header = FALSE)

#Switch numeric encoding for activity for the more descriptive version
for(i in 1:(nrow(activity_train))){
  lable <- activity_labels[match(x = activity_train[i,1],table = activity_labels[,1]),2]
  activity_train[i,1] <- lable
}

#create a data table with all information from test, add column type which indicates it came from train data
train <- cbind('train',subject_train, activity_train, results_train)
colnames(train) <- c('type','subject','activity',features[,2])

#merge datasets
all_data <- rbind(test,train)


#extract only the mean and std columns convert subject and activity to factors
mean_std <-all_data[,c(1,2,3,grep(pattern = '(-mean..-|-std..-)',x = colnames(x = all_data)))]
mean_std[,2] = as.factor(mean_std[,2])
mean_std[,3] = as.factor(mean_std[,3])

#wrap with dplyr table
mean_std_df <- tbl_df(mean_std)

#remove type col, group by subject and activity, order by subject then activity, take a mean of each column
by_subject_activity <- mean_std %>%
  select(2:ncol(mean_std)) %>%
  group_by(subject, activity) %>% 
  arrange(subject,activity) %>%
  summarise_each(funs(mean)) 
  
write.table(by_subject_activity, file = 'Tidy_by_subject_activity_mean')  

  


