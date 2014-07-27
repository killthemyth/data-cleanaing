# Source: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Setting working directory
# Please Note: Here 'UCI HAR Dataset' named changed to 'dataset' for simplicity
setwd("~/Downloads/assign/r/ch/gc/UCI HAR Dataset/") 

# Activity 1
# Merging training and testing dataset


# Extract feature_names in variable feat_name

feat_names = read.table('features.txt')
head(feat_names)

# Reading testing file
test_x = read.table('test/X_test.txt') # X (from test)
head(test_x)
dim(test_x)

test_y = scan('test/y_test.txt') # Y (from test)
head(test_y)
length(test_y)

test_subjects = scan('test/subject_test.txt') # Subject (From test)
head(test_subjects)
length(test_subjects)

# Checking the uniformity of dimension 
if (length(test_y) != length(test_subjects) || length(test_y) != dim(test_x)[1]) {
  stop('testing dimension data set doesn\'t matched')
}

# Reading training files
train_x = read.table('train/X_train.txt') # X (From train)
head(train_x)
dim(train_x)

train_y = scan('train/y_train.txt') # Y (from train)
head(train_y)
length(train_y)

train_subjects = scan('train/subject_train.txt') # Subject (From train)
head(train_subjects)
length(train_subjects)

# Checking the uniformity of dimension 
if (length(train_y) != length(train_subjects) || length(train_y) != dim(train_x)[1]) {
  stop('training dimension data set doesn\'t matched')
}

# Naming
names(train_x) = feat_names$V2
names(test_x) = feat_names$V2

# Merge predictors and subject variabes

train_x$subject = train_subjects
test_x$subject = test_subjects

# Merge predictors and outcome variable

train_x$activity = train_y
test_x$activity = test_y

# Merge data sets

data_set = rbind(train_x, test_x)
head(data_set)
dim(data_set)


# Activity 2: Extracting the mean and standard deviation.

mean_and_std_columns = grepl( '(-mean\\(\\)|-std\\(\\))', feat_names$V2 )
mean_and_std_columns = append(mean_and_std_columns, TRUE) 
mean_and_std_columns = append(mean_and_std_columns, TRUE)

means_and_stds = data_set[, mean_and_std_columns]
names(means_and_stds)
dim(means_and_stds)


# Activity 3: Uses descriptive activity names to name the activities in the data set

activity_labels = read.table('activity_labels.txt')
means_and_stds$activity_label = factor(means_and_stds$activity, levels=c(1,2,3,4,5,6), 
                                       labels=activity_labels$V2)


# Activity 4, 5: Creating tidy.csv file
tidy.frame = data.frame()

subjects = sort( unique(means_and_stds$subject) )
activities = sort( unique(means_and_stds$activity) )

for (subj in subjects) {
  for (act in activities) {
    subset = means_and_stds[ means_and_stds$subject==subj & means_and_stds$activity == act, ]
    by_sub_act = as.data.frame( lapply( subset[,1:66], FUN=mean ) )
    by_sub_act$subject = subj
    by_sub_act$activity = act
    by_sub_act$activity_label = activity_labels[act,2]
    tidy.frame = rbind(tidy.frame, by_sub_act)
  }
}

write.table(tidy.frame, file="../tidy-data.csv" )
