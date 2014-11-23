# Working directory is set in the download location of the files 

data_train <- read.table("train/subject_train.txt", col.names="SubjID")

data_test <- read.table("test/subject_test.txt", col.names="SubjID")

# These two datasets are merged

subj_id <- rbind(data_train, data_test)

# Load files with training subset and test subset of Labels 

acti_train <- read.table("train/y_train.txt", col.names="Acti")

acti_test <- read.table("test/y_test.txt", col.names="Acti")

# merge row-wise

# Call the new single-column data frame `acti`
acti <- rbind(acti_train, acti_test)

# form data frame with descriptive names for activities

# load the file with the activity coding

acti_coding <- read.table("activity_labels.txt", col.names=c("Code", "Desc"))


# Converting Desc column into character

acti_coding$Desc <- as.character(acti_coding$Desc)

# create `desc_acti` single-column, character data frame with activity labels

desc_acti <- data.frame(DescActi = as.character(acti_coding$Desc[acti$Acti]))

desc_acti$DescActi <- as.character(desc_acti$DescActi)

# Now bind the numeric and character activity data frames column-wise

act_label <- cbind(acti, desc_acti)


# compare the first and second columns of act_label via the coding

# merge the train/test feature data sets into a single feature data set

feat_train <- read.table("train/X_train.txt")

feat_test <- read.table("test/X_test.txt")

# merge them row-wise, with test appended behind train as with the others

features <- rbind(feat_train, feat_test)

# build vector whose entries are the column indices of the features which we want to retain, out of the initial list

#use the 'features.txt' file, where all feature names are listed.

feat_map <- read.table("features.txt")

feat_frame <- data.frame(FeatIndx=feat_map[,1], FeatName=feat_map[,2])

feat_frame$FeatIndx <- as.integer(feat_frame$FeatIndx)

feat_frame$FeatName <- as.character(feat_frame$FeatName)

means <- feat_frame[grep("-mean", feat_frame$FeatName),]

stdvs <- feat_frame[grep("-std", feat_frame$FeatName),]

# build desired feature columns with the corresponding names

# Initialise the output data set with the subject IDs and the

# activity labels, in numeric as well as character forms.

out_feat <- cbind(subj_id, act_label)

# Now iterate over the needed column indices in features and, for each,
# extract the column, give it the proper feature name, and bind it to
# the `out_feat` data frame column-wise.

for (i in 1:nrow(means)) {
  vect <- data.frame(features[,means$FeatIndx[i]])
  colnames(vect) <- means$FeatName[i]
  out_feat <- cbind(out_feat, vect)
}

# Then those with std devs in them

for (i in 1:nrow(stdvs)) {
  vect <- data.frame(features[,stdvs$FeatIndx[i]])
  colnames(vect) <- stdvs$FeatName[i]
  out_feat <- cbind(out_feat, vect)
}


# - 79 features that have means or stdev in the next columns
# and with 10,299 rows:
# - original training entries in the first 7,352 rows
# - original test entries in the following 2,947 rows

# Next and last calculate averages of each remaining feature and 
# save the output in a new data set.

out_avgs <- as.data.frame(aggregate(out_feat[,4:82], list(Subject=out_feat$subjID, Activity=out_feat$DescActi), mean))


write.table(out_avgs, "./Req_files.txt")
write.table(out_avgs, "./Req_files.csv")