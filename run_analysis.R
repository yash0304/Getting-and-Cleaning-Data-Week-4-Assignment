getwd()
library(readxl)
library(dplyr)

############################# Train Data ##############################

x_train<-read.table("./train/X_train.txt")
y_train<-read.table("./train/Y_train.txt")
sub_train<-read.table("./train/subject_train.txt")

############################ Test Data #############################

x_test<-read.table("./test/X_test.txt")
y_test<-read.table("./test/Y_test.txt")
sub_test<-read.table("./test/subject_test.txt")

#####################################################################

variable_names<-read.table("features.txt")
activity_labels<-read.table("activity_labels.txt")

########################## Merging the data #########################

x_total<-rbind(x_train,x_test)
y_total<-rbind(y_train,y_test)
sub_total<-rbind(sub_train,sub_test)

######################## Mean or Standard Deviation ###################

selected_var <- variable_names[grep("mean\\(\\)|std\\(\\)",variable_names[,2]),]
x_total <- x_total[,selected_var[,1]]

##############Uses descriptive activity names to name the activities in the data set #############
colnames(y_total) <- "activity"
y_total$activitylabel <- factor(y_total$activity, labels = as.character(activity_labels[,2]))
activitylabel <- y_total[,-1]

############### Appropriately labels the data set with descriptive variable names. ##########
colnames(x_total) <- variable_names[selected_var[,1],2]

##############From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject. ################
colnames(sub_total) <- "subject"
total <- cbind(x_total, activitylabel, sub_total)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
write.table(total_mean, file = "./tidydata.txt", row.names = FALSE, col.names = TRUE)
dir()
