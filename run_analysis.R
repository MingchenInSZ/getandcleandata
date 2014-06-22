
#In the function, the number labeled description is the main step
#to solve the question in the project instruction

run_analysis<-function()
{
  curdir<-getwd();#get the current directory
  
  # Then set all the file necessary
  
  trainfile<-paste(curdir,"/UCI HAR Dataset/train/X_train.txt",sep="");
  trainlabel<-paste(curdir,"/UCI HAR Dataset/train/y_train.txt",sep="");
  trainsubject<-paste(curdir,"/UCI HAR Dataset/train/subject_train.txt",sep="")
  testfile<-paste(curdir,"/UCI HAR Dataset/test/X_test.txt",sep="");
  testlabel<-paste(curdir,"/UCI HAR Dataset/test/y_test.txt",sep="");
  testsubject<-paste(curdir,"/UCI HAR Dataset/test/subject_test.txt",sep="")
  featurefile<-paste(curdir,"/UCI HAR Dataset/features.txt",sep="")
  ac_file<-paste(curdir,"/UCI HAR Dataset/activity_labels.txt",sep="")
  
  #read all the files including data, label and subjects and all the features
  
  traindata<-scan(file=trainfile,what=numeric(0));
  trainlabel<-read.table(trainlabel,col.names=c("ac_label"));
  subtrain<-scan(file=trainsubject,what=list(subject=0))
  testdata<-scan(testfile,what=numeric(0));
  testlabel<-read.table(testlabel,col.names=c("ac_label"));
  subtest<-scan(file=testsubject,what=list(subject=0))
  features<-scan(file=featurefile,what=list(index=0,feature=""))
  aclabels<-scan(file=ac_file,what=list(ind=0,label=""))
  
  # all the features
  print("reading done")
  feature_vector<-append(features$feature,c("ac_label","subject","label"))
  
  #calculate the rows of the data
  
  trainrows<-length(traindata)/561
  testrows<-length(testdata)/561
  
  #convert all the data (train and test) into matrix format
  
  train<-matrix(traindata,nrow=trainrows,ncol=561,byrow=T)
  test<-matrix(testdata,nrow=testrows,ncol=561,byrow=T)
  
  # bind the activity label and subject to the data
  
  train<-cbind(train,trainlabel$ac_label)
  train<-cbind(train,subtrain$subject)
  train<-cbind(train,rep(1,trainrows)) # 1---trian
  test<-cbind(test,testlabel$ac_label)
  test<-cbind(test,subtest$subject)
  test<-cbind(test,rep(0,testrows))# 0---test
  
  
  
  # merge train and test
  
  data<- rbind(train,test)
  
  #add label to the data
  ##-----Merges the training and the test sets to create one data set.(1)
  
  data<-as.data.frame(data)
  
  ##Appropriately labels the data set with descriptive variable names. (4)
  
  names(data)<-feature_vector
  
  #replace all the activity index with its labels read from aclebals
  ## -----Uses descriptive activity names to name the activities in the data set.(3)
  acl<-c()
  for(v in data$ac_label)
  {
    acl<-append(acl,aclabels$label[v])
    
  }
  data$ac_label<-acl
  
  # extract all the measures on mean and std
  
  ext<-c()# this is the all the extracted variables
  inds<-c()# this is the index of the variables in rs
  st<-1
  for(f in features$feature)
  {
    fs<-strsplit(f,"-")[[1]]
    if(substr(fs[2],1,5)=="mean(")
    {
      ext<-append(ext,f)
      inds<-append(inds,st)
    }
    if(substr(fs[2],1,4)=="std(")
    {
      ext<-append(ext,f)
      inds<-append(inds,st)
      
    }
    st<-st+1
    if(st==544)# the last few variables have no character '-'
    {
      break
    }
  }
  
  #add the ac_label,subject and label to the ext
  
  ext<-append(ext,c("ac_label","subject","label"))
  
  # This is the extracted all the measures on mean and std
  ## -----Extracts only the measurements on the mean and standard deviation for each measurement.(2)
  
  ext_data<-subset(data,select=ext)
  
  # Using aggregate function to get the independent dataset
  ##Creates a second, independent tidy data set with the average of each variable for each activity and each subject.(5)
  ind_data<-aggregate(.~ac_label+subject,data=data[,-1],mean)
  
  result<-list(total_data=data,extracted_data=ext_data,independent_data=ind_data)
  
  
  result
  
}