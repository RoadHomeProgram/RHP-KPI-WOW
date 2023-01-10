#Data validation and cleaning
#the purpose of this script is to provide an entry point for harmonizing the data
#Although it is intended that the files follow the same formatting across AMCs
#the datasets provided by AMCs are not always identical and may have small differences that need to be identified and resolved (often manually)
#this script is intended to do basic validation on each file
#checking that names and data are formatted correctly, as well as removing extraneous columns
source("/Users/ryanschubert/Documents/RHP-KPI-WOW/helpers.R")
logDir<-"/Users/ryanschubert/Documents/RHP-KPI-WOW/logs/" #this should go in an environment variable

flagFile<-function(warning,log) {
  print('WARNING:')
  print(warning)
  out<-log %&% "Validation_" %&% timestamp
  fwrite(warning,out,append=T)
  next
}

#function removes any empty columns
stripColumns<-function(df) {
  df<-df[,!apply(df,2,isEmpty)]
  return(df)
}

#checks if all the correct columns exist and then subsets to those column
#if not exist, prints out a warning and moves on from this fileset
#naming is case sensitive, so captures lowercase 
checkNames=function(data,refNames) {
  #are all the reference columns in the dataset
  allExists<-sum(refNames %in% colnames(data)) == length(refNames)
  if (allExists) {
    return(data[,refNames])
  } else {
    flagFile(warning=list(WARNING='File ' %&% file %&% ' does not have the complete set of matching names. Check variable formatting.'),
             log=logDir)
  }
}

#check order
checkOrder=function() {
  
}

resolveNames=function(){
  
}


#check type
checkType=function(data,ref) {

}

resolveType=function(){
  
}



main<-function(file,reference) {
  data<-read.table(file,sep='|',header=T)
  ref<-read.csv(reference)
  data<-stripColumns(data)
  #check on the names
  namesSummary<-checkNames(data,ref$name)
  
}

tmp<-read.table('/Users/ryanschubert/Dropbox (Rush)/WCN Data/original/Rush/assessment_data_202211.txt',sep='|',header=T)
assessRef<-read.csv("/Users/ryanschubert/Dropbox (Rush)/Ryan's stuff/rush/remake KPI/V2_prototype/data_model_reference/assessments_model.csv")
main(file='/Users/ryanschubert/Dropbox (Rush)/WCN Data/original/Rush/assessment_202211.txt',
     reference="/Users/ryanschubert/Dropbox (Rush)/Ryan's stuff/rush/remake KPI/V2_prototype/data_model_reference/assessments_model.csv")

file<-'/Users/ryanschubert/Dropbox (Rush)/WCN Data/original/Rush/assessment_data_202211.txt'
reference<-"/Users/ryanschubert/Documents/RHP-KPI-WOW/data_model_reference/assessments_model.csv"
unlist(sapply(tmp,class))
