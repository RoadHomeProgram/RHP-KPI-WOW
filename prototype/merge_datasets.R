# MGH.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/MGH/"
# UCLA.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/UCLA/"
# RUSH.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Rush/"
# EMORY.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Emory/"
# out.dir<-'/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Cross site data/'
#returns a boolean - does a unique file exist in the given directory
#returns true if exactly one file with the given prefix exists
#returns false for any other default
uniqueFileExists<-function(lookup) {
  dir<-lookup[1]
  prefix<-lookup[2]
  nMatches<-sum(grepl(prefix,list.files(dir)))
  isUnique<-nMatches == 1
  return(isUnique)
}


#this group of functions handles reading in all the files and merging them at once
harmonizeData<-function(df){
  if (!is.null(df$PATIENT_ID_NUM)) {
    df<-mutate(df,PATIENT_ID_NUM = as.character(PATIENT_ID_NUM))
  }
  if (!is.null(df$SERVICE_PERFORMED)) {
    df<-mutate(df,SERVICE_PERFORMED = as.character(SERVICE_PERFORMED))
  }
  if (!is.null(df$REFERRAL_ID_NUM)) {
    df<-mutate(df,REFERRAL_ID_NUM = as.character(REFERRAL_ID_NUM))
  }
  return(df)
}

handleOneReadin<-function(dir,prefix) {
  print(dir %&% prefix)
  targetFile<-list.files(dir)[grepl(prefix,list.files(dir))]
  readinData<-fread(dir %&% targetFile,na=c("99","999"))
  readinData<-harmonizeData(readinData)
  return(readinData)
}

readin<-function(prefix,directoriesVec){
  dataList<-lapply(directoriesVec,handleOneReadin,prefix)
  completeDataset<-bind_rows(dataList)
}


###
mergeWCNData<-function(MGH.dir,UCLA.dir,RUSH.dir,EMORY.dir,out.dir,ignore.missing=F) {
  #check if there exists exactly one of each file in each directory. 
  #If false exit
  directoriesVec<-c(MGH.dir,UCLA.dir,RUSH.dir,EMORY.dir)
  prefixesVec<-c("^assessment", "^patient", "^visit", "^referral", "^satisfaction")
  lookupFiles<-expand.grid(directoriesVec,prefixesVec)
  allExists<-sum((apply(lookupFiles,1,uniqueFileExists))) == nrow(lookupFiles)
  if (!ignore.missing & !allExists) {
    print("error: cannot determine unique fileset for each site")
    print("Problematic items are as follows:")
    print(lookupFiles[!apply(lookupFiles,1,uniqueFileExists),])
    break
  }
  #next we want to just stack the matching files together
  tmp<-lapply(prefixesVec,readin,directoriesVec)
  names(tmp)<-gsub('^','',prefixesVec,fixed=T)
  for (i in 1:length(tmp)){
    targetOut<-out.dir %&% names(tmp)[i] %&% "_" %&% Sys.Date() %&% ".csv"
    dups<-duplicated(tmp[[i]])
    sanitizedOut<-tmp[[i]][!dups,]
    fwrite(sanitizedOut,targetOut)
  }
  return(tmp)
}
