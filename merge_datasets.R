# MGH.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/MGH/"
# UCLA.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/UCLA/"
# RUSH.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Rush/"
# EMORY.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Emory/"
# out.dir<-'/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Cross site data/'
#returns a boolean - does a unique file exist in the given directory
#returns true if exactly one file with the given prefix exists
#returns false for any other default
library(data.table)
library(dplyr)
library(tidyr)

scoring.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/VR12score/"
source(scoring.dir %&% "R ScoringVR12score_vrData.R")
"%|%" = function(a,b) paste(a,b,sep="|")
#
uniqueFileExists<-function(lookup) {
  dir<-lookup[1]
  prefix<-lookup[2]
  nMatches<-sum(grepl(prefix,list.files(dir)))
  isUnique<-nMatches == 1
  return(isUnique)
}


#this group of functions handles reading in all the files and merging them at once
#eg it reads in all the assessment files across directories and then merges them, then the same for patients, visits etc
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

#this function handles VR12 scoring
#it relies on the R ScoringVR12score_vrData.R written by Scott J Hetzel MS at U Wisconsin Madison - future work should adapt/sunset this script as it is quite old
#it scores the data, then appends it back to the original dataset under the assessment types MCS and PCS
#happens in this stage because MCS and PCS are needed downstream for all processes
scoreVR12<-function(dir,assessments){

  prescore<-assessments %>%
    filter(grepl("VARAND",ASSESSMENT_TYPE,ignore.case=T)) %>%
    select(-one_of("ASSESSMENT_RESPONSE_VALUE" %&% 21:22)) %>%
    mutate(Survey="Mail",
           PAT_ID = ASSESSMENT_ID %|% 
                    PATIENT_ID_NUM %|% 
                    FACILITY_NAME %|% 
                    SERVICE_LINE %|%
                    ASSESSMENT_TYPE %|%
                    ASSESSMENT_TERM %|%
                    ASSESSMENT_DATE %|%
                    COLLECTION_TYPE) %>%
    select(PAT_ID,starts_with("ASSESSMENT_RESPONSE_VALUE"),Survey)
    
  cleanNames<-c("mrn","vr1","vr2a","vr2b","vr3a","vr3b","vr4a","vr4b","vr5","vr6a","vr6b","vr6c","vr7","vr8","vr9","vrplus1","vrplus1a","vrplus1b","vrplus2","vrplus3","vrplus4","Survey")
  colnames(prescore)<-cleanNames
  
  fwrite(prescore,dir %&% "prescoreVRData/Prescore_VR12.csv")
  vr12score(file.in=dir %&% "prescoreVRData/Prescore_VR12.csv",
            file.out=dir %&% "scoredVRData/Mail_scored_VR12_out.csv",
            keyfilepath=scoring.dir)
  
  VR12ScoredData<-fread(dir %&% "scoredVRData/Mail_scored_VR12_out.csv") 
  sanitized_VARAND<-VR12ScoredData %>%
    select(mrn,MCS,PCS) %>%
    pivot_longer(cols = c(MCS,PCS)) %>%
    separate(mrn,into=c('ASSESSMENT_ID',"PATIENT_ID_NUM",'FACILITY_NAME','SERVICE_LINE','ASSESSMENT_TYPE',"ASSESSMENT_TERM",'ASSESSMENT_DATE','COLLECTION_TYPE'),sep="\\|") %>%
    mutate(ASSESSMENT_TYPE = name) %>%
    rename(ASSESSMENT_RESPONSE_VALUE1='value') %>%
    select(-name)
  toAppend<-data.frame(matrix(NA,nrow=nrow(sanitized_VARAND),ncol=21))
  colnames(toAppend)<-c("ASSESSMENT_RESPONSE_VALUE" %&% 2:22)
  sanitized_VARAND<-cbind.data.frame(sanitized_VARAND,toAppend) 
  
  assessments<-assessments %>%
    select(-V31) %>%
    rbind.data.frame(sanitized_VARAND)
  return(assessments)
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
  #add the MCS and PCS to the assessments
  tmp[[1]]<-scoreVR12(out.dir,tmp[[1]])
  names(tmp)<-gsub('^','',prefixesVec,fixed=T)
  for (i in 1:length(tmp)){
    targetOut<-out.dir %&% names(tmp)[i] %&% "_" %&% Sys.Date() %&% ".csv"
    dups<-duplicated(tmp[[i]])
    sanitizedOut<-tmp[[i]][!dups,]
    fwrite(sanitizedOut,targetOut)
  }
  return(tmp)
}
