#this is a nicer version of the kpi script 
#instead of having them all in one function, lets separate each question out into separate functionalities

library(data.table)
library(naniar)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

# string concat function
scoring.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/VR12score/"
source(scoring.dir %&% "R ScoringVR12score_vrData.R")
source("/Users/ryanschubert/Documents/RHP-KPI-WOW/helpers.R")

#this function returns a dataset containing patients captured in the current fiscal year
inFiscalYear<-function() {
  
}

therapyList<-function() {

}

#this function returns a dataset with veterans who have at least one treatment according to the master list of therapies
withTreatment<-function(data) {
  vets_with_treatment<-data %>%
    filter(SERVICE_PERFORMED %in% master_list_services$TreatmentID) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>%
    distinct() %>%
    unlist() %>%
    unname()
  return(vets_with_treatment)
}

#returns the correct assessment end term
#1 for all variables except for vr12 items
assessmentTermEnd<-function(metric){
  if (metric == 'VARAND') {
    return(9001)
  } else {
    return(1)
  }
}

#this function processes the assessments calculate the percentage of individuals meeting some change sore threshold on a given metric
calcOutcomes<-function(data,metric,threshold){
  processedData<-data %>%
    filter(grepl(metric,ASSESSMENT_TYPE,ignore.case=T),
    ASSESSMENT_TERM == assertAssessmentTermEnd(metric) | ASSESSMENT_TERM == 0,
    PATIENT_ID_NUM %in% vets_with_treatment) 
  removeEmpty<-emptyColumns(processedData)
  processedData<-processedData[,-removeEmpty]
  processedData

}

generateTable1<-function(){
  questions<-c(
    "% of participants that reduce their PCL score by 5 points or more",
    "% of participants that reduce their PCL score by 10 points or more",
    "% of participants that reduce their PHQ-9 score by 3 points or more",
    "% of participants that improved by 2 points or more on CD-RISC",
    "% of Mental Component Score participants that improved by 2 points or more on VR-12",
    "% of Physical Component Score participants that improved by 2 points or more on VR-12",
    "% of Survey participants who overall felt satisfied with the clinical care received at AMC"
  )
  table1<-data.frame(question=questions,
                     value=rep(NA,7))
  
  PCLVetFAM<-assessments %>%
    filter(grepl("PCL",ASSESSMENT_TYPE,ignore.case=T),
           SERVICE_LINE=="IOP",
           ASSESSMENT_TERM == 1 | ASSESSMENT_TERM ==0,
           PATIENT_ID_NUM %in% vets_with_treatment$PATIENT_ID_NUM) %>%
    select(-one_of(c("ASSESSMENT_RESPONSE_VALUE21","ASSESSMENT_RESPONSE_VALUE22"))) %>%
    filter ((ASSESSMENT_TYPE == "PCL5" & ASSESSMENT_TERM == 0) | (ASSESSMENT_TYPE == "PCL5W" & ASSESSMENT_TERM == 1)) %>%
    mutate(ASSESSMENT_DATE=as.Date(ASSESSMENT_DATE, format="%m/%d/%Y"),
           inFiscalYear=ASSESSMENT_DATE> quarters$startDates[5]) %>%
    group_by(PATIENT_ID_NUM,ASSESSMENT_TERM) %>%
    slice_max(ASSESSMENT_DATE,n=1) %>%
    ungroup()
  
  removalFreq <-table(PCLVetFAM$PATIENT_ID_NUM,PCLVetFAM$ASSESSMENT_TERM) %>%
    as.data.frame() %>%
    filter(Freq!=1) %>%
    select(Var1) %>%
    mutate(Var1=as.character(Var1)) %>%
    arrange() %>%
    distinct() %>%
    unlist() %>% 
    unname()
}

generateTable2<-function(){
  
}

generateTable3<-function(){
  
}

generateTable4<-function(){
  
}


generateKPIreport<-function(in.dir,masterListFile,out.dir,cutoffDate=today) {
  #this chuck just reads in the current
  assessments<-fread(in.dir %&% "assessment_2023-01-09.csv",na=c("99","999","NA",""))
  patients<-fread(in.dir %&% "patient_2023-01-09.csv",na=c("99","999","NA",""))
  visits<-fread(in.dir %&% "visit_2023-01-09.csv",na=c("99","999","NA",""))
  referrals<-fread(in.dir %&% "referral_2023-01-09.csv",na=c("99","999","NA",""))
  satisfaction<-fread(in.dir %&% "satisfaction_2023-01-09.csv",na=c("99","999","NA",""))
  master_list_services<-fread(masterListFile,na=c("")) %>%
    filter(!is.na(`Treatment*`)) %>%
    rename(Treatment="Treatment*") %>%
    separate(`Master List of Therapies and Services (# = CDS value)`,into=c("TreatmentID"),sep=" ") %>% 
    mutate(TreatmentID=as.integer(TreatmentID))
  
  fiscalYearStart<-determineFiscalYear(cutoffDate)
  quarters<-createQuarterTemplate(cutoffDate)
  

}



