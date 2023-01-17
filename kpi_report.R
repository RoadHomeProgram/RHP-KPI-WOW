#this is a nicer version of the kpi script 
#instead of having them all in one function, lets separate each question out into separate functionalities

library(data.table)
library(naniar)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

source("/Users/ryanschubert/Documents/RHP-KPI-WOW/helpers.R")

#returns the correct assessment end term
#1 for all variables except for vr12 items
assessmentTermEnd<-function(metric){
  if (metric == 'VARAND') {
    return(9001)
  } else {
    return(1)
  }
}

#this function returns a dataset containing patients with an endpoint assessment for a given metric in the current fiscal year
inFiscalYear<-function(data,metric) {
  data<-data %>%
    mutate(inFiscalYear=ASSESSMENT_DATE> determineFYStart(cutoff))
  inYear<-data %>%
    filter(ASSESSMENT_TERM == assessmentTermEnd(metric),
          inFiscalYear) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>%
    distinct() %>%
    unlist() %>% 
    unname()
  data<-data %>%
    filter(PATIENT_ID_NUM  %in% inYear)
  return(data)
}


#this function returns a dataset with veterans who have at least one treatment according to the master list of therapies
withTreatment<-function(data,services=master_list_services) {
  vets_with_treatment<-data %>%
    filter(SERVICE_PERFORMED %in% master_list_services$TreatmentID) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>%
    distinct() %>%
    unlist() %>%
    unname()
  return(vets_with_treatment)
}

#this function processes the assessments to calculate the percentage of individuals meeting some change sore threshold on a given metric in the current fiscal year
calcOutcomes<-function(data,metric,threshold,cutoff=today){
  processedData<-data %>%
    filter(grepl(metric,ASSESSMENT_TYPE,ignore.case=T),
    ASSESSMENT_TERM == assessmentTermEnd(metric) | ASSESSMENT_TERM == 0,
    PATIENT_ID_NUM %in% vets_with_treatment) 
  removeEmpty<-!emptyColumns(processedData)
  processedData<-processedData[,..removeEmpty]
  processedData<-processedData %>%
    mutate(ASSESSMENT_DATE=as.Date(ASSESSMENT_DATE, format="%m/%d/%Y")) %>%
    group_by(PATIENT_ID_NUM,ASSESSMENT_TERM) %>%
    slice_max(ASSESSMENT_DATE,n=1,with_ties = F) %>%
    ungroup() %>%
    inFiscalYear(metric=metric)
  sum_scores<-processedData %>% 
    select(contains("ASSESSMENT_RESPONSE_VALUE")) %>%
    apply(1,sum)
  processedData<-cbind.data.frame(processedData,sum_scores)
  sanitized<-processedData %>%
    select(PATIENT_ID_NUM,ASSESSMENT_TERM,sum_scores) %>%
    pivot_wider(id_cols=PATIENT_ID_NUM,
                names_from=ASSESSMENT_TERM,
                values_from = sum_scores) %>%
    mutate(delta=`0`-`1`,
            meetsThreshold=delta>=threshold)
  res<-table(sanitized$meetsThreshold)/sum(table(sanitized$meetsThreshold)) * 100
  return(res[2])

}

calcSatisfaction<-function(satisfaction) {
  withResponse<-satisfaction %>%
    mutate(SURVEY_DATE=as.Date(SURVEY_DATE,"%Y-%m-%d")) %>%
    filter(!is.na(OVERALL_SATISFACTION),
           SERVICE_LINE == "IOP",
           SURVEY_DATE>=quarters$startDates[5]) #1301
  positiveResponse<-filter(withResponse,OVERALL_SATISFACTION == "A" | OVERALL_SATISFACTION == "SA")
  res<-nrow(positiveResponse)/sum(!is.na(withResponse$OVERALL_SATISFACTION)) * 100
  return(res)
}

generateTable1<-function(assessments,cutoff=today){
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
  
  table1[1,2]<-calcOutcomes(data=assessments,metric='PCL',threshold=5,cutoff=cutoff)
  table1[2,2]<-calcOutcomes(data=assessments,metric='PCL',threshold=10,cutoff=cutoff)
  table1[3,2]<-calcOutcomes(data=assessments,metric='PHQ',threshold=3,cutoff=cutoff)
  table1[4,2]<-100-calcOutcomes(data=assessments,metric='CDRISC',threshold=-1,cutoff=cutoff)
  table1[5,2]<-100-calcOutcomes(data=assessments,metric='MCS',threshold=-2,cutoff=cutoff)
  table1[6,2]<-100-calcOutcomes(data=assessments,metric='PCS',threshold=-2,cutoff=cutoff)
  return(table1)
}

generateTable2<-function(){
  
}

generateTable3<-function(){
  
}

generateTable4<-function(){
  
}




generateKPIreport<-function(in.dir,masterListFile,out.dir,cutoffDate=today) {
  #this chuck just reads in the current
  assessments<-fread(in.dir %&% "assessment_2023-01-17.csv",na=c("99","999","NA",""))
  patients<-fread(in.dir %&% "patient_2023-01-17.csv",na=c("99","999","NA",""))
  visits<-fread(in.dir %&% "visit_2023-01-17.csv",na=c("99","999","NA",""))
  referrals<-fread(in.dir %&% "referral_2023-01-17.csv",na=c("99","999","NA",""))
  satisfaction<-fread(in.dir %&% "satisfaction_2023-01-17.csv",na=c("99","999","NA",""))
  master_list_services<-fread("/Users/ryanschubert/Dropbox (Rush)/Ryan's stuff/rush/remake KPI/Master List of therapies.csv",na=c("")) %>%
    filter(!is.na(`Treatment*`)) %>%
    rename(Treatment="Treatment*") %>%
    separate(`Master List of Therapies and Services (# = CDS value)`,into=c("TreatmentID"),sep=" ") %>% 
    mutate(TreatmentID=as.integer(TreatmentID))
  
  vets_with_treatment<-withTreatment(visits)
  fiscalYearStart<-determineFiscalYear(cutoffDate)
  quarters<-createQuarterTemplate(cutoffDate)
  
  tmp<-generateTable1(assessments)
  tmp[7,2]<-calcSatisfaction(satisfaction)
    
  
  
}



