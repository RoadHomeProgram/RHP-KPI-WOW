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
inFiscalYear<-function(data,metric,cutoff=today) {
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
    inFiscalYear(metric=metric,cutoff=cutoff)
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

preprocessEngagement<-function(patients,visits) {
  patientType<-patients %>%
    select(PATIENT_ID_NUM,PATIENT_TYPE,ACCEPTED_FLAG,NOT_ACCEPTED_REASON)

  visits_quarterly_summary<-visits %>%
    mutate(SERVICE_DATE=as.Date(SERVICE_DATE,"%Y-%m-%d"),
           quarter=case_when(
             SERVICE_DATE >= quarters$startDates[1] ~ quarters$quarter[1],
             SERVICE_DATE >= quarters$startDates[2] ~ quarters$quarter[2],
             SERVICE_DATE >= quarters$startDates[3] ~ quarters$quarter[3],
             SERVICE_DATE >= quarters$startDates[4] ~ quarters$quarter[4],
             SERVICE_DATE >= quarters$startDates[5] ~ quarters$quarter[5],
             T ~ NA_character_
           )) %>%
    filter(!is.na(quarter),
           SERVICE_DURATION > 0) %>%
    left_join(patientType,by=c("PATIENT_ID_NUM"))
  return(visits_quarterly_summary)
}

averageHours<-function(data) {
    data<-data %>%
        summarise(Total_Patients=n_distinct(PATIENT_ID_NUM),
                Total_Hours=sum(SERVICE_DURATION)/60,
                Total_Visits=n()) %>%
        mutate(Avg_Sessions=Total_Visits/Total_Patients,
            Avg_Hours=Total_Hours/Total_Patients)
    return(data)
}

medianHours<-function(data,quarterly=FALSE) {
    data<-data %>%
        summarise(total_hours=sum(SERVICE_DURATION)/60,
                Total_visits=n()) %>%
        ungroup()
        if(quarterly) {
            data<-data %>%
              group_by(quarter,SERVICE_LINE,PATIENT_TYPE) %>%
                summarise(median_hours=median(total_hours),
                        median_visits=median(Total_visits))
        } else {
            data<-data %>%
              group_by(SERVICE_LINE,PATIENT_TYPE) %>%
                summarise(median_hours=median(total_hours),
                        median_visits=median(Total_visits))
        }
    return(data)
}

generateTables2_3_4_5<-function(patients,visits) {

    preprocessedEngagement<-preprocessEngagement(patients,visits)

    table2<-preprocessedEngagement %>%
        group_by(quarter,SERVICE_LINE,PATIENT_TYPE) %>%
        count()

    average_hours_summary<-preprocessedEngagement %>%
        group_by(SERVICE_LINE,PATIENT_TYPE) %>%
        averageHours()
    
    median_hours_summary<-preprocessedEngagement %>%
        group_by(PATIENT_ID_NUM,SERVICE_LINE,PATIENT_TYPE) %>%
        medianHours()

    table3<-left_join(average_hours_summary,median_hours_summary,by=c("SERVICE_LINE","PATIENT_TYPE"))

    average_hours_quarterly<-preprocessedEngagement %>%
        group_by(quarter,SERVICE_LINE,PATIENT_TYPE) %>%
        averageHours()

    median_hours_quarterly<-preprocessedEngagement %>%
        group_by(quarter,PATIENT_ID_NUM,SERVICE_LINE,PATIENT_TYPE) %>%
        medianHours(quarterly=TRUE)

    table4_5<-left_join(average_hours_quarterly,median_hours_quarterly,by=c("quarter","SERVICE_LINE","PATIENT_TYPE"))

    return(list(table2,table3,table4_5))
}


countReferrals<-function(referrals,patients) {
  
  referral_processed<-referrals %>% 
    filter(DATA_VALUE_TYPE == "IN") %>%
    rename(SERVICE_DATE='EVENT_DATE') %>%
    mutate(SERVICE_DURATION=999) %>%
    preprocessEngagement(patients=patients,visits=.) %>%
    group_by(PATIENT_ID_NUM) %>%
    slice_max(SERVICE_DATE,n=1) %>%
    filter(SERVICE_DATE >= quarters$startDates[5],
           DATA_VALUE_CODE == "WWP",
           PATIENT_TYPE == "VET",
           ACCEPTED_FLAG %in% c("N","NR","Y")) %>% 
    select(-SERVICE_DURATION)
  
  ACCEPTANCE_TABLE<-table(referral_processed$quarter,referral_processed$ACCEPTED_FLAG) %>%
    as.data.frame() %>%
    pivot_wider(id_cols=Var1,names_from=Var2,values_from=Freq) %>%
    mutate(Total = N + NR + Y,
           Pending = NR,
           Pending_perc = Pending/Total*100,
           Resolved = N + Y,
           Resolved_perc = Resolved/Total*100,
           Accepted = Y,
           Accepted_perc = Accepted/Resolved*100) %>%
    rename(quarter=Var1)
  
  NOT_ACCEPTED_by_AMC<-referral_processed %>%
    filter(ACCEPTED_FLAG == "N",
           !(NOT_ACCEPTED_REASON %in% c("LOS","CON","TRA"))) %>%
    group_by(quarter) %>%
    summarise(NA_by_AMC=n())
  
  NOT_ACCEPTED_by_Patient<-referral_processed %>%
    filter(ACCEPTED_FLAG == "N",
           (NOT_ACCEPTED_REASON %in% c("CON","TRA"))) %>%
    group_by(quarter) %>%
    summarise(NA_by_patient=n())
  
  NOT_ACCEPTED_LOS<-referral_processed %>%
    filter(ACCEPTED_FLAG == "N",
           NOT_ACCEPTED_REASON == "LOS") %>%
    group_by(quarter) %>%
    summarise(NA_by_LOS=n())
  
  table6<-full_join(ACCEPTANCE_TABLE,NOT_ACCEPTED_by_AMC,by=c("quarter")) %>%
    full_join(NOT_ACCEPTED_by_Patient,by=c("quarter")) %>%
    full_join(NOT_ACCEPTED_LOS,by=c("quarter")) %>%
    mutate(NA_by_AMC_perc=NA_by_AMC/(Y+N)*100,
           NA_by_patient_perc=NA_by_patient/(Y+N)*100,
           NA_by_LOS_perc=NA_by_LOS/(Y+N)*100)
  table6<-table6 %>% select(-one_of(c("Y","N","NR")))
  return(table6)
}

generateKPIreport<-function(in.dir,masterListFile,out.dir,cutoffDate=today) {
  #this chuck just reads in the current
  #in.dir<-'/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Cross site data/'
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
  
  table1<-generateTable1(assessments,cutoff=cutoffDate)
  table1[7,2]<-calcSatisfaction(satisfaction)
    
  tables2_3_4_5<-generateTables2_3_4_5(patients,visits)
  
  table6<-countReferrals(referrals,patients)
}



