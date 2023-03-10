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
  if (metric == 'VARAND' | metric == 'MCS' | metric == 'PCS') {
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

#this function returns an array with veteran IDs
getVetRecordIDs<-function(patients){
  veterans<-patients %>%
    filter(PATIENT_TYPE == "VET")  %>%
    mutate(PATIENT_ID_NUM=as.character(PATIENT_ID_NUM)) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>%
    distinct() %>%
    unlist() %>%
    unname()
  return(veterans)
}
#this function processes the assessments to calculate the percentage of individuals meeting some change sore threshold on a given metric in the current fiscal year
calcOutcomes<-function(data,metric,threshold,cutoff=today,withTreatment,veterans){
  processedData<-data %>%
    filter(grepl(metric,ASSESSMENT_TYPE,ignore.case=T),
    ASSESSMENT_TERM == assessmentTermEnd(metric) | ASSESSMENT_TERM == 0,
    PATIENT_ID_NUM %in% withTreatment,
    PATIENT_ID_NUM %in% veterans) %>%
    mutate(ASSESSMENT_POINT=case_when(
      ASSESSMENT_TERM==0 ~ 'Baseline',
      ASSESSMENT_TERM==assessmentTermEnd(metric) ~ 'Endpoint',
      T ~ NA_character_
    ))
  if(metric == 'PCL'){
    processedData<-processedData %>%
      filter(!(ASSESSMENT_TYPE == 'PCL5' & ASSESSMENT_TERM == 1),
             !(ASSESSMENT_TYPE == 'PCL5W' & ASSESSMENT_TERM == 0))
  }
  removeEmpty<-!emptyColumns(processedData)
  processedData<-processedData[,..removeEmpty]
  #add a try catch - if can't calculate the statistic return an NA
  out <- tryCatch(
    {
      processedData<-processedData %>%
        mutate(ASSESSMENT_DATE=as.Date(ASSESSMENT_DATE, format="%m/%d/%Y")) %>%
        group_by(PATIENT_ID_NUM,ASSESSMENT_POINT) %>%
        slice_max(ASSESSMENT_DATE,n=1,with_ties = F) %>%
        ungroup() %>%
        inFiscalYear(metric=metric,cutoff=cutoff)
      sum_scores<-processedData %>% 
        select(contains("ASSESSMENT_RESPONSE_VALUE")) %>%
        apply(1,sum)
      processedData<-cbind.data.frame(processedData,sum_scores)
      sanitized<-processedData %>%
        select(PATIENT_ID_NUM,ASSESSMENT_POINT,sum_scores) %>%
        pivot_wider(id_cols=PATIENT_ID_NUM,
                    names_from=ASSESSMENT_POINT,
                    values_from = sum_scores) %>%
        mutate(delta=Baseline-Endpoint,
               meetsThreshold=delta>=threshold)
      sanitized<-sanitized[complete.cases(sanitized),]
      res<-table(sanitized$meetsThreshold)/sum(table(sanitized$meetsThreshold)) * 100
      return(res[2])
    },
    error=function(cond) {
      message("Failed to calculate result")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
  )  
  return(out) 
}


#this function calculates the satisfation rate
calcSatisfaction<-function(satisfaction,cutoff) {
  withResponse<-satisfaction %>%
    mutate(SURVEY_DATE=as.Date(SURVEY_DATE,"%Y-%m-%d")) %>%
    filter(!is.na(OVERALL_SATISFACTION),
           SERVICE_LINE == "IOP",
           SURVEY_DATE>=determineFYStart(cutoff)) #1301
  positiveResponse<-filter(withResponse,OVERALL_SATISFACTION == "A" | OVERALL_SATISFACTION == "SA")
  res<-nrow(positiveResponse)/sum(!is.na(withResponse$OVERALL_SATISFACTION)) * 100
  return(res)
}

#this function goes through each outcome and calculates the result
createTable1<-function(assessments,cutoff=today,withTreatment,veterans){
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
  
  table1[1,2]<-calcOutcomes(data=assessments,metric='PCL',threshold=5,cutoff=cutoff,withTreatment=withTreatment,veterans)
  table1[2,2]<-calcOutcomes(data=assessments,metric='PCL',threshold=10,cutoff=cutoff,withTreatment=withTreatment,veterans)
  table1[3,2]<-calcOutcomes(data=assessments,metric='PHQ',threshold=3,cutoff=cutoff,withTreatment=withTreatment,veterans)
  table1[4,2]<-100-calcOutcomes(data=assessments,metric='CDRISC',threshold=-2,cutoff=cutoff,withTreatment=withTreatment,veterans)
  table1[5,2]<-100-calcOutcomes(data=assessments,metric='MCS',threshold=-2,cutoff=cutoff,withTreatment=withTreatment,veterans)
  table1[6,2]<-100-calcOutcomes(data=assessments,metric='PCS',threshold=-2,cutoff=cutoff,withTreatment=withTreatment,veterans)
  return(table1)
}

preprocessEngagement<-function(patients,visits,quarters) {
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

createTables2_3_4_5<-function(patients,visits,quarters) {

    preprocessedEngagement<-preprocessEngagement(patients,visits,quarters)

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


countReferrals<-function(referrals,patients,quarters) {
  
  referral_processed<-referrals %>% 
    filter(DATA_VALUE_TYPE == "IN") %>%
    rename(SERVICE_DATE='EVENT_DATE') %>%
    mutate(SERVICE_DURATION=999) %>%
    preprocessEngagement(patients=patients,visits=.,quarters) %>%
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


#	Average Time in Days to Treatment
overallStart<-function(data,withTreatment,veterans){
  startData<-data %>%
    filter(PATIENT_ID_NUM %in% withTreatment,
           as.character(PATIENT_ID_NUM) %in% veterans) %>%
    group_by(PATIENT_ID_NUM) %>%
    slice_min(START_DATE,n=1,with_ties = F) %>%
    select(START_DATE,PATIENT_ID_NUM)
  #SERVICE_DATE >= quarters$startDates[5])
  return(startData)
}

referralDates<-function(data,quarters) {
  refSubset<-data %>%
    mutate(EVENT_DATE = as.Date(EVENT_DATE,"%Y-%m-%d")) %>%
    filter(DATA_VALUE_TYPE == "IN",
           EVENT_DATE >= quarters$startDates[5]) %>%
    group_by(PATIENT_ID_NUM) %>%
    slice_max(EVENT_DATE,n=1,with_ties = F) %>%
    select(PATIENT_ID_NUM,EVENT_DATE)
  return(refSubset)
  #filter DATA_VALUE_CODE=="WWP"
  #DATA_VALUE_CODE!="WWP",
}


summariseDaysToEvent<-function(refDates,startDates){
  compare_dates=inner_join(startDates,refDates,by=c("PATIENT_ID_NUM")) %>%
    mutate(timedelta=difftime(START_DATE,EVENT_DATE,units="days")) %>%
    filter(timedelta>0)
  
  result<-compare_dates %>%
    ungroup() %>%
    summarise(overall_N=n(),
              total_days=sum(timedelta),
              avg_days=mean(timedelta),
              median_days=median(timedelta),
              min_days=min(timedelta),
              max_days=max(timedelta))
  return(result)
}


createDaysUntilTables<-function(visits,referrals,patients,quarters,withTreatment,veterans) {
  iopStartDates<-visits %>% filter(SERVICE_LINE=='IOP',IOP_START_DATE >= quarters$startDates[5])  %>% rename(START_DATE='IOP_START_DATE') %>% overallStart(withTreatment,veterans)
  firstVisitDates<-visits %>% filter(SERVICE_DATE >= quarters$startDates[5]) %>% rename(START_DATE='SERVICE_DATE') %>% overallStart(withTreatment,veterans)
  acceptanceDates<-patients %>% filter(ACCEPTED_DATE >= quarters$startDates[5],PATIENT_TYPE == "VET",ACCEPTED_FLAG=="Y") %>% rename(START_DATE='ACCEPTED_DATE') %>% overallStart(withTreatment,veterans)
  
  #all individuals
  refs<-referrals %>% referralDates(quarters)
  table7<-summariseDaysToEvent(refs,iopStartDates)
  table10<-summariseDaysToEvent(refs,firstVisitDates)
  table13<-summariseDaysToEvent(refs,acceptanceDates)
  
  #all WWP referred individuals
  refs<-referrals %>% filter(DATA_VALUE_CODE=="WWP") %>% referralDates(quarters)
  table8<-summariseDaysToEvent(refs,iopStartDates)
  table11<-summariseDaysToEvent(refs,firstVisitDates)
  table14<-summariseDaysToEvent(refs,acceptanceDates)
  
  #all nonWWP referred individuals
  refs<-referrals %>% filter(DATA_VALUE_CODE!="WWP") %>% referralDates(quarters)
  table9<-summariseDaysToEvent(refs,iopStartDates)
  table12<-summariseDaysToEvent(refs,firstVisitDates)
  table15<-summariseDaysToEvent(refs,acceptanceDates)
  
  return(list(table7,table8,table9,table10,table11,table12,table13,table14,table15))
}

patientsInFiscalYear<-function(visits,quarters) {
  patients<-visits %>%
    mutate(SERVICE_DATE = as.Date(SERVICE_DATE,"%Y-%m-%d")) %>%
    filter(SERVICE_DATE >= quarters$startDates[5]) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>%
    distinct() %>%
    unlist() %>%
    unname()
  return(patients)
}

patientsInTrailingTwelveMonths<-function(visits,cutoff) {
  patients<-visits %>%
    mutate(SERVICE_DATE = as.Date(SERVICE_DATE,"%Y-%m-%d")) %>%
    filter(SERVICE_DATE >= cutoff - months(12)) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>%
    distinct() %>%
    unlist() %>%
    unname()
  return(patients)
}

#Grabs the unique list of patients that visited in the last year
#grabs the most recent visit for each patient in each service line
#any patients with more than one record after this gets relabeled as BOTH
#refilter to most recent patient in each service group including BOTH
#finally join with patients df and tabulate the results
tabulateVisits<-function(visits,patients,quarters) {
  past12MonthVisits<-visits %>%
      mutate(SERVICE_DATE = as.Date(SERVICE_DATE,"%Y-%m-%d")) %>%
      filter(SERVICE_DATE >= quarters$startDates[5])
  #group by IOP and patient id then slice max by date
  patient_count<-past12MonthVisits %>%
    group_by(PATIENT_ID_NUM,SERVICE_LINE) %>%
    slice_max(SERVICE_DATE,n=1,with_ties = F) %>%
    select(PATIENT_ID_NUM) %>%
    ungroup() %>%
    group_by(PATIENT_ID_NUM) %>%
    summarise(count=n())  %>%
    ungroup()
  
  past12MonthVisits<-inner_join(past12MonthVisits,patient_count,by=c("PATIENT_ID_NUM")) 
  summarise_visits<-past12MonthVisits %>% 
    mutate(group=if_else(count==2,"BOTH",SERVICE_LINE)) %>%
    group_by(PATIENT_ID_NUM) %>%
    slice_max(SERVICE_DATE,with_ties = FALSE) %>%
    inner_join(patients,by=c("PATIENT_ID_NUM"))

  table17<-table(summarise_visits$group,summarise_visits$PATIENT_TYPE) %>% as.data.frame()

  return(table17)
}


generateTables16_17<-function(visits,patients,quarters,cutoff) {
  patientIDs<-patientsInTrailingTwelveMonths(visits,cutoff)

  table16<-patients %>%
    filter(PATIENT_ID_NUM %in% patientIDs) %>%
    group_by(PATIENT_TYPE) %>%
    summarise(PatientCount=n())

  table17<-tabulateVisits(visits,patients,quarters)

  return(list(table16,table17))
}


generateKPIreport<-function(assessments,patients,visits,referrals,satisfaction, master_list_services,
                            out.dir,cutoffDate=today) {
  #this chuck just reads in the current
  #in.dir<-'/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Cross site data/'
  
  withTreatment<-getWithTreatmentRecordIDs(visits,master_list_services)
  veterans<-getVetRecordIDs(patients)
  fiscalYearStart<-determineFiscalYear(cutoffDate)
  quarters<-createQuarterTemplate(cutoffDate)

  
  table1<-createTable1(assessments,cutoff=cutoffDate,withTreatment,veterans)
  table1[7,2]<-calcSatisfaction(satisfaction,cutoffDate)
    
  tables2_3_4_5<-createTables2_3_4_5(patients,visits,quarters)
  
  table6<-countReferrals(referrals,patients,quarters)
  
  tables7To15<-createDaysUntilTables(visits,referrals,patients,quarters,withTreatment,veterans)

  tables16_17<-generateTables16_17(visits,patients,quarters,cutoff = cutoffDate)
  
  outputList<-list(
    overall_outcomes=table1,
    visit_count_quarterly=tables2_3_4_5[[1]],
    hours_total=tables2_3_4_5[[2]],
    hours_quarterly=tables2_3_4_5[[3]],
    referrals=table6,
    DTT_Overall_IOP=tables7To15[[1]],
    DTT_WWPReferred_IOP=tables7To15[[2]],
    DTT_Non_WWPReferred_IOP=tables7To15[[3]],
    DTFV_Overall=tables7To15[[4]],
    DTFV_WWPReferred=tables7To15[[5]],
    DTFV_Non_WWPReferred=tables7To15[[6]],
    DTA_Overall=tables7To15[[7]],
    DTA_WWPReferred=tables7To15[[8]],
    DTA_Non_WWPReferred=tables7To15[[9]],
    unique_patients=tables16_17[[1]],
    unique_patients_by_program=tables16_17[[2]])
  write_xlsx(outputList,out.dir %&% 'KPI_crosssite_report_' %&% cutoffDate %&% '.xlsx')
}



