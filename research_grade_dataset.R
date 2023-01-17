library(data.table)
library(dplyr)
library(tidyr)
"%&%" = function(a,b) paste0(a,b)
"%_%" = function(a,b) paste(a,b,sep='_')
generateDataset<-function(assessments,patients,out.dir){
  assessments<-fread(assessments,na=c("99","999"))
  patients<-fread(patients,na=c("99","999"))
  
  longScores<-assessments %>%
      group_by(PATIENT_ID_NUM,ASSESSMENT_TERM,FACILITY_NAME,ASSESSMENT_TYPE,SERVICE_LINE) %>%
      slice_max(ASSESSMENT_DATE,n=1,with_ties = F)  %>%
      ungroup() %>%
      filter((ASSESSMENT_TYPE == "PCL5" & ASSESSMENT_TERM==0) | 
              (ASSESSMENT_TYPE == "PCL5W" & ASSESSMENT_TERM==1) |
              !(ASSESSMENT_TYPE %in% c('PCL5','PCL5W','SF4'))) %>%
      pivot_longer(starts_with("ASSESSMENT_RESPONSE_VALUE"))  %>%
      filter(!is.na(value)) %>%
      mutate(itemNumber=gsub("ASSESSMENT_RESPONSE_VALUE",'',name),
             timepoint=case_when(
               ASSESSMENT_TERM == 0 ~ "Base",
               ASSESSMENT_TERM == 1 ~ "Post",
               ASSESSMENT_TERM == 9001 ~ "3Month",
               ASSESSMENT_TERM == 9002 ~ "6Month",
               ASSESSMENT_TERM == 9003 ~ "12Month",
               T ~ as.character(ASSESSMENT_TERM)
               ),
             newNames=ASSESSMENT_TYPE %_% itemNumber %_% timepoint)
  summary_scores<-longScores %>% 
    group_by(PATIENT_ID_NUM,ASSESSMENT_TERM,FACILITY_NAME,ASSESSMENT_TYPE,SERVICE_LINE) %>%
    summarise(sumScore=sum(value)) %>%
    mutate(ASSESSMENT_TYPE = ASSESSMENT_TYPE %_% 'Total') %>%
    pivot_wider(id_cols = c(PATIENT_ID_NUM,FACILITY_NAME,SERVICE_LINE),
                names_from=c(ASSESSMENT_TYPE,ASSESSMENT_TERM),
                values_from = sumScore) %>%
    select(-(starts_with('MCS') | starts_with('PCS'))) %>%
    select(PATIENT_ID_NUM,FACILITY_NAME,SERVICE_LINE,ends_with('_0') | ends_with('_1') | ends_with('_9001') | ends_with('_9002') | ends_with('_9003')) %>%
    rename_with(.fn=function(x){gsub('_0','_Base',x)}) %>%
    rename_with(.fn=function(x){gsub('_1','_Post',x)}) %>%
    rename_with(.fn=function(x){gsub('_9001','_3Month',x)}) %>%
    rename_with(.fn=function(x){gsub('_9002','_6Month',x)}) %>%
    rename_with(.fn=function(x){gsub('_9003','_12Month',x)})
    
  
  wideScores<-longScores %>% 
    pivot_wider(id_cols=c(PATIENT_ID_NUM,FACILITY_NAME,SERVICE_LINE),
                names_from = newNames,
                values_from=value)  %>%
    select(PATIENT_ID_NUM,
                FACILITY_NAME,
                SERVICE_LINE,
                ends_with('_Base'),
                ends_with('_Post'),
                ends_with('_3Month'),
                ends_with('_6Month'),
                ends_with('_12Month'))
  output<-right_join(patients,wideScores,by=c("PATIENT_ID_NUM","FACILITY_NAME")) %>% 
    left_join(summary_scores,by=c("PATIENT_ID_NUM","SERVICE_LINE","FACILITY_NAME")) %>%
    select(-V42)
  #pivot the assessment values longer
  #create item number from assessment value column name
  #then paste item number, assessment term, and assessment type together
  #then pivot wider based on that
  fwrite(output,out.dir %&% "Cross_Site_wide_dataset" %_% as.Date(Sys.Date()) %&% ".csv")
}