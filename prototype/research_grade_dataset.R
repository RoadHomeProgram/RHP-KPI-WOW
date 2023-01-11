library(data.table)
library(dplyr)
library(tidyr)
"%&%" = function(a,b) paste0(a,b)
"%_%" = function(a,b) paste(a,b,sep='_')
generateDataset<-function(in.dir,out.dir){
  assessments<-fread(in.dir %&% "assessment_2023-01-09.csv",na=c("99","999"))
  patients<-fread(in.dir %&% "patient_2023-01-09.csv",na=c("99","999"))
  
  longScores<-assessments %>%
      group_by(PATIENT_ID_NUM,ASSESSMENT_TERM,FACILITY_NAME,ASSESSMENT_TYPE,SERVICE_LINE) %>%
      slice_max(ASSESSMENT_DATE,n=1,with_ties = F)  %>%
      ungroup() %>%
      filter((ASSESSMENT_TYPE == "PCL5" & ASSESSMENT_TERM==0) | (ASSESSMENT_TYPE == "PCL5W" & ASSESSMENT_TERM==1)) %>%
      pivot_longer(starts_with("ASSESSMENT_RESPONSE_VALUE"))  %>%
      filter(!is.na(value)) %>%
      mutate(itemNumber=gsub("ASSESSMENT_RESPONSE_VALUE",'',name),
             timepoint=if_else(ASSESSMENT_TERM == 0, "Base","Post"),
             newNames=ASSESSMENT_TYPE %_% itemNumber %_% timepoint) #%>%
  summary_scores<-longScores %>% 
    group_by(PATIENT_ID_NUM,ASSESSMENT_TERM,FACILITY_NAME,ASSESSMENT_TYPE,SERVICE_LINE) %>%
    summarise(sumScore=sum(value)) %>%
    pivot_wider(id_cols = c(PATIENT_ID_NUM,FACILITY_NAME,SERVICE_LINE),
                names_from=ASSESSMENT_TYPE,
                values_from = sumScore) %>%
    rename(PCL5_Total_Base="PCL5",
           PCL5_Total_Post="PCL5W")
  
  wideScores<-longScores %>% 
    pivot_wider(id_cols=c(PATIENT_ID_NUM,FACILITY_NAME,SERVICE_LINE),
                names_from = newNames,
                values_from=value)
  output<-right_join(patients,wideScores,by=c("PATIENT_ID_NUM","FACILITY_NAME")) %>% 
    left_join(summary_scores,by=c("PATIENT_ID_NUM","SERVICE_LINE")) 
  #pivot the assessment values longer
  #create item number from assessment value column name
  #then paste item number, assessment term, and assessment type together
  #then pivot wider based on that
  fwrite(output,out.dir %&% "pcl_scores_research_grade" %_% as.Date(Sys.Date()) %&% ".csv")
}