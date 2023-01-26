library(data.table)
library(naniar)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readxl)
library(writexl)

source("/Users/ryanschubert/Documents/RHP-KPI-WOW/helpers.R")


## 1. Warrior Care Network Participants
plotWCNParticipants<-function(visits,patients,masterListServices){
  
  visitCount<-visits %>%
    filter(SERVICE_DATE <= '2022-09-30') %>%
    group_by(PATIENT_ID_NUM,SERVICE_LINE) %>%
    summarise(count=n())
  table(table(visitCount$PATIENT_ID_NUM))
  multiHistorical<-names(table(visitCount$PATIENT_ID_NUM))[table(visitCount$PATIENT_ID_NUM)>1] 
  visitCount<-visits %>%
    filter(SERVICE_DATE > '2022-09-30',
           SERVICE_DATE < '2022-11-05') %>%
    group_by(PATIENT_ID_NUM,SERVICE_LINE) %>%
    summarise(count=n())
  table(table(visitCount$PATIENT_ID_NUM))
  multiFY2023<-names(table(visitCount$PATIENT_ID_NUM))[table(visitCount$PATIENT_ID_NUM)>1] 
  allMulti<-c(multiHistorical,multiFY2023)
  
  patientType<-patients %>%
    select(PATIENT_ID_NUM,PATIENT_TYPE) %>%
    arrange() %>% distinct()
  
  annotatedVisits<-visits %>%
    filter(SERVICE_PERFORMED %in% masterListServices$TreatmentID,
           SERVICE_DATE < '2022-11-05') %>%
    select(PATIENT_ID_NUM,SERVICE_LINE,SERVICE_DATE,FACILITY_NAME) %>%
    mutate(FY = if_else(SERVICE_DATE >= '2022-10-01','FY23','2015-06-01 to 2022-09-30')) %>%
    inner_join(patientType,by=c('PATIENT_ID_NUM')) %>%
    filter(!(SERVICE_LINE == 'IOP' & PATIENT_TYPE == 'FAM')) %>%
    mutate(SERVICE_LINE = if_else((PATIENT_ID_NUM %in% multiHistorical & SERVICE_DATE <= '2022-09-30') | 
                                    (PATIENT_ID_NUM %in% multiFY2023 & SERVICE_DATE > '2022-09-30') ,
                                  'OP/IOP',
                                  SERVICE_LINE),
           colorGroup='breakdown') %>%
    select(-SERVICE_DATE) %>%
    arrange() %>%
    distinct()
  
  annotatedVisits<-annotatedVisits %>% mutate(SERVICE_LINE='Total',colorGroup='Total') %>%
    rbind.data.frame(annotatedVisits)
  
  pBar<-ggplot(annotatedVisits,aes(x=SERVICE_LINE)) + 
    geom_bar(aes(fill=colorGroup)) +
    facet_wrap(~FY,scales="free") +
    geom_text(stat='count', aes(label=..count..), vjust=1.5) +
    theme_bw() +
    theme(legend.position = "none")
  
  return(pBar)
}

## 2. Satisfaction Results All time
calcSatisfactionRates<-function(satisfaction){
  filled_satisfaction<-satisfaction %>%
    filter(SERVICE_LINE == "IOP",
           !is.na(RECOMMEND_PROGRAM) &
             !is.na(OVERALL_SATISFACTION) &
             !is.na(OVERCOME_BARRIERS) &
             !is.na(IMPROVED_PROBLEMS))
  
  satisfactionRates<-data.frame(matrix(NA,nrow=4,ncol=4))
  colnames(satisfactionRates)<-c("Question","Survey Percentage","Agree/Strongly Agree","Total Surveys")
  
  
  satisfactionRates[1,1]<-"If I meet another veteran who is having a difficult time, I will recommend the AMC Program."
  satisfactionRates[1,2]<-sum((table(filled_satisfaction$RECOMMEND_PROGRAM)/nrow(filled_satisfaction)*100)[names(table(filled_satisfaction$RECOMMEND_PROGRAM)) == "A" | names(table(filled_satisfaction$RECOMMEND_PROGRAM)) == "SA"])
  satisfactionRates[1,3]<-sum(table(filled_satisfaction$RECOMMEND_PROGRAM)[names(table(filled_satisfaction$RECOMMEND_PROGRAM)) == "A" | names(table(filled_satisfaction$RECOMMEND_PROGRAM)) == "SA"])
  satisfactionRates[1,4]<-nrow(filled_satisfaction)
  
  satisfactionRates[2,1]<-"Overall, I feel satisfied by the clinical care I received at the AMC."
  satisfactionRates[2,2]<-sum((table(filled_satisfaction$OVERALL_SATISFACTION)/nrow(filled_satisfaction)*100)[names(table(filled_satisfaction$OVERALL_SATISFACTION)) == "A" | names(table(filled_satisfaction$OVERALL_SATISFACTION)) == "SA"])
  satisfactionRates[2,3]<-sum(table(filled_satisfaction$OVERALL_SATISFACTION)[names(table(filled_satisfaction$OVERALL_SATISFACTION)) == "A" | names(table(filled_satisfaction$OVERALL_SATISFACTION)) == "SA"])
  satisfactionRates[2,4]<-nrow(filled_satisfaction)
  
  satisfactionRates[3,1]<-"The AMC program helped me overcome barriers or obstacles to seek the care I needed, whether at the AMC or somewhere else."
  satisfactionRates[3,2]<-sum((table(filled_satisfaction$OVERCOME_BARRIERS)/nrow(filled_satisfaction)*100)[names(table(filled_satisfaction$OVERCOME_BARRIERS)) == "A" | names(table(filled_satisfaction$OVERCOME_BARRIERS)) == "SA"])
  satisfactionRates[3,3]<-sum(table(filled_satisfaction$OVERCOME_BARRIERS)[names(table(filled_satisfaction$OVERCOME_BARRIERS)) == "A" | names(table(filled_satisfaction$OVERCOME_BARRIERS)) == "SA"])
  satisfactionRates[3,4]<-nrow(filled_satisfaction)
  
  satisfactionRates[4,1]<-"The care I received at AMC has improved the problems I needed help with."
  satisfactionRates[4,2]<-sum((table(filled_satisfaction$IMPROVED_PROBLEMS)/nrow(filled_satisfaction)*100)[names(table(filled_satisfaction$IMPROVED_PROBLEMS)) == "A" | names(table(filled_satisfaction$IMPROVED_PROBLEMS)) == "SA"])
  satisfactionRates[4,3]<-sum(table(filled_satisfaction$IMPROVED_PROBLEMS)[names(table(filled_satisfaction$IMPROVED_PROBLEMS)) == "A" | names(table(filled_satisfaction$IMPROVED_PROBLEMS)) == "SA"])
  satisfactionRates[4,4]<-nrow(filled_satisfaction)
  
  return(satisfactionRates)
}

## 3. Service Utilization Summary
calcServiceUtilization<-function(visits,cycleStart){
  sessionCounts<-visits %>%
    mutate(IOP_START_DATE=as.Date(IOP_START_DATE, format="%Y-%m-%d"),
    ) %>%
    filter(SERVICE_DURATION > 0,
           !(SERVICE_LINE == "IOP" & IOP_START_DATE > cycleStart)) %>%
    group_by(SERVICE_LINE) %>%
    summarise(count=n())
  hourCounts<-visits %>%
    mutate(IOP_START_DATE=as.Date(IOP_START_DATE, format="%Y-%m-%d"),
    ) %>%
    filter(SERVICE_DURATION > 0,
           !(SERVICE_LINE == "IOP" & IOP_START_DATE > cycleStart)) %>%
    group_by(SERVICE_LINE) %>%
    summarise(total_hours=sum(SERVICE_DURATION)/60)
  
  participantCount<-visits %>%
    filter(SERVICE_DURATION > 0,
           !(SERVICE_LINE == "IOP" & IOP_START_DATE > cycleStart)) %>%
    group_by(SERVICE_LINE,PATIENT_ID_NUM) %>%
    summarise(total_participants=n()) %>%
    group_by(SERVICE_LINE) %>%
    summarise(total_participants=n())
  
  averageHours<-hourCounts %>%
    inner_join(participantCount,by=c("SERVICE_LINE")) %>%
    mutate(average_hours=total_hours/total_participants)
  return(list(sessionCounts,hourCounts,averageHours))
}

## 4. completion rate
calcCompletionRate<-function(patients,assessments,visits) {
  
  warriors <- patients %>%
    filter(PATIENT_TYPE=='VET') %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>%
    distinct() %>%
    unlist() %>%
    unname()
  
  warriorsAtBaseline<-assessments %>%
    filter(ASSESSMENT_TERM==0,
           SERVICE_LINE=='IOP',
           PATIENT_ID_NUM %in% warriors) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>% distinct() %>%
    unlist()
  
  warriorsAtEndpoint<-assessments %>%
    filter(ASSESSMENT_TERM==1,
           SERVICE_LINE=='IOP',
           PATIENT_ID_NUM %in% warriors) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>% distinct() %>%
    unlist()
  
  baselineN<-visits %>% 
    filter(PATIENT_ID_NUM %in% warriorsAtBaseline,
           SERVICE_LINE == 'IOP',
           IOP_START_DATE != '1900-01-01',
           SERVICE_DURATION >0) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>% distinct() %>%
    nrow()
  
  endpointN<-visits %>%
    filter(PATIENT_ID_NUM %in% warriorsAtEndpoint,
           SERVICE_LINE == 'IOP',
           IOP_START_DATE != '1900-01-01',
           SERVICE_DURATION >0) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>% distinct() %>%
    nrow()
  
  rate<-endpointN/baselineN *100
  return(rate)
}

## 5. warrior outcomes

extractTimepoint<-function(data,metric,timepoint){
  
  
  
  metricData<-data %>% 
    mutate(ASSESSMENT_DATE=as.Date(ASSESSMENT_DATE, format="%Y-%m-%d")) %>%
    filter(ASSESSMENT_TERM == 1 | ASSESSMENT_TERM == 0,
           !(ASSESSMENT_TERM == 1 & ASSESSMENT_TYPE == 'PCL5'),
           !(ASSESSMENT_TERM == 0 & ASSESSMENT_TYPE == 'PCL5W'),
           ASSESSMENT_TYPE %in% c("PCL5","PCL5W","CDRISC","NSI","PHQ9"),
           SERVICE_LINE == "IOP") %>%
    group_by(PATIENT_ID_NUM,ASSESSMENT_TYPE,ASSESSMENT_TERM) %>%
    slice_max(ASSESSMENT_DATE,n=1,with_ties = F) %>%
    pivot_wider(id_cols=PATIENT_ID_NUM,names_from=c(ASSESSMENT_TYPE,ASSESSMENT_TERM),values_from = starts_with("ASSESSMENT_RESPONSE_VALUE")) %>%
    select(PATIENT_ID_NUM,contains(metric))

  empty_cols<-apply(metricData,2,function(x){sum(is.na(x))})
  metricData<-metricData[,empty_cols != nrow(metricData)]
  metricData<-metricData[complete.cases(metricData),]
  
  timepointData<-metricData %>% 
    select(PATIENT_ID_NUM, contains("_" %&% timepoint))  %>%
    column_to_rownames("PATIENT_ID_NUM") %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    rename_with(.fn=function(x) return('value')) %>%
    rownames_to_column(var="PATIENT_ID_NUM")  %>%
    mutate(timepoint=if_else(timepoint == 1,"Post IOP",'Pre IOP'),
           metric=metric)
  return(timepointData)
}

formatPlotInput<-function(base,post) {
  metricSummary<-rbind.data.frame(base,
                                post) %>%
    group_by(timepoint,metric) %>%
    summarise(mean_score=mean(value),sd_score=sd(value),count=n()) %>%
    mutate(timepoint=factor(timepoint,levels=c("Pre IOP","Post IOP")))
  return(metricSummary)
}


plotOutcome<-function(input,metric) {
  plot<-input %>%
    ggplot(aes(x=timepoint,y=mean_score)) +
    geom_point() +
    geom_text(aes(label=round(mean_score)),position=position_nudge(y=1),size=5) +
    geom_path(group =".") +
    theme_bw(20)  +
    ggtitle(metric %&% " (n=" %&% min(input$count) %&% ")") + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.caption = element_text(hjust=0.5))
  return(plot)
}
### PCL
### PHQ
### CDRISC
### NSI

generateOutcomePlot<-function(data,metric){
  baselineData<-extractTimepoint(data,metric,timepoint=0)
  postData<-extractTimepoint(data,metric,timepoint=1)
  
  plotInput<-formatPlotInput(baselineData,postData)
  
  plot<-plotOutcome(plotInput,metric)
  return(plot)
}


generateWowResults<-function(in.dir,out.dir,cutoffDate) {
  assessments<-fread(in.dir %&% "assessment_2023-01-23.csv",na=c("99","999","NA",""))
  patients<-fread(in.dir %&% "patient_2023-01-23.csv",na=c("99","999","NA",""))
  visits<-fread(in.dir %&% "visit_2023-01-23.csv",na=c("99","999","NA",""))
  referrals<-fread(in.dir %&% "referral_2023-01-23.csv",na=c("99","999","NA",""))
  satisfaction<-fread(in.dir %&% "satisfaction_2023-01-23.csv",na=c("99","999","NA",""))
  masterListServices<-fread("/Users/ryanschubert/Dropbox (Rush)/Ryan's stuff/rush/remake KPI/Master List of therapies.csv",na=c("")) %>%
    filter(!is.na(`Treatment*`)) %>%
    rename(Treatment="Treatment*") %>%
    separate(`Master List of Therapies and Services (# = CDS value)`,into=c("TreatmentID"),sep=" ") %>% 
    mutate(TreatmentID=as.integer(TreatmentID))
  
  cycleStart<-as.Date(format(as.Date(format(cutoffDate, "%Y-%m-01")) -1,"%Y-%m-01"))
  
  ## 1. Warrior Care Network Participants
  barPlot<-plotWCNParticipants(visits,patients,masterListServices)
  
  ## 2. Satisfaction Results All time
  satisfactionTable<-calcSatisfactionRates(satisfaction)
  
  ## 3. Service Utilization Summary
  utilizationSummary<-calcServiceUtilization(visits,cycleStart)
  
  ## 4. completion rate
  completionRate<-calcCompletionRate(patients,assessments,visits)
  
  ## 5. warrior outcomes
  pclPlot<-generateOutcomePlot(assessments,metric='PCL')
  phqPlot<-generateOutcomePlot(assessments,metric='PHQ')
  cdriscPlot<-generateOutcomePlot(assessments,metric='CDRISC')
  nsiPlot<-generateOutcomePlot(assessments,metric='NSI')
  
  outputList<-list(satisfaction=satisfactionTable,
                   sessions=utilizationSummary[[1]],
                   hours=utilizationSummary[[2]],
                   average_hours=utilizationSummary[[3]],
                   completion=as.data.frame(completionRate))
  
  write_xlsx(outputList,out.dir %&% 'WOW_crosssite_report_' %&% cutoffDate %&% '.xlsx')
  ggsave(out.dir %&% 'iop_counts_bar_' %&% cutoffDate %&% '.png',plot=barPlot)
  ggsave(out.dir %&% 'pcl_plot_' %&% cutoffDate %&% '.png',plot=pclPlot)
  ggsave(out.dir %&% 'cdrisc_plot_' %&% cutoffDate %&% '.png',plot=cdriscPlot)
  ggsave(out.dir %&% 'phq_plot_' %&% cutoffDate %&% '.png',plot=phqPlot)
  ggsave(out.dir %&% 'nsi_plot_' %&% cutoffDate %&% '.png',plot=nsiPlot)
}