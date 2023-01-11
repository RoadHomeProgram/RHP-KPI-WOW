library(data.table)
library(naniar)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readxl)
library(writexl)
"%&%" = function(a,b) paste0(a,b)

source("/Users/ryanschubert/Dropbox (Rush)/Ryan's stuff/rush/remake KPI/prototype/dateHelpers.R")

generateWOW<-function(in.dir,masterListFile,out.dir,reportingPeriod,cutoffDate=today) {
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
  
  ## 1. Warrior Care Network Participants
  FYStart<-determineFYStart(cutoffDate)
  FY<-determineFiscalYear(cutoffDate)
  quarters<-createQuarterTemplate(cutoffDate)
  cycleStart<-as.Date(format(as.Date(format(cutoffDate, "%Y-%m-01")) -1,"%Y-%m-01"))
  
  vetsOnly<-patients %>%
    filter(PATIENT_TYPE=="VET")
  preprocessVisits<-visits %>%
    mutate(IOP_START_DATE=as.Date(IOP_START_DATE, format="%Y-%m-%d"),
           SERVICE_DATE=as.Date(SERVICE_DATE, format="%Y-%m-%d")) %>%
    filter(!(SERVICE_LINE == "IOP" & !(PATIENT_ID_NUM %in% vetsOnly$PATIENT_ID_NUM)),
           SERVICE_PERFORMED %in% master_list_services$TreatmentID,
           VISIT_DURATION > 0,
           IOP_START_DATE < cycleStart)
  with_grouping<-preprocessVisits %>%
    group_by(SERVICE_LINE,PATIENT_ID_NUM) %>%
    summarise(count=n()) %>%
    ungroup() %>%
    group_by(PATIENT_ID_NUM) %>%
    summarise(count=n()) %>%
    ungroup %>%
    mutate(both=count==2) %>%
    inner_join(preprocessVisits,by=c("PATIENT_ID_NUM")) %>%
    mutate(category=if_else(both,"IOP/OP",SERVICE_LINE),
           currentFY=if_else(SERVICE_DATE > FYStart,"FY " %&% FY,"06-1-2015 - " %&% FYStart)) %>%
    select(PATIENT_ID_NUM,category,currentFY) %>%
    arrange() %>%
    distinct()
  pBar<-with_grouping %>%
    mutate(category="Total") %>%
    rbind.data.frame(with_grouping) %>%
    mutate(colour=if_else(category=="Total","Red","Blue"),
           category=factor(category,levels=c("OP","IOP","IOP/OP","Total"))) %>%
    ggplot(aes(x=category)) + 
    geom_bar(aes(fill=colour)) +
    facet_wrap(~currentFY,scales="free") +
    geom_text(stat='count', aes(label=..count..), vjust=1.5) +
    theme_bw() +
    theme(legend.position = "none")
  # pBar

  ## 2. Satisfaction Results All time
  filled_satisfaction<-satisfaction %>%
    filter(SERVICE_LINE == "IOP",
           !is.na(RECOMMEND_PROGRAM) &
             !is.na(OVERALL_SATISFACTION) &
             !is.na(OVERCOME_BARRIERS) &
             !is.na(IMPROVED_PROBLEMS))
  #sum(table(filled_satisfaction$SURVEY_ID_NUM)> 1)
  
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
  
  satisfactionRates[3,1]<-"The AMC program helped me overcome barriersor obstacles to seek the care I needed, whether at the AMC or somewhere else."
  satisfactionRates[3,2]<-sum((table(filled_satisfaction$OVERCOME_BARRIERS)/nrow(filled_satisfaction)*100)[names(table(filled_satisfaction$OVERCOME_BARRIERS)) == "A" | names(table(filled_satisfaction$OVERCOME_BARRIERS)) == "SA"])
  satisfactionRates[3,3]<-sum(table(filled_satisfaction$OVERCOME_BARRIERS)[names(table(filled_satisfaction$OVERCOME_BARRIERS)) == "A" | names(table(filled_satisfaction$OVERCOME_BARRIERS)) == "SA"])
  satisfactionRates[3,4]<-nrow(filled_satisfaction)
  
  satisfactionRates[4,1]<-"Overall, I feel satisfied by the clinical care I received at the AMC."
  satisfactionRates[4,2]<-sum((table(filled_satisfaction$IMPROVED_PROBLEMS)/nrow(filled_satisfaction)*100)[names(table(filled_satisfaction$IMPROVED_PROBLEMS)) == "A" | names(table(filled_satisfaction$IMPROVED_PROBLEMS)) == "SA"])
  satisfactionRates[4,3]<-sum(table(filled_satisfaction$IMPROVED_PROBLEMS)[names(table(filled_satisfaction$IMPROVED_PROBLEMS)) == "A" | names(table(filled_satisfaction$IMPROVED_PROBLEMS)) == "SA"])
  satisfactionRates[4,4]<-nrow(filled_satisfaction)
  
  ## 3. Service Utilization Summary
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
  ## completion rate
  n1<-visits %>%
    mutate(IOP_START_DATE=as.Date(IOP_START_DATE, format="%Y-%m-%d"),
    ) %>%
    filter(SERVICE_DURATION > 0,
           SERVICE_LINE == "IOP",
           IOP_START_DATE < cycleStart) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>%
    distinct() %>% nrow()
  
  
  n2<-assessments %>%
    filter(SERVICE_LINE=="IOP",
           ASSESSMENT_TERM %in% c(1,9001,9002,9003)) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>%
    distinct() %>% nrow()
  
  completionRate<-n2/n1*100
  
  ## warrior outcomes
  
  tmp<-assessments %>%
    mutate(ASSESSMENT_DATE=as.Date(ASSESSMENT_DATE, format="%Y-%m-%d")) %>%
    filter(ASSESSMENT_TERM == 1 | ASSESSMENT_TERM == 0,
           ASSESSMENT_TYPE %in% c("PCL5","PCL5W","CDRISC","NSI","PHQ9"),
           SERVICE_LINE == "IOP") %>%
    group_by(PATIENT_ID_NUM,ASSESSMENT_TYPE,ASSESSMENT_TERM) %>%
    slice_max(ASSESSMENT_DATE,n=1,with_ties = F) %>%
    pivot_wider(id_cols=PATIENT_ID_NUM,names_from=c(ASSESSMENT_TYPE,ASSESSMENT_TERM),values_from = starts_with("ASSESSMENT_RESPONSE_VALUE"))
  
  empty_cols<-apply(tmp,2,function(x){sum(is.na(x))})
  
  tmp<-tmp[,empty_cols != nrow(tmp)]
  #now for
  sum(table(tmp$PATIENT_ID_NUM) > 1)
  
  ## PCL-5 outcome
  
  pcl_data<-tmp %>% 
    select(PATIENT_ID_NUM,contains("PCL5")) %>%
    select(-ends_with('PCL5_1')) %>%
    select(-ends_with('PCL5W_0'))
  vis_miss(pcl_data)
  pcl_data<-pcl_data[complete.cases(pcl_data),]
  
  base_pcl<-pcl_data %>% 
    select(PATIENT_ID_NUM, contains("PCL5_0"))  %>%
    column_to_rownames("PATIENT_ID_NUM") %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    rename_with(.fn=function(x) return("PCL5")) %>%
    rownames_to_column(var="PATIENT_ID_NUM")  %>%
    mutate(timepoint="Pre IOP")
  post_pcl<-pcl_data %>% 
    select(PATIENT_ID_NUM, contains("PCL5W_1"))  %>%
    column_to_rownames("PATIENT_ID_NUM") %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    rename_with(.fn=function(x) return("PCL5")) %>%
    rownames_to_column(var="PATIENT_ID_NUM")  %>%
    mutate(timepoint="Post IOP")
  
  pcl_summary<-rbind.data.frame(base_pcl,
                                post_pcl) %>%
    group_by(timepoint) %>%
    summarise(mean_score=mean(PCL5,na.rm=T),sd_score=sd(PCL5,na.rm=T),count=n()) %>%
    mutate(timepoint=factor(timepoint,levels=c("Pre IOP","Post IOP")))
  pcl_plot<-pcl_summary %>%
    ggplot(aes(x=timepoint,y=mean_score)) +
    geom_point() +
    geom_text(aes(label=round(mean_score)),position=position_nudge(y=5)) +
    geom_path(group =".") +
    theme_bw(20) + 
    ylim(0,80) +
    ggtitle("PCL-5 (n=" %&% min(pcl_summary$count) %&% ")") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.caption = element_text(hjust=0.5)) +
    labs(caption="0-18: Minimal, 19-36: Mild, 37-49: Moderate, 50-80: Severe")
  pcl_plot
  
  ## CD-RISC outcomes
  
  
  CDRISC_data<-tmp %>% 
    select(PATIENT_ID_NUM,contains("CDRISC"))
  # vis_miss(CDRISC_data)
  CDRISC_data<-CDRISC_data[complete.cases(CDRISC_data),]
  
  base_CDRISC<-CDRISC_data %>% 
    select(PATIENT_ID_NUM, contains("_0"))  %>%
    column_to_rownames("PATIENT_ID_NUM") %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    rename_with(.fn=function(x) return("CDRISC")) %>%
    rownames_to_column(var="PATIENT_ID_NUM")  %>%
    mutate(timepoint="Pre IOP")
  post_CDRISC<-CDRISC_data %>% 
    select(PATIENT_ID_NUM, contains("_1"))  %>%
    column_to_rownames("PATIENT_ID_NUM") %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    rename_with(.fn=function(x) return("CDRISC")) %>%
    rownames_to_column(var="PATIENT_ID_NUM")  %>%
    mutate(timepoint="Post IOP")
  
  CDRISC_summary<-rbind.data.frame(base_CDRISC,
                                   post_CDRISC) %>%
    filter(!is.na(CDRISC)) %>%
    group_by(timepoint) %>%
    summarise(mean_score=mean(CDRISC),sd_score=sd(CDRISC),count=n()) %>%
    mutate(timepoint=factor(timepoint,levels=c("Pre IOP","Post IOP")))
  cdrisc_plot<-CDRISC_summary %>%
    ggplot(aes(x=timepoint,y=mean_score)) +
    geom_point() +
    geom_text(aes(label=round(mean_score)),position=position_nudge(y=1), size=5) +
    geom_path(group =".") +
    theme_bw(20) +
    ylim(18,24) +
    ylab("CD-RISC Score") + 
    ggtitle("CD-RISC (n=" %&% min(CDRISC_summary$count) %&% ")") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.caption = element_text(hjust=0.5)) +
    labs(caption="Higher Scores = Greater Resiliency")
  cdrisc_plot

  PHQ_data<-tmp %>% 
    select(PATIENT_ID_NUM,contains("PHQ9"))
  vis_miss(PHQ_data)
  PHQ_data<-PHQ_data[complete.cases(PHQ_data),]
  
  base_PHQ<-PHQ_data %>% 
    select(PATIENT_ID_NUM, contains("_0"))  %>%
    column_to_rownames("PATIENT_ID_NUM") %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    rename_with(.fn=function(x) return("PHQ9")) %>%
    rownames_to_column(var="PATIENT_ID_NUM")  %>%
    mutate(timepoint="Pre IOP")
  post_PHQ<-PHQ_data %>% 
    select(PATIENT_ID_NUM, contains("_1"))  %>%
    column_to_rownames("PATIENT_ID_NUM") %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    rename_with(.fn=function(x) return("PHQ9")) %>%
    rownames_to_column(var="PATIENT_ID_NUM")  %>%
    mutate(timepoint="Post IOP")
  
  PHQ_summary<-rbind.data.frame(base_PHQ,
                                post_PHQ) %>%
    filter(!is.na(PHQ9)) %>%
    group_by(timepoint) %>%
    summarise(mean_score=mean(PHQ9),sd_score=sd(PHQ9),count=n()) %>%
    mutate(timepoint=factor(timepoint,levels=c("Pre IOP","Post IOP")))
  phq_plot<-PHQ_summary %>%
    ggplot(aes(x=timepoint,y=mean_score)) +
    geom_point() +
    geom_text(aes(label=round(mean_score)),position=position_nudge(y=3),size=5) +
    geom_path(group =".") +
    theme_bw(20)  +
    ylim(0,24) +
    ggtitle("PHQ-9 (n=" %&% min(CDRISC_summary$count) %&% ")") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.caption = element_text(hjust=0.5,size = 10)) +
    labs(caption="1-4: Minimal, 5-9: Mild, 10-14: Moderate, 15-19: Mod Severe, 20-27: Severe")  
  phq_plot  
  
  
  NSI_data<-tmp %>% 
    select(PATIENT_ID_NUM,contains("NSI"))
  vis_miss(NSI_data)
  NSI_data<-NSI_data[complete.cases(NSI_data),]
  
  base_NSI<-NSI_data %>% 
    select(PATIENT_ID_NUM, contains("_0"))  %>%
    column_to_rownames("PATIENT_ID_NUM") %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    rename_with(.fn=function(x) return("NSI")) %>%
    rownames_to_column(var="PATIENT_ID_NUM")  %>%
    mutate(timepoint="Pre IOP")
  post_NSI<-NSI_data %>% 
    select(PATIENT_ID_NUM, contains("_1"))  %>%
    column_to_rownames("PATIENT_ID_NUM") %>%
    apply(.,1,sum) %>%
    as.data.frame() %>%
    rename_with(.fn=function(x) return("NSI")) %>%
    rownames_to_column(var="PATIENT_ID_NUM")  %>%
    mutate(timepoint="Post IOP")
  
  NSI_summary<-rbind.data.frame(base_NSI,
                                post_NSI) %>%
    group_by(timepoint) %>%
    summarise(mean_score=mean(NSI),sd_score=sd(NSI),count=n()) %>%
    mutate(timepoint=factor(timepoint,levels=c("Pre IOP","Post IOP")))
  nsi_plot<-NSI_summary %>%
    ggplot(aes(x=timepoint,y=mean_score)) +
    geom_point() +
    geom_text(aes(label=round(mean_score)),position=position_nudge(y=5),size=5) +
    geom_path(group =".") +
    theme_bw(20)  +
    ylim(0,60) +
    ggtitle("NSI (n=" %&% min(NSI_summary$count) %&% ")") + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.caption = element_text(hjust=0.5)) +
    labs(caption="0-18: Minimal, 19-36: Mild, 37-49: Moderate, 50-80: Severe")
  
  outputList<-list(satisfaction=satisfactionRates,
                   sessions=sessionCounts,
                   hours=hourCounts,
                   average_hours=averageHours,
                   completion=as.data.frame(completionRate))
  
  write_xlsx(outputList,out.dir %&% 'WOW_crosssite_report_' %&% cutoffDate %&% '.xlsx')
  ggsave(out.dir %&% 'iop_counts_bar_' %&% cutoffDate %&% '.png',plot=pBar)
  ggsave(out.dir %&% 'pcl_plot_' %&% cutoffDate %&% '.png',plot=pcl_plot)
  ggsave(out.dir %&% 'cdrisc_plot_' %&% cutoffDate %&% '.png',plot=cdrisc_plot)
  ggsave(out.dir %&% 'phq_plot_' %&% cutoffDate %&% '.png',plot=phq_plot)
  ggsave(out.dir %&% 'nsi_plot_' %&% cutoffDate %&% '.png',plot=nsi_plot)
  
}