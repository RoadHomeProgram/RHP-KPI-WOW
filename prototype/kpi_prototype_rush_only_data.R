# load in libraries
library(data.table)
library(naniar)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

# string concat function
"%&%" = function(a,b) paste0(a,b)

#define global vars with data
dir<-"/Users/ryanschubert/Dropbox (Rush)/Ryan's stuff/rush/remake KPI/"
assessments<-fread(dir %&% "assessment_data_202207.txt",na=c("99","999"))
patients<-fread(dir %&% "patient_data_202207.txt",na=c("99","999"))
visits<-fread(dir %&% "visit_data_202207.txt",na=c("99","999"))
referrals<-fread(dir %&% "referral_data_202207.txt",na=c("99","999"))
satisfaction<-fread(dir %&% "satisfaction_data_202207.txt",na=c("99","999"))
master_list_services<-fread(dir %&% "Master List of therapies.csv",na=c("")) %>%
  filter(!is.na(`Treatment*`)) %>%
  rename(Treatment="Treatment*") %>%
  separate(`Master List of Therapies and Services (# = CDS value)`,into=c("TreatmentID"),sep=" ") %>% 
  mutate(TreatmentID=as.integer(TreatmentID))


#generate target output
timestamp<-Sys.time() %>% gsub(" ","_",.)
target.out<-dir %&% "WCN_KPI_report" %&% timestamp %&% ".xlsx"
questions<-c(
  "% of participants that reduce their PCL score by 5 points or more",
  "% of participants that reduce their PCL score by 10 points or more",
  "% of participants that reduce their PHQ-9 score by 3 points or more",
  "% of participants that improved by 2 points or more on CD-RISC",
  "% of Mental Component Score participants that improved by 2 points or more on VR-12",
  "% of Physical Component Score participants that improved by 2 points or more on VR-12",
  "% of Survey participants who overall felt satisfied with the clinical care received at AMC"
)

######
#table 1 starts here
######

table1<-data.frame(question=questions,
                   value=rep(NA,7))
## 1.	% of participants that reduce their PCL score by 5 points or more

visits$PATIENT_ID_NUM
vets_with_treatment<-visits %>%
  filter(SERVICE_PERFORMED %in% master_list_services$TreatmentID) %>%
  select(PATIENT_ID_NUM) %>%
  arrange() %>%
  distinct() 

PCLVetFAM<-assessments %>%
  filter(grepl("PCL",ASSESSMENT_TYPE,ignore.case=T),
         SERVICE_LINE=="IOP",
         ASSESSMENT_TERM == 1 | ASSESSMENT_TERM ==0,
         PATIENT_ID_NUM %in% vets_with_treatment$PATIENT_ID_NUM) %>%
  select(-one_of(c("ASSESSMENT_RESPONSE_VALUE21","ASSESSMENT_RESPONSE_VALUE22"))) %>%
  filter ((ASSESSMENT_TYPE == "PCL5" & ASSESSMENT_TERM == 0) | (ASSESSMENT_TYPE == "PCL5W" & ASSESSMENT_TERM == 1)) %>%
  mutate(ASSESSMENT_DATE=as.Date(ASSESSMENT_DATE, format="%m/%d/%Y"),
         inFiscalYear=ASSESSMENT_DATE>"2021-10-01") %>%
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

#filters to 
PCLVetFAM<-PCLVetFAM %>%
  mutate(PATIENT_ID_NUM=as.character(PATIENT_ID_NUM)) %>%
  filter(!(PATIENT_ID_NUM  %in% removalFreq))

#gets the list of vets with a post treatment assessment within the current fiscal yer
inYear<-PCLVetFAM %>%
  filter(ASSESSMENT_TERM == 1,inFiscalYear) %>%
  select(PATIENT_ID_NUM) %>%
  arrange() %>%
  distinct() %>%
  unlist() %>% 
  unname()

#filters to vets in the fiscal year
PCLVetFAM<-PCLVetFAM %>%
  filter(PATIENT_ID_NUM  %in% inYear)




sum_scores<-PCLVetFAM %>% 
  select(contains("ASSESSMENT_RESPONSE_VALUE")) %>%
  apply(1,sum)
PCLVetFAM<-cbind.data.frame(PCLVetFAM,sum_scores)
sanitized_PCL<-PCLVetFAM %>%
  select(PATIENT_ID_NUM,ASSESSMENT_TYPE,sum_scores) %>%
  pivot_wider(id_cols=PATIENT_ID_NUM,
              names_from=ASSESSMENT_TYPE,
              values_from = sum_scores) %>%
  mutate(deltaPCL=PCL5-PCL5W,
         meetsThreshold=deltaPCL>=5)
table(sanitized_PCL$meetsThreshold)
res1<-table(sanitized_PCL$meetsThreshold)/nrow(sanitized_PCL) * 100 
table1[1,2]<-res1[2]

## 2.	% of participants that reduce their PCL score by 10 points or more

vetOnly<-patients %>%
  filter(PATIENT_TYPE == "VET",
         PATIENT_ID_NUM %in% vets_with_treatment$PATIENT_ID_NUM)  %>%
  mutate(PATIENT_ID_NUM=as.character(PATIENT_ID_NUM)) %>%
  select(PATIENT_ID_NUM) %>%
  arrange() %>%
  distinct() %>%
  unlist() %>%
  unname()

PCLVet<-PCLVetFAM %>%
  filter(PATIENT_ID_NUM %in% vetOnly)

sanitized_PCL_Vet<-PCLVet %>%
  select(PATIENT_ID_NUM,ASSESSMENT_TYPE,sum_scores) %>%
  pivot_wider(id_cols=PATIENT_ID_NUM,
              names_from=ASSESSMENT_TYPE,
              values_from = sum_scores) %>%
  mutate(deltaPCL=PCL5-PCL5W,
         meetsThreshold=deltaPCL>=10)
table(sanitized_PCL_Vet$meetsThreshold)
res2<-table(sanitized_PCL_Vet$meetsThreshold)/nrow(sanitized_PCL_Vet) * 100
table1[2,2]<-res2[2]

## 3.	% of participants that reduce their PHQ-9 score by 3 points or more

PHQVetFAM<-assessments %>%
  filter(grepl("PHQ9",ASSESSMENT_TYPE,ignore.case=T),
         SERVICE_LINE=="IOP",
         ASSESSMENT_TERM == 1 | ASSESSMENT_TERM ==0,
         PATIENT_ID_NUM %in% vets_with_treatment$PATIENT_ID_NUM) %>%
  select(-one_of("ASSESSMENT_RESPONSE_VALUE" %&% 10:22)) %>%
  filter ((ASSESSMENT_TERM == 0) | (ASSESSMENT_TERM == 1)) %>%
  mutate(ASSESSMENT_DATE=as.Date(ASSESSMENT_DATE, format="%m/%d/%Y"),
         inFiscalYear=ASSESSMENT_DATE>"2021-10-01") %>%
  group_by(PATIENT_ID_NUM,ASSESSMENT_TERM) %>%
  slice_max(ASSESSMENT_DATE,n=1) %>%
  ungroup()

removalFreq <-table(PHQVetFAM$PATIENT_ID_NUM,PHQVetFAM$ASSESSMENT_TERM) %>%
  as.data.frame() %>%
  filter(Freq!=1) %>%
  select(Var1) %>%
  mutate(Var1=as.character(Var1)) %>%
  arrange() %>%
  distinct() %>%
  unlist() %>% 
  unname()

PHQVetFAM<-PHQVetFAM %>%
  mutate(PATIENT_ID_NUM=as.character(PATIENT_ID_NUM)) %>%
  filter(!(PATIENT_ID_NUM  %in% removalFreq))

inYear<-PHQVetFAM %>%
  filter(ASSESSMENT_TERM == 1,inFiscalYear) %>%
  select(PATIENT_ID_NUM) %>%
  arrange() %>%
  distinct() %>%
  unlist() %>% 
  unname()

PHQVetFAM<-PHQVetFAM %>%
  filter(PATIENT_ID_NUM  %in% inYear)

sum_scores<-PHQVetFAM %>% 
  select(contains("ASSESSMENT_RESPONSE_VALUE")) %>%
  apply(1,sum)
PHQVetFAM<-cbind.data.frame(PHQVetFAM,sum_scores)
sanitized_PHQ<-PHQVetFAM %>%
  select(PATIENT_ID_NUM,ASSESSMENT_TERM,sum_scores) %>%
  pivot_wider(id_cols=PATIENT_ID_NUM,
              names_from=ASSESSMENT_TERM,
              values_from = sum_scores) %>%
  mutate(deltaPHQ=`0`-`1`,
         meetsThreshold=deltaPHQ>=3)
table(sanitized_PHQ$meetsThreshold)
res3<-table(sanitized_PHQ$meetsThreshold)/nrow(sanitized_PHQ) * 100
table1[3,4]<-res3[2]

## 4.	% of participants that improved by 2 points or more on CD-RISC

CDRISCVetFAM<-assessments %>%
  filter(grepl("CDRISC",ASSESSMENT_TYPE,ignore.case=T),
         SERVICE_LINE=="IOP",
         ASSESSMENT_TERM == 1 | ASSESSMENT_TERM ==0,
         PATIENT_ID_NUM %in% vets_with_treatment$PATIENT_ID_NUM) %>%
  select(-one_of("ASSESSMENT_RESPONSE_VALUE" %&% 11:22)) %>%
  filter ((ASSESSMENT_TERM == 0) | (ASSESSMENT_TERM == 1)) %>%
  mutate(ASSESSMENT_DATE=as.Date(ASSESSMENT_DATE, format="%m/%d/%Y"),
         inFiscalYear=ASSESSMENT_DATE>"2021-10-01") %>%
  group_by(PATIENT_ID_NUM,ASSESSMENT_TERM) %>%
  slice_max(ASSESSMENT_DATE,n=1) %>%
  ungroup()

removalFreq <-table(CDRISCVetFAM$PATIENT_ID_NUM,CDRISCVetFAM$ASSESSMENT_TERM) %>%
  as.data.frame() %>%
  filter(Freq!=1) %>%
  select(Var1) %>%
  mutate(Var1=as.character(Var1)) %>%
  arrange() %>%
  distinct() %>%
  unlist() %>% 
  unname()

CDRISCVetFAM<-CDRISCVetFAM %>%
  mutate(PATIENT_ID_NUM=as.character(PATIENT_ID_NUM)) %>%
  filter(!(PATIENT_ID_NUM  %in% removalFreq))

inYear<-CDRISCVetFAM %>%
  filter(ASSESSMENT_TERM == 1,inFiscalYear) %>%
  select(PATIENT_ID_NUM) %>%
  arrange() %>%
  distinct() %>%
  unlist() %>% 
  unname()

CDRISCVetFAM<-CDRISCVetFAM %>%
  filter(PATIENT_ID_NUM  %in% inYear)

sum_scores<-CDRISCVetFAM %>% 
  select(contains("ASSESSMENT_RESPONSE_VALUE")) %>%
  apply(1,sum)
CDRISCVetFAM<-cbind.data.frame(CDRISCVetFAM,sum_scores)
sanitized_CDRISC<-CDRISCVetFAM %>%
  select(PATIENT_ID_NUM,ASSESSMENT_TERM,sum_scores) %>%
  pivot_wider(id_cols=PATIENT_ID_NUM,
              names_from=ASSESSMENT_TERM,
              values_from = sum_scores) %>%
  mutate(deltaCDRISC=`0`-`1`,
         meetsThreshold=deltaCDRISC<2)
table(sanitized_CDRISC$meetsThreshold)
res4<-table(sanitized_CDRISC$meetsThreshold)/nrow(sanitized_CDRISC) * 100
table1[4,2]<-res4[2]

## 5.	% of Mental / Physical Component Score participants that improved by 2 points or more on VR-12


VARANDVetFAM<-assessments %>%
  filter(grepl("VARAND",ASSESSMENT_TYPE,ignore.case=T),
         SERVICE_LINE=="IOP",
         ASSESSMENT_TERM == 9001 | ASSESSMENT_TERM ==0,
         PATIENT_ID_NUM %in% vets_with_treatment$PATIENT_ID_NUM) %>%
  select(-one_of("ASSESSMENT_RESPONSE_VALUE" %&% 21:22)) %>%
  filter ((ASSESSMENT_TERM == 0) | (ASSESSMENT_TERM == 9001)) %>%
  mutate(ASSESSMENT_DATE=as.Date(ASSESSMENT_DATE, format="%m/%d/%Y"),
         inFiscalYear=ASSESSMENT_DATE>"2021-10-01") %>%
  group_by(PATIENT_ID_NUM,ASSESSMENT_TERM) %>%
  slice_max(ASSESSMENT_DATE,n=1) %>%
  ungroup()

removalFreq <-table(VARANDVetFAM$PATIENT_ID_NUM,VARANDVetFAM$ASSESSMENT_TERM) %>%
  as.data.frame() %>%
  filter(Freq!=1) %>%
  select(Var1) %>%
  mutate(Var1=as.character(Var1)) %>%
  arrange() %>%
  distinct() %>%
  unlist() %>% 
  unname()

VARANDVetFAM<-VARANDVetFAM %>%
  mutate(PATIENT_ID_NUM=as.character(PATIENT_ID_NUM)) %>%
  filter(!(PATIENT_ID_NUM  %in% removalFreq))

inYear<-VARANDVetFAM %>%
  filter(ASSESSMENT_TERM == 9001,inFiscalYear) %>%
  select(PATIENT_ID_NUM) %>%
  arrange() %>%
  distinct() %>%
  unlist() %>% 
  unname()

VARANDVetFAM<-VARANDVetFAM %>%
  filter(PATIENT_ID_NUM  %in% inYear)

scoring.dir<-"/Users/ryanschubert/Dropbox/Ryan's stuff/rush/VR12/actual data/"
cleanNames<-c("mrn",
              "vr1",
              "vr2a",
              "vr2b",
              "vr3a",
              "vr3b",
              "vr4a",
              "vr4b",
              "vr5",
              "vr6a",
              "vr6b",
              "vr6c",
              "vr7",
              "vr8",
              "vr9",
              "vrplus1",
              "vrplus1a",
              "vrplus1b",
              "vrplus2",
              "vrplus3",
              "vrplus4",
              "Survey")
prescore<-VARANDVetFAM %>%
  mutate(Survey="Mail", PAT_ID = PATIENT_ID_NUM %&% "_" %&% ASSESSMENT_TERM) %>%
  select(PAT_ID,starts_with("ASSESSMENT_RESPONSE_VALUE"),Survey)

colnames(prescore)<-cleanNames
fwrite(prescore,dir %&% "Prescore_VR12.csv")
source(scoring.dir %&% "R ScoringVR12score_vrData.R")

vr12score(file.in=dir %&% "Prescore_VR12.csv",
          file.out=dir %&% "Mail_scored_VR12_out.csv",
          keyfilepath=scoring.dir)


VR12ScoredData<-fread(dir %&% "Mail_scored_VR12_out.csv") 
sanitized_VARAND<-VR12ScoredData %>%
  select(mrn,MCS,PCS) %>%
  separate(mrn,into=c("PATIENT_ID_NUMBER","ASSESSMENT_TERM")) %>%
  pivot_wider(id_col=PATIENT_ID_NUMBER,
              names_from = ASSESSMENT_TERM,
              values_from=c(MCS,PCS)) %>%
  mutate(deltaMCS=MCS_9001-MCS_0,
         deltaPCS=PCS_9001-PCS_0,
         meetsThresholdMCS=deltaMCS>1.4,
         meetsThresholdPCS=deltaPCS>1.4)

table(sanitized_VARAND$meetsThresholdMCS)
res5<-table(sanitized_VARAND$meetsThresholdMCS)/nrow(sanitized_VARAND) * 100
table1[5,2]<-res5[2]

table(sanitized_VARAND$meetsThresholdPCS)
res6<-table(sanitized_VARAND$meetsThresholdPCS)/nrow(sanitized_VARAND) * 100
table1[6,2]<-res6[2]

## 6.	% of Survey participants who overall felt satisfied with the clinical care received at AMC


withResponse<-satisfaction %>%
  mutate(SURVEY_DATE=as.Date(SURVEY_DATE,"%Y-%m-%d")) %>%
  filter(!is.na(OVERALL_SATISFACTION),
         SERVICE_LINE == "IOP",
         SURVEY_DATE>"2021-10-01") #1301
positiveResponse<-filter(withResponse,OVERALL_SATISFACTION == "A" | OVERALL_SATISFACTION == "SA")
res7<-nrow(positiveResponse)/nrow(withResponse) * 100
table1[7,2]<-res7[2]




#########
#table 2 starts here
#########

## 7.	WCN Program Engagements by Rolling 4 Quarters

patientType<-patients %>%
  select(PATIENT_ID_NUM,PATIENT_TYPE)

visits_quarterly_summary<-visits %>%
  mutate(SERVICE_DATE=as.Date(SERVICE_DATE,"%Y-%m-%d"),
         quarter=case_when(
           "2021-10-01" > SERVICE_DATE & SERVICE_DATE >="2021-07-01" ~ "21-Q4",
           "2022-01-01" > SERVICE_DATE & SERVICE_DATE  >="2021-10-01" ~ "22-Q1",
           "2022-04-01" > SERVICE_DATE & SERVICE_DATE  >="2022-01-01" ~ "22-Q2",
           "2022-07-01" > SERVICE_DATE & SERVICE_DATE  >="2022-04-01" ~ "22-Q3",
           SERVICE_DATE >="2022-07-01" ~ "22-Q4",
           T ~ NA_character_
         )) %>%
  filter(!is.na(quarter),
         SERVICE_DURATION > 0) %>%
  left_join(patientType,by=c("PATIENT_ID_NUM")) %>%
  group_by(quarter,SERVICE_LINE,PATIENT_TYPE) %>%
  count()

table2<-visits_quarterly_summary

## 8.	Total IOP & OP Hours - Last Rolling 4 Quarters

visits_hours_summary<-visits %>%
  mutate(SERVICE_DATE=as.Date(SERVICE_DATE,"%Y-%m-%d")) %>%
  filter(SERVICE_DATE >= "2021-07-01",
         SERVICE_DURATION > 0) %>%
  left_join(patientType,by=c("PATIENT_ID_NUM"))

tmp1<-visits_hours_summary %>%
  group_by(SERVICE_LINE,PATIENT_TYPE) %>%
  summarise(Total_Patients=n_distinct(PATIENT_ID_NUM),
            Total_Hours=sum(SERVICE_DURATION)/60,
            Total_Visits=n()) %>%
  mutate(Avg_Sessions=Total_Visits/Total_Patients,
         Avg_Hours=Total_Hours/Total_Patients)
tmp2<-visits_hours_summary %>%
  group_by(PATIENT_ID_NUM,SERVICE_LINE,PATIENT_TYPE) %>%
  summarise(total_hours=sum(SERVICE_DURATION)/60,
            Total_visits=n()) %>%
  ungroup() %>%
  group_by(SERVICE_LINE,PATIENT_TYPE) %>%
  summarise(median_hours=median(total_hours),
            median_visits=median(Total_visits))
table3<-left_join(tmp1,tmp2,by=c("SERVICE_LINE","PATIENT_TYPE"))

## 9.	IOP & OP Hours by Rolling 4 Quarters

visits_hours_summary<-visits %>%
  mutate(SERVICE_DATE=as.Date(SERVICE_DATE,"%Y-%m-%d"),
         quarter=case_when(
           "2021-10-01" > SERVICE_DATE & SERVICE_DATE >="2021-07-01" ~ "21-Q4",
           "2022-01-01" > SERVICE_DATE & SERVICE_DATE  >="2021-10-01" ~ "22-Q1",
           "2022-04-01" > SERVICE_DATE & SERVICE_DATE  >="2022-01-01" ~ "22-Q2",
           "2022-07-01" > SERVICE_DATE & SERVICE_DATE  >="2022-04-01" ~ "22-Q3",
           SERVICE_DATE >="2022-07-01" ~ "22-Q4",
           T ~ NA_character_
         )) %>%
  filter(!is.na(quarter),
         SERVICE_DATE >= "2021-07-01",
         SERVICE_DURATION > 0) %>%
  left_join(patientType,by=c("PATIENT_ID_NUM"))

tmp1<-visits_hours_summary %>%
  group_by(quarter,SERVICE_LINE,PATIENT_TYPE) %>%
  summarise(Total_Patients=n_distinct(PATIENT_ID_NUM),
            Total_Hours=sum(SERVICE_DURATION)/60,
            Total_Visits=n()) %>%
  mutate(Avg_Sessions=Total_Visits/Total_Patients,
         Avg_Hours=Total_Hours/Total_Patients)
tmp2<-visits_hours_summary %>%
  group_by(quarter,PATIENT_ID_NUM,SERVICE_LINE,PATIENT_TYPE) %>%
  summarise(total_hours=sum(SERVICE_DURATION)/60,
            Total_visits=n()) %>%
  ungroup() %>%
  group_by(quarter,SERVICE_LINE,PATIENT_TYPE) %>%
  summarise(median_hours=median(total_hours),
            median_visits=median(Total_visits))
table4_5<-left_join(tmp1,tmp2,by=c("quarter","SERVICE_LINE","PATIENT_TYPE"))

## 10.	% of WWP Referred Accepted

vets<-patients %>% 
  filter(PATIENT_TYPE == "VET",
         ACCEPTED_FLAG %in% c("N","NR","Y"))

referral_processed<-referrals %>% filter(DATA_VALUE_TYPE == "IN") %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE,"%Y-%m-%d"),
         quarter=case_when(
           "2021-10-01" > EVENT_DATE & EVENT_DATE >="2021-07-01" ~ "21-Q4",
           "2022-01-01" > EVENT_DATE & EVENT_DATE  >="2021-10-01" ~ "22-Q1",
           "2022-04-01" > EVENT_DATE & EVENT_DATE  >="2022-01-01" ~ "22-Q2",
           "2022-07-01" > EVENT_DATE & EVENT_DATE  >="2022-04-01" ~ "22-Q3",
           EVENT_DATE >="2022-07-01" ~ "22-Q4",
           T ~ NA_character_
         )) %>%
  group_by(PATIENT_ID_NUM) %>%
  slice_max(EVENT_DATE,n=1) %>%
  filter(EVENT_DATE >= "2021-07-01",
         DATA_VALUE_CODE == "WWP",
         !is.na(quarter))  %>%
  inner_join(vets,by=c("PATIENT_ID_NUM"))

sum(table(referral_processed$PATIENT_ID_NUM) > 1) #spotcheck
table(referral_processed$ACCEPTED_FLAG)
ACCEPTANCE_TABLE<-table(referral_processed$quarter,referral_processed$ACCEPTED_FLAG) %>%
  as.data.frame() %>%
  pivot_wider(id_cols=Var1,names_from=Var2,values_from=Freq) %>%
  mutate(Total = N + NR + Y,
         Pending = NR,
         Pending_perc = Pending/Total*100,
         Resolved = N + Y,
         Resolved_perc = Resolved/Total*100,
         Accepted = Y,
         Accepted_perc = Accepted/Total*100) %>%
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
table6<-table5 %>% select(-one_of(c("Y","N","NR")))

## 11.	Average Time in Days to Treatment

overall_iop_start<-visits %>%
  mutate(IOP_START_DATE = as.Date(IOP_START_DATE,"%Y-%m-%d")) %>%
  filter(PATIENT_ID_NUM %in% vets_with_treatment$PATIENT_ID_NUM,
         as.character(PATIENT_ID_NUM) %in% vetOnly,
         SERVICE_LINE == "IOP",
         SERVICE_DATE >= "2021-09-01") %>%
  group_by(PATIENT_ID_NUM) %>%
  slice_min(IOP_START_DATE,n=1,with_ties = F) %>%
  select(IOP_START_DATE,PATIENT_ID_NUM)
refs<-referrals %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE,"%Y-%m-%d")) %>%
  filter(DATA_VALUE_TYPE == "IN",
         EVENT_DATE >= "2021-09-01") %>%
  group_by(PATIENT_ID_NUM) %>%
  slice_max(EVENT_DATE,n=1,with_ties = F) %>%
  select(PATIENT_ID_NUM,EVENT_DATE)
compare_dates=inner_join(overall_iop_start,refs,by=c("PATIENT_ID_NUM")) %>%
  mutate(timedelta=difftime(IOP_START_DATE,EVENT_DATE,units="days"))
table7<-compare_dates %>%
  ungroup() %>%
  summarise(overall_N=n(),
            total_days=sum(timedelta),
            avg_days=mean(timedelta),
            median_days=median(timedelta),
            min_days=min(timedelta),
            max_days=max(timedelta))

refs<-referrals %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE,"%Y-%m-%d")) %>%
  filter(DATA_VALUE_TYPE == "IN",
         DATA_VALUE_CODE=="WWP",
         EVENT_DATE >= "2021-09-01") %>%
  group_by(PATIENT_ID_NUM) %>%
  slice_max(EVENT_DATE,n=1,with_ties = F) %>%
  select(PATIENT_ID_NUM,EVENT_DATE)
compare_dates=inner_join(overall_iop_start,refs,by=c("PATIENT_ID_NUM")) %>%
  mutate(timedelta=difftime(IOP_START_DATE,EVENT_DATE,units="days"))
table8<-compare_dates %>%
  ungroup() %>%
  summarise(overall_N=n(),
            total_days=sum(timedelta),
            avg_days=mean(timedelta),
            median_days=median(timedelta),
            min_days=min(timedelta),
            max_days=max(timedelta))

refs<-referrals %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE,"%Y-%m-%d")) %>%
  filter(DATA_VALUE_TYPE == "IN",
         DATA_VALUE_CODE!="WWP",
         EVENT_DATE >= "2021-09-01") %>%
  group_by(PATIENT_ID_NUM) %>%
  slice_max(EVENT_DATE,n=1,with_ties = F) %>%
  select(PATIENT_ID_NUM,EVENT_DATE)
compare_dates=inner_join(overall_iop_start,refs,by=c("PATIENT_ID_NUM")) %>%
  mutate(timedelta=difftime(IOP_START_DATE,EVENT_DATE,units="days"))
table9<-compare_dates %>%
  ungroup() %>%
  summarise(overall_N=n(),
            total_days=sum(timedelta),
            avg_days=mean(timedelta),
            median_days=median(timedelta),
            min_days=min(timedelta),
            max_days=max(timedelta))

overall_start<-visits %>%
  mutate(SERVICE_DATE = as.Date(SERVICE_DATE,"%Y-%m-%d")) %>%
  filter(PATIENT_ID_NUM %in% vets_with_treatment$PATIENT_ID_NUM,
         as.character(PATIENT_ID_NUM) %in% vetOnly,
         SERVICE_DATE >= "2021-09-01") %>%
  group_by(PATIENT_ID_NUM) %>%
  slice_min(SERVICE_DATE,n=1,with_ties = F) %>%
  select(SERVICE_DATE,PATIENT_ID_NUM)
refs<-referrals %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE,"%Y-%m-%d")) %>%
  filter(DATA_VALUE_TYPE == "IN",
         EVENT_DATE >= "2021-09-01") %>%
  group_by(PATIENT_ID_NUM) %>%
  slice_max(EVENT_DATE,n=1,with_ties = F) %>%
  select(PATIENT_ID_NUM,EVENT_DATE)
compare_dates=inner_join(overall_start,refs,by=c("PATIENT_ID_NUM")) %>%
  mutate(timedelta=difftime(SERVICE_DATE,EVENT_DATE,units="days"),
         timedelta=if_else(timedelta<as.difftime(0,units="days"),as.difftime(0,units="days"),timedelta))
table10<-compare_dates %>%
  ungroup() %>%
  summarise(overall_N=n(),
            total_days=sum(timedelta),
            avg_days=mean(timedelta),
            median_days=median(timedelta),
            min_days=min(timedelta),
            max_days=max(timedelta))

refs<-referrals %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE,"%Y-%m-%d")) %>%
  filter(DATA_VALUE_TYPE == "IN",
         DATA_VALUE_CODE=="WWP",
         EVENT_DATE >= "2021-09-01") %>%
  group_by(PATIENT_ID_NUM) %>%
  slice_max(EVENT_DATE,n=1,with_ties = F) %>%
  select(PATIENT_ID_NUM,EVENT_DATE)
compare_dates=inner_join(overall_start,refs,by=c("PATIENT_ID_NUM")) %>%
  mutate(timedelta=difftime(SERVICE_DATE,EVENT_DATE,units="days"),
         timedelta=if_else(timedelta<as.difftime(0,units="days"),as.difftime(0,units="days"),timedelta))
table11<-compare_dates %>%
  ungroup() %>%
  summarise(overall_N=n(),
            total_days=sum(timedelta),
            avg_days=mean(timedelta),
            median_days=median(timedelta),
            min_days=min(timedelta),
            max_days=max(timedelta))

refs<-referrals %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE,"%Y-%m-%d")) %>%
  filter(DATA_VALUE_TYPE == "IN",
         DATA_VALUE_CODE!="WWP",
         EVENT_DATE >= "2021-09-01") %>%
  group_by(PATIENT_ID_NUM) %>%
  slice_max(EVENT_DATE,n=1,with_ties = F) %>%
  select(PATIENT_ID_NUM,EVENT_DATE)
compare_dates=inner_join(overall_start,refs,by=c("PATIENT_ID_NUM")) %>%
  mutate(timedelta=difftime(SERVICE_DATE,EVENT_DATE,units="days"),
         timedelta=if_else(timedelta<as.difftime(0,units="days"),as.difftime(0,units="days"),timedelta))
table12<-compare_dates %>%
  ungroup() %>%
  summarise(overall_N=n(),
            total_days=sum(timedelta),
            avg_days=mean(timedelta),
            median_days=median(timedelta),
            min_days=min(timedelta),
            max_days=max(timedelta))

## 12.	Unique Patients by Patient Type

unique_patients_12_months<-visits %>%
  mutate(SERVICE_DATE = as.Date(SERVICE_DATE,"%Y-%m-%d")) %>%
  filter(SERVICE_DATE >= "2021-09-01") %>%
  group_by(PATIENT_ID_NUM) %>%
  slice_max(SERVICE_DATE,n=1,with_ties = F) %>%
  ungroup() %>%
  inner_join(patients,by=c("PATIENT_ID_NUM")) %>%
  group_by(PATIENT_TYPE) %>%
  summarise(count=n())
table13<-unique_patients_12_months
## 13.	Unique Patients by Program Type

past12MonthVisits<-visits %>%
  mutate(SERVICE_DATE = as.Date(SERVICE_DATE,"%Y-%m-%d")) %>%
  filter(SERVICE_DATE >= "2021-09-01")
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
table14<-table(summarise_visits$group,summarise_visits$PATIENT_TYPE) %>% as.data.frame()
table(summarise_visits$count,summarise_visits$SERVICE_LINE)
outputList<-list(t1=table1,
     t2=table2,
     t3=table3,
     t4_5=table4_5,
     t6=table6,
     t7=table7,
     t8=table8,
     t9=table9,
     t10=table10,
     t11=table11,
     t12=table12,
     t13=table13,
     t14=table14)
write_xlsx(outputList,dir %&% "test_output_rush.xlsx")
