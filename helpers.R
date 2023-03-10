library(lubridate)

#concatonates two strings together
"%&%" = function(a,b) paste0(a,b)
"%_%" = function(a,b) paste(a,b,sep="_")

#identifies if a given vector is empty
isEmpty<-function(vec) {
  return(sum(is.na(vec)) == length(vec))
}

emptyColumns<-function(data){
  return(apply(data,2,isEmpty))
}

timestamp<-gsub(' ','_',now())


# date helpers
today<-Sys.Date()

determineReportingCycleStart<-function(date){
  month<-as.numeric(format(as.Date(date),'%m'))
  year<-as.numeric(format(as.Date(date), '%Y'))
  cycleStart<-ifelse(month==1, (year-1) %&% '-12-01',year %&% '-' %&% (month -1) %&% '-01')
  return(cycleStart)
}

determineFiscalYear<-function(date){
  year<-as.numeric(format(as.Date(date), '%Y'))
  FY<-ifelse(date >= year %&% "-10-01",(year + 1),year)
  return(FY)
}

determineQuarter=function(date) {
  year<-as.numeric(format(as.Date(date), '%Y'))
  quarter<-case_when(date >= year %&% "-10-01" ~ (year + 1) %&% "-Q1",
                     date >= year %&% "-07-01" ~ year %&% "-Q4", 
                     date >= year %&% "-04-01" ~ year %&% "-Q3",
                     date >= year %&% "-01-01" ~ year %&% "-Q2")
  return(quarter)
}

determineQuarterStart<-function(date){
  year<-as.numeric(format(date, '%Y'))
  quarter<-case_when(date >= year %&% "-10-01" ~ year %&% "-10-01",
                     date >= year %&% "-07-01" ~ year %&% "-07-01",
                     date >= year %&% "-04-01" ~ year %&% "-04-01",
                     date >= year %&% "-01-01" ~ year %&% "-01-01")
  return(quarter)
}

createQuarterTemplate<-function(date) {
  template<-data.frame(startDates=rep(NA,5),
                       quarter=rep(NA,5))
  iterDate<-date
  for(i in 1:5) {
    template$startDates[i]<-determineQuarterStart(iterDate)
    iterDate<-iterDate %m-% months(3)
    template$quarter[i]<-determineQuarter(template$startDates[i])
  }
  return(template)
}

determineFYStart<-function(date) {
  year<-as.numeric(format(date, '%Y'))
  return(ifelse(date > year %&% '-10-01',year %&% '-10-01', (year - 1) %&% '-10-01'))
}

#this function returns an array with veterans who have at least one treatment according to the master list of therapies
getWithTreatmentRecordIDs<-function(data,services=master_list_services) {
  withTreatment<-data %>%
    filter(SERVICE_PERFORMED %in% services$TreatmentID) %>%
    select(PATIENT_ID_NUM) %>%
    arrange() %>%
    distinct() %>%
    unlist() %>%
    unname()
  return(withTreatment)
}
