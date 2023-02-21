#load in necessary functions from other scripts we've written
t1<-Sys.time()
source.dir<-"/Users/ryanschubert/Documents/RHP-KPI-WOW/"
"%&%" = function(a,b) paste0(a,b)
source(source.dir %&% 'merge_datasets.R')
source(source.dir %&% 'research_grade_dataset.R')
source(source.dir %&% 'kpi_report.R')
source(source.dir %&% 'wow_results.R')

today=as.Date('2022-09-01')

#define some global variables we will need later
MGH.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/MGH/"
UCLA.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/UCLA/"
RUSH.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Rush/"
EMORY.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Emory/"
crossSite.dir<-'/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Cross site data/'
dataOut.dir<-'/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/researchDataset/'
#0th step is preprocessing the data to remove formatting errors


#pool the data across sites - this is an intermediary dataset and not the research grade dataset
#(assumes one unique set of files exists for each site. If a unique set of files cannot be identified for a site then the user can choose to either have the program exit or run without all the files)
mergeWCNData(MGH.dir=MGH.dir,
             UCLA.dir=UCLA.dir,
             RUSH.dir=RUSH.dir,
             EMORY.dir=EMORY.dir,
             out.dir=crossSite.dir)

#grab the newly created datasets
assessments<-fread(crossSite.dir %&% "assessment_" %&% today %&% '.csv',na=c("99","999","NA",""))
patients<-fread(crossSite.dir %&% "patient_" %&% today %&% '.csv',na=c("99","999","NA",""))
visits<-fread(crossSite.dir %&% "visit_" %&% today %&% '.csv',na=c("99","999","NA",""))
referrals<-fread(crossSite.dir %&% "referral_" %&% today %&% '.csv',na=c("99","999","NA",""))
satisfaction<-fread(crossSite.dir %&% "satisfaction_" %&% today %&% '.csv',na=c("99","999","NA",""))
master_list_services<-fread("/Users/ryanschubert/Dropbox (Rush)/Ryan's stuff/rush/remake KPI/Master List of therapies.csv",na=c("")) %>%
  filter(!is.na(`Treatment*`)) %>%
  rename(Treatment="Treatment*") %>%
  separate(`Master List of Therapies and Services (# = CDS value)`,into=c("TreatmentID"),sep=" ") %>% 
  mutate(TreatmentID=as.integer(TreatmentID))

#create a research grade dataset
generateDataset(assessments=assessments,
                patients=patients,
                out.dir=dataOut.dir)

#generate the kpi report
generateKPIreport(assessments=assessments,
                  patients=patients,
                  visits=visits,
                  referrals=referrals,
                  satisfaction=satisfaction, 
                  master_list_services=master_list_services,
                  out.dir='/Users/ryanschubert/Dropbox (Rush)/WCN Data/reports/',
                  cutoffDate=today)

#generate the wow report
generateWowResults(assessments=assessments,
                   patients=patients,
                   visits=visits,
                   referrals=referrals,
                   satisfaction=satisfaction, 
                   masterListServices=master_list_services,
                   out.dir='/Users/ryanschubert/Dropbox (Rush)/WCN Data/reports/',
                   cutoffDate=today)

for (site in c("EMORY",'MGH','RUSH','UCLA')) {
  assessmentsSubset<-assessments %>% filter(FACILITY_NAME==site)
  patientsSubset<-patients %>% filter(FACILITY_NAME==site)
  visitsSubset<-visits %>% filter(FACILITY_NAME==site)
  referralsSubset<-referrals %>% filter(FACILITY_NAME==site)
  satisfactionSubset<-satisfaction %>% filter(FACILITY_NAME==site)
  #generate the kpi report
  generateKPIreport(assessments=assessmentsSubset,
                    patients=patientsSubset,
                    visits=visitsSubset,
                    referrals=referralsSubset,
                    satisfaction=satisfactionSubset, 
                    master_list_services=master_list_services,
                    out.dir='/Users/ryanschubert/Documents/RHP-KPI-WOW/test/' %&% site %&% '_',
                    cutoffDate=today)
  
  #generate the wow report
  generateWowResults(assessments=assessmentsSubset,
                     patients=patientsSubset,
                     visits=visitsSubset,
                     referrals=referralsSubset,
                     satisfaction=satisfactionSubset, 
                     masterListServices=master_list_services,
                     out.dir='/Users/ryanschubert/Documents/RHP-KPI-WOW/test/' %&% site %&% '_',
                     cutoffDate=today)
}

print(Sys.time()-t1)
