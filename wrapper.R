#load in necessary functions from other scripts we've written
t1<-Sys.time()
source.dir<-"/Users/ryanschubert/Documents/RHP-KPI-WOW/"
"%&%" = function(a,b) paste0(a,b)
source(source.dir %&% 'merge_datasets.R')
source(source.dir %&% 'research_grade_dataset.R')
source(source.dir %&% 'kpi_report.R')
source(source.dir %&% 'wow_results.R')


#define some global variables we will need later
MGH.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/MGH/"
UCLA.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/UCLA/"
RUSH.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Rush/"
EMORY.dir<-"/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Emory/"
crossSite.dir<-'/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/dashboardData/Cross site data/'
dataOut.dir<-'/Users/ryanschubert/Dropbox (Rush)/WCN Data/processedData/researchDataset/'
masterListPath<-"/Users/ryanschubert/Dropbox (Rush)/Ryan's stuff/rush/remake KPI/Master List of therapies.csv"
#0th step is preprocessing the data to remove formatting errors
#harmonization happens in 00_harmonize_data


#pool the data across sites - this is an intermediary dataset and not the research grade dataset
#(assumes one unique set of files exists for each site. If a unique set of files cannot be identified for a site then the user can choose to either have the program exit or run without all the files)
mergeWCNData(MGH.dir=MGH.dir,
             UCLA.dir=UCLA.dir,
             RUSH.dir=RUSH.dir,
             EMORY.dir=EMORY.dir,
             out.dir=crossSite.dir)

# list.files(crossSite.dir)
#create a research grade dataset
generateDataset(assessments=crossSite.dir %&% "assessment_" %&% Sys.Date() %&% ".csv",
                patients=crossSite.dir %&% "patient_" %&% Sys.Date() %&% ".csv",
                out.dir=dataOut.dir)

#generate the kpi report
generateKPIreport(in.dir=crossSite.dir,
                  masterListFile=masterListPath,
                  out.dir='/Users/ryanschubert/Dropbox (Rush)/WCN Data/reports/',
                  cutoffDate=today)

#generate the wow report
generateWowResults(in.dir = crossSite.dir,
            out.dir='/Users/ryanschubert/Dropbox (Rush)/WCN Data/reports/',
            cutoffDate=today)
print(Sys.time()-t1)
