###############################################################################
# This program creates a function called vr12score which calculates the       # 
# VR-12 Physical Composite Score (PCS) and Mental Composite Score (MCS) using #
# R statistical software                                                      #
# The function 'vr12score' has 3 possible arguments                           #
#  1) https://urldefense.com/v3/__http://file.in__;!!OlavHw!rS0cst1RHmRGts6kfPPVt_77Lq2nZuFI8vslYkdLufB6Iv3oIVBt6_XQSCWkgfPsLvM$ :     The file path to the CSV data set that contains the raw    #  
#                  VR-12 data                                                 #
#  2) file.out:    The file path and name to which a new CSV file will be     #   
#                  created that contains the original data and the new PCS    #
#                  and MCS columns.  If NULL, then 'https://urldefense.com/v3/__http://file.in__;!!OlavHw!rS0cst1RHmRGts6kfPPVt_77Lq2nZuFI8vslYkdLufB6Iv3oIVBt6_XQSCWkgfPsLvM$ ' will be          #
#                  overwritten with the new data set created with PCS and MCS #
#  3) keyfilepath: The file path to the folder that contains the four CSV     #
#                  coefficients data files.  Defaults to the current working  #
#                  directory (getwd).  Error will occur if correct folder     #
#                  is not given.                                              #
###############################################################################


###############################################################################
# vr12score Requirements:                                                     #
# 1)  'https://urldefense.com/v3/__http://file.in__;!!OlavHw!rS0cst1RHmRGts6kfPPVt_77Lq2nZuFI8vslYkdLufB6Iv3oIVBt6_XQSCWkgfPsLvM$ ' must be a CSV document with the first row being column names. #
#     Blanks or 'NA' are considered missing values                            #
# 2)  'https://urldefense.com/v3/__http://file.in__;!!OlavHw!rS0cst1RHmRGts6kfPPVt_77Lq2nZuFI8vslYkdLufB6Iv3oIVBt6_XQSCWkgfPsLvM$ ' must contain column names: Survey, vr1, vr2a, vr2b, vr3a,     #
#     vr3b, vr4a, vr4b, vr5, vr6a, vr6b, vr6c, and vr7 (specific order is not    #
#     needed, but capitalization must match exactly or error will be printed) #
# 3)  column 'Survey' (Mail or Phone) must have entries that all at least     #
#     start with m/M or p/P                                                   #
# 4)  the other columns, listed in 2), entries must be whole numbers that are #
#     possible values for each column.  If not an error will be printed.      #
# 5)  CSV data files by the names: mcs90_vr12_mar14_native_mail.csv,          # 
#     mcs90_vr12_mar14_native_phone.csv,                                      #
#     pcs90_vr12_mar14_native_mail.csv, and                                   #
#     pcs90_vr12_mar14_native_phone.csv.  Must be in keyfilepath folder       #
###############################################################################

###############################################################################
#  Creator:  Scott J Hetzel MS                                                #
#  Title:  Associate Researcher                                               #
#  Affiliation: University of Wisconsin - Madison                             #
#  Department:  Biostatistics and Medical Informatics                         #
#  Email:  hetzel@biostat.wisc.edu                                            #
#  Date:  12/15/2014                                                          #
#  R version:  R 3.0.1                                                        #
###############################################################################
#removed reference to url - Ryan
vr12score <- function(file.in ,file.out=NULL,keyfilepath=NULL)
  {
    #  Load CSV data set from https://urldefense.com/v3/__http://file.in__;!!OlavHw!rS0cst1RHmRGts6kfPPVt_77Lq2nZuFI8vslYkdLufB6Iv3oIVBt6_XQSCWkgfPsLvM$ 
  #removed reference to url - Ryan  
  vr12data <- read.csv(file.in ,header=TRUE,na.strings=c("NA",""))

    #  Make sure column names contain: "Survey","vr1","vr2a","vr2b","vr3a","vr3b",
    #  "vr4a","vr4b","vr5","vr6a","vr6b","vr6c", and "vr7"
  #this is a case sensitive search  and I believe the excel is in uppercase, I made my excel names lowercase manually for this - Ryan
    varlist <- c("Survey","vr1","vr2a","vr2b","vr3a","vr3b","vr4a","vr4b","vr5","vr6a","vr6b","vr6c","vr7")
    if(sum(!(varlist %in% colnames(vr12data)))>0)
      stop(paste("Column names of the original data file must contain:",paste(varlist,collapse=", "),sep=""))

    #  Check that Survey is a factor or character
    if(!is.factor(vr12data[["Survey"]])&!is.character(vr12data[["Survey"]]))
      stop("Column Survey needs to be a factor or a character vector")
    else
      { # Check to make sure all Survey rows start with m/M or p/P
        if(sum(levels(as.factor(substr(levels(as.factor(vr12data[["Survey"]])),1,1))) %in% c("m","M","p","P"))
           != nlevels(as.factor(substr(levels(as.factor(vr12data[["Survey"]])),1,1))))
          stop("There is at least 1 entry in Survey that doesn't start with m/M or p/P")
      }
    #  Check that each data column is numeric
    for(j in 2:length(varlist))
      {
        if(!is.numeric(vr12data[[varlist[j]]]))
      stop(paste("Column ",varlist[j], " needs to be numeric ",sep=""))
        else 
          {#  Now check if each column has the correct possible values
            if(j %in% c(2,5,6,7,8,9,13))
              numlist <- as.factor(1:5)
            else if(j %in% c(3,4))
              numlist <- as.factor(1:3)
            else
              numlist <- as.factor(1:6)
            if(sum(levels(as.factor(vr12data[[varlist[j]]])) %in% numlist) != nlevels(as.factor(vr12data[[varlist[j]]])))
          stop(paste("There is at least 1 entry in ", varlist[j], " that isn't an appropriate value: ",
                     paste(numlist,collapse=", "),sep=""))
          }
      }     
      
    #  Create 0-100 point scales of the items
    vr12data[["vr1x"]] <- ifelse(vr12data[["vr1"]]==1,100,
                                 ifelse(vr12data[["vr1"]]==2,85,
                                        ifelse(vr12data[["vr1"]]==3,60,
                                               ifelse(vr12data[["vr1"]]==4,35,0))))
    vr12data[["vr2ax"]] <- (vr12data[["vr2a"]]-1)*50
    vr12data[["vr2bx"]] <- (vr12data[["vr2b"]]-1)*50
    vr12data[["vr3ax"]] <- (5-vr12data[["vr3a"]])*25
    vr12data[["vr3bx"]] <- (5-vr12data[["vr3b"]])*25
    vr12data[["vr4ax"]] <- (5-vr12data[["vr4a"]])*25
    vr12data[["vr4bx"]] <- (5-vr12data[["vr4b"]])*25
    vr12data[["vr5x"]] <- (5-vr12data[["vr5"]])*25
    vr12data[["vr6ax"]] <- (6-vr12data[["vr6a"]])*20
    vr12data[["vr6bx"]] <- (6-vr12data[["vr6b"]])*20
    vr12data[["vr6cx"]] <- (vr12data[["vr6c"]]-1)*20
    vr12data[["vr7x"]] <- (vr12data[["vr7"]]-1)*25

    #  Calculate the key value, if value is missing assign it 2 to corresponding power, if not then 0
    key.slots <- data.frame("vr1k"=ifelse(is.na(vr12data[["vr1"]]),2^11,0),
                            "vr2ak"=ifelse(is.na(vr12data[["vr2a"]]),2^10,0),
                            "vr2bk"=ifelse(is.na(vr12data[["vr2b"]]),2^9,0),
                            "vr3ak"=ifelse(is.na(vr12data[["vr3a"]]),2^8,0),
                            "vr3bk"=ifelse(is.na(vr12data[["vr3b"]]),2^7,0),
                            "vr4ak"=ifelse(is.na(vr12data[["vr4a"]]),2^6,0),
                            "vr4bk"=ifelse(is.na(vr12data[["vr4b"]]),2^5,0),
                            "vr5k"=ifelse(is.na(vr12data[["vr5"]]),2^4,0),
                            "vr6ak"=ifelse(is.na(vr12data[["vr6a"]]),2^3,0),
                            "vr6bk"=ifelse(is.na(vr12data[["vr6b"]]),2^2,0),
                            "vr6ck"=ifelse(is.na(vr12data[["vr6c"]]),2^1,0),
                            "vr7k"=ifelse(is.na(vr12data[["vr7"]]),2^0,0))
    #  Create key column
    vr12data[["key"]] <- apply(key.slots,1,sum)
    
    #  Load external key coefficient data sets
    if(is.null(keyfilepath))
      keyfilepath <- getwd()

    #  Load key coefficient CSV data files that contain coefficients based on key
    #edited references to file keys - Ryan
    mcsMail <- read.csv(paste(keyfilepath,"/R Scoringmcs90_vr12_mar14_native_mail.csv",sep=""),header=TRUE)
    mcsPhone <- read.csv(paste(keyfilepath,"/R Scoringmcs90_vr12_mar14_native_phone.csv",sep=""),header=TRUE)
    pcsMail <- read.csv(paste(keyfilepath,"/R Scoringpcs90_vr12_mar14_native_mail.csv",sep=""),header=TRUE)
    pcsPhone <- read.csv(paste(keyfilepath,"/R Scoringpcs90_vr12_mar14_native_phone.csv",sep=""),header=TRUE)
    
    #  Run a for loop to get MCS and PCS scores for each subject
    #  Set default setting for MSC column
    vr12data[["PCS"]] <- NA
    vr12data[["MCS"]] <- NA
    
    for(i in 1:length(vr12data[["key"]]))
      {
        if(substr(vr12data[["Survey"]][i],1,1)=="m"|substr(vr12data[["Survey"]][i],1,1)=="M")  # Subset based on Mail survey
          {
            #  Calculate MCS based on the key and mail survey
            if(vr12data[["key"]][i] %in% mcsMail$key)
              {
                vr12data[["MCS"]][i] <- sum(c(vr12data[["vr1x"]][i],vr12data[["vr2ax"]][i],vr12data[["vr2bx"]][i],vr12data[["vr3ax"]][i],
                                              vr12data[["vr3bx"]][i],vr12data[["vr4ax"]][i],vr12data[["vr4bx"]][i],vr12data[["vr5x"]][i],
                                              vr12data[["vr6ax"]][i],vr12data[["vr6bx"]][i],vr12data[["vr6cx"]][i],vr12data[["vr7x"]][i])*
                                            c(mcsMail$vr1x[mcsMail$key==vr12data[["key"]][i]],mcsMail$vr2ax[mcsMail$key==vr12data[["key"]][i]],
                                              mcsMail$vr2bx[mcsMail$key==vr12data[["key"]][i]],mcsMail$vr3ax[mcsMail$key==vr12data[["key"]][i]],
                                              mcsMail$vr3bx[mcsMail$key==vr12data[["key"]][i]],mcsMail$vr4ax[mcsMail$key==vr12data[["key"]][i]],
                                              mcsMail$vr4bx[mcsMail$key==vr12data[["key"]][i]],mcsMail$vr5x[mcsMail$key==vr12data[["key"]][i]],
                                              mcsMail$vr6ax[mcsMail$key==vr12data[["key"]][i]],mcsMail$vr6bx[mcsMail$key==vr12data[["key"]][i]],
                                              mcsMail$vr6cx[mcsMail$key==vr12data[["key"]][i]],mcsMail$vr7x[mcsMail$key==vr12data[["key"]][i]]),
                                            na.rm=TRUE)+mcsMail$cons[mcsMail$key==vr12data[["key"]][i]]
              }
            else  # key isn't in MCS mail survey
              vr12data[["MCS"]][i] <- NA

            #  Calculate PCS based on the key and mail survey
            if(vr12data[["key"]][i] %in% pcsMail$key)
              {
                vr12data[["PCS"]][i] <- sum(c(vr12data[["vr1x"]][i],vr12data[["vr2ax"]][i],vr12data[["vr2bx"]][i],vr12data[["vr3ax"]][i],
                                              vr12data[["vr3bx"]][i],vr12data[["vr4ax"]][i],vr12data[["vr4bx"]][i],vr12data[["vr5x"]][i],
                                              vr12data[["vr6ax"]][i],vr12data[["vr6bx"]][i],vr12data[["vr6cx"]][i],vr12data[["vr7x"]][i])*
                                            c(pcsMail$vr1x[pcsMail$key==vr12data[["key"]][i]],pcsMail$vr2ax[pcsMail$key==vr12data[["key"]][i]],
                                              pcsMail$vr2bx[pcsMail$key==vr12data[["key"]][i]],pcsMail$vr3ax[pcsMail$key==vr12data[["key"]][i]],
                                              pcsMail$vr3bx[pcsMail$key==vr12data[["key"]][i]],pcsMail$vr4ax[pcsMail$key==vr12data[["key"]][i]],
                                              pcsMail$vr4bx[pcsMail$key==vr12data[["key"]][i]],pcsMail$vr5x[pcsMail$key==vr12data[["key"]][i]],
                                              pcsMail$vr6ax[pcsMail$key==vr12data[["key"]][i]],pcsMail$vr6bx[pcsMail$key==vr12data[["key"]][i]],
                                              pcsMail$vr6cx[pcsMail$key==vr12data[["key"]][i]],pcsMail$vr7x[pcsMail$key==vr12data[["key"]][i]]),
                                            na.rm=TRUE)+pcsMail$cons[pcsMail$key==vr12data[["key"]][i]]
              }
            else  # key isn't in PCS mail survey
              vr12data[["PCS"]][i] <- NA
          }
        else if(substr(vr12data[["Survey"]][i],1,1)=="p"|substr(vr12data[["Survey"]][i],1,1)=="P")  # Subset based on Phone survey
          {
            #  Calculate MCS based on the key and phone survey
            if(vr12data[["key"]][i] %in% mcsPhone$key)
              {
                vr12data[["MCS"]][i] <- sum(c(vr12data[["vr1x"]][i],vr12data[["vr2ax"]][i],vr12data[["vr2bx"]][i],vr12data[["vr3ax"]][i],
                                              vr12data[["vr3bx"]][i],vr12data[["vr4ax"]][i],vr12data[["vr4bx"]][i],vr12data[["vr5x"]][i],
                                              vr12data[["vr6ax"]][i],vr12data[["vr6bx"]][i],vr12data[["vr6cx"]][i],vr12data[["vr7x"]][i])*
                                            c(mcsPhone$vr1x[mcsPhone$key==vr12data[["key"]][i]],mcsPhone$vr2ax[mcsPhone$key==vr12data[["key"]][i]],
                                              mcsPhone$vr2bx[mcsPhone$key==vr12data[["key"]][i]],mcsPhone$vr3ax[mcsPhone$key==vr12data[["key"]][i]],
                                              mcsPhone$vr3bx[mcsPhone$key==vr12data[["key"]][i]],mcsPhone$vr4ax[mcsPhone$key==vr12data[["key"]][i]],
                                              mcsPhone$vr4bx[mcsPhone$key==vr12data[["key"]][i]],mcsPhone$vr5x[mcsPhone$key==vr12data[["key"]][i]],
                                              mcsPhone$vr6ax[mcsPhone$key==vr12data[["key"]][i]],mcsPhone$vr6bx[mcsPhone$key==vr12data[["key"]][i]],
                                              mcsPhone$vr6cx[mcsPhone$key==vr12data[["key"]][i]],mcsPhone$vr7x[mcsPhone$key==vr12data[["key"]][i]]),
                                            na.rm=TRUE)+mcsPhone$cons[mcsPhone$key==vr12data[["key"]][i]]
              }
            else  # key isn't in MCS phone survey
              vr12data[["MCS"]][i] <- NA

            #  Calculate PCS based on the key and phone survey
            if(vr12data[["key"]][i] %in% pcsPhone$key)
              {
                vr12data[["PCS"]][i] <- sum(c(vr12data[["vr1x"]][i],vr12data[["vr2ax"]][i],vr12data[["vr2bx"]][i],vr12data[["vr3ax"]][i],
                                              vr12data[["vr3bx"]][i],vr12data[["vr4ax"]][i],vr12data[["vr4bx"]][i],vr12data[["vr5x"]][i],
                                              vr12data[["vr6ax"]][i],vr12data[["vr6bx"]][i],vr12data[["vr6cx"]][i],vr12data[["vr7x"]][i])*
                                            c(pcsPhone$vr1x[pcsPhone$key==vr12data[["key"]][i]],pcsPhone$vr2ax[pcsPhone$key==vr12data[["key"]][i]],
                                              pcsPhone$vr2bx[pcsPhone$key==vr12data[["key"]][i]],pcsPhone$vr3ax[pcsPhone$key==vr12data[["key"]][i]],
                                              pcsPhone$vr3bx[pcsPhone$key==vr12data[["key"]][i]],pcsPhone$vr4ax[pcsPhone$key==vr12data[["key"]][i]],
                                              pcsPhone$vr4bx[pcsPhone$key==vr12data[["key"]][i]],pcsPhone$vr5x[pcsPhone$key==vr12data[["key"]][i]],
                                              pcsPhone$vr6ax[pcsPhone$key==vr12data[["key"]][i]],pcsPhone$vr6bx[pcsPhone$key==vr12data[["key"]][i]],
                                              pcsPhone$vr6cx[pcsPhone$key==vr12data[["key"]][i]],pcsPhone$vr7x[pcsPhone$key==vr12data[["key"]][i]]),
                                            na.rm=TRUE)+pcsPhone$cons[pcsPhone$key==vr12data[["key"]][i]]
              }
            else  # key isn't in PCS phone survey
              vr12data[["PCS"]][i] <- NA
          }
      }
    #  Export the vr12data file to either rewrite https://urldefense.com/v3/__http://file.in__;!!OlavHw!rS0cst1RHmRGts6kfPPVt_77Lq2nZuFI8vslYkdLufB6Iv3oIVBt6_XQSCWkgfPsLvM$  or to write a new file to file.out
    #removed if else clause and just had it write to file - Ryan  
    write.csv(vr12data,file=file.out,row.names=FALSE)
  }
