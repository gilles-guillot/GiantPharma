## Data analysis  
if(!('readxl' %in% installed.packages())) install.packages('readxl')
library(readxl)
library(tidyverse)
library(dplyr)
library(knitr)

A = read_excel('./Health_Insights_Case_Study__TSO.xlsx',sheet="Set A")
B = read_excel('./Health_Insights_Case_Study__TSO.xlsx',sheet="Set B")
C = read_excel('./Health_Insights_Case_Study__TSO.xlsx',sheet="Set C")
affil = read_excel('./Health_Insights_Case_Study__TSO.xlsx',
                   sheet="PI Affliation",col_types=c("numeric","numeric"))
affil = affil[-nrow(affil),] # contains sum of last column

B$Performance = B$pat_ENTERED_TREATMENT / B$pat_TARGET_TREATMENT # correcting a typo in B

A = A[,c("gpharma_HCP_ID",
         "physician_last_name",
         "physician_first_name",
         "physician_city",
         "physician_specialty_desc",
         "Patient_Count")]
B = B[,c("STUDY_CODE_ALIAS","CENTER_NAME","Doctor_ID","PI_FORENAME","PI_SURNAME",
         "pat_TARGET_TREATMENT","pat_ENTERED_TREATMENT",
         "Performance","# of competitor trials by PI (active)",
         "# of Supporting Staff","PI Tier","PI Risk Score")]


## Converting PI Risk Score H/M/L into numerical safety score 1/2/3
B$`PI Reliability Score` = numeric(nrow(B))
B$`PI Reliability Score`[B$`PI Risk Score`=='H'] = 1
B$`PI Reliability Score`[B$`PI Risk Score`=='M'] = 2
B$`PI Reliability Score`[B$`PI Risk Score`=='L'] = 3
B$`PI Reliability Score` = as.numeric(B$`PI Reliability Score`)


## completing A with column Doctor_ID 
A_doctid= numeric(nrow(A))
for(i in  1:nrow(A))
{
  j = which( C$gpharma_HCP_ID == A$gpharma_HCP_ID[i] ) 
  if(length(j) > 1)
  { 
    if(length(unique( C$`Doctor ID(Set B)`[j])) == 1)
    { j = j[1]}else
      stop()
  }
  A_doctid[i] = C$`Doctor ID(Set B)`[j]
}
A$Doctor_ID = A_doctid

################
## Finding ophthalmologists in B
B$Opht = rep(NA,nrow(B))
for(i in 1:nrow(B))
{
  if(B$Doctor_ID[i] %in% A$Doctor_ID)
  {
    subs = which(A$Doctor_ID==B$Doctor_ID[i])
    #if(length(subs)>1) print(A$physician_specialty_desc[subs]) # print(c(i,subs))
    B$Opht[i] = ifelse(A$physician_specialty_desc[subs[1]]=='OPHTHALMOLOGY',
                       TRUE,FALSE)
  }
}
B_Opht = B[B$Opht,]
B_Opht = B_Opht[complete.cases(B_Opht),]

B$Spec = rep(NA,nrow(B))
for(i in 1:nrow(B))
{
  if(B$Doctor_ID[i] %in% A$Doctor_ID)
  {
    subs = which(A$Doctor_ID==B$Doctor_ID[i])
    #if(length(subs)>1) print(A$physician_specialty_desc[subs]) # print(c(i,subs))
    B$Spec[i] = A$physician_specialty_desc[subs[1]]
  }
}

################## 
## aggregating rows by Doctor_ID
B_Opht_ag = aggregate(x=B_Opht[,c("Performance",
                                 "# of competitor trials by PI (active)",
                                 "# of Supporting Staff",
                                 "PI Tier",
                                 "PI Reliability Score")],
                      by=list(Doctor_ID=B_Opht$Doctor_ID),
                      FUN=mean)
B_Opht_ag$PI_SURNAME = B_Opht_ag$PI_FORENAME = B_Opht_ag$CENTER_NAME = B_Opht_ag$Score = character(nrow(B_Opht_ag))

for(i in 1:nrow(B_Opht_ag))
{
  subs = which(B_Opht$Doctor_ID == B_Opht_ag$Doctor_ID[i])
  B_Opht_ag$PI_SURNAME[i] = B_Opht$PI_SURNAME[subs][1]
  B_Opht_ag$PI_FORENAME[i] = B_Opht$PI_FORENAME[subs][1]
  B_Opht_ag$CENTER_NAME[i] = B_Opht$CENTER_NAME[subs][1]
}
B_Opht_ag$Score = ( B_Opht_ag$Performance +  
            B_Opht_ag$`# of competitor trials by PI (active)` +   
            B_Opht_ag$`# of Supporting Staff` + 
            B_Opht_ag$`PI Tier`  + 
            B_Opht_ag$`PI Reliability Score` )


