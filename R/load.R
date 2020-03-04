## loadind data

# install.packages('xlsx')
# library(xlsx)
# 
# A = read.xlsx('../Health_Insights_Case_Study__TSO.xlsx', sheetIndex = 3, header=TRUE)
# B = read.xlsx('../Health_Insights_Case_Study__TSO.xlsx', sheetIndex = 4, header=TRUE)
# C = read.xlsx('../Health_Insights_Case_Study__TSO.xlsx', sheetIndex = 5, header=TRUE)
# affil = read.xlsx('../Health_Insights_Case_Study__TSO.xlsx',sheetIndex = 6, header = TRUE)
# 
# 
# length(unique(A$gpharma_HCP_ID))
# length(unique(B$Doctor_ID))       


if(!('readxl' %in% installed.packages())) install.packages('readxl')
library(readxl)
library(dplyr)

A = read_excel('./Health_Insights_Case_Study__TSO.xlsx',sheet="Set A")
B = read_excel('./Health_Insights_Case_Study__TSO.xlsx',sheet="Set B")
C = read_excel('./Health_Insights_Case_Study__TSO.xlsx',sheet="Set C")
affil = read_excel('./Health_Insights_Case_Study__TSO.xlsx',
                   sheet="PI Affliation",col_types=c("numeric","numeric"))
affil = affil[-nrow(affil),] # contains sum of last column
## B$Performance[20:22]
## plot(as.numeric(B$Performance),
## B$pat_ENTERED_TREATMENT/B$pat_TARGET_TREATMENT) ; abline(0,1)
B$Performance = B$pat_ENTERED_TREATMENT / B$pat_TARGET_TREATMENT # correcting a typo in B

A = A[,c("gpharma_HCP_ID",
         "physician_last_name",
         "physician_first_name",
         "physician_city",
         "physician_specialty_desc",
         "Patient_Count")]
B = B[,c("STUDY_CODE_ALIAS","CENTER_NAME","Doctor_ID","PI_FORENAME","PI_SURNAME",
         "Trial_STATUS_DESC","pat_TARGET_TREATMENT","pat_ENTERED_TREATMENT",
         "Performance","# of competitor trials by PI (active)",
         "# of Supporting Staff","PI  Availability","PI Tier","PI Risk Score")]

## completing A with column Doctor_ID 
A_doctid= numeric(nrow(A))
for(i in  1:nrow(A))
{
  j = which( C$gpharma_HCP_ID == A$gpharma_HCP_ID[i] ) 
  if(length(j) > 1)
  { 
    print(C[j,
          c("HCP_First_Name","HCP_Last_Name",
            "gpharma_HCP_ID","Doctor ID(Set B)")])
  print('')
  if(length(unique( C$`Doctor ID(Set B)`[j])) == 1)
  { j = j[1]}else
    stop()
    }
  A_doctid[i] = C$`Doctor ID(Set B)`[j]
}
A$Doctor_ID = A_doctid



#####################################
## understanding info and structure
## A
str(A)
dim(A)
length(unique(A$gpharma_HCP_ID))
cbind(A$gpharma_HCP_ID,
unique(A$gpharma_HCP_ID))

## B
str(B)
dim(B)
length(unique(B$STUDY_CODE_ALIAS))
length(unique(B$CENTER_NAME))
summary(as.numeric(B$Performance))

xx = B[,1:2]
yy = distinct(xx)

sort(A$physician_last_name)
sort(B$PI_SURNAME)
sort(C$HCP_Last_Name)

dim(affil)
sort(affil$`Row Labels`)
afill()

sort(A$gpharma_HCP_ID)
sort(affil$`Row Labels`)
sort(B$Doctor_ID)

cbind(sort(A$gpharma_HCP_ID),sort(B$Doctor_ID))

setdiff(B$Doctor_ID,A$gpharma_HCP_ID)
setdiff(A$gpharma_HCP_ID,B$Doctor_ID)
A$gpharma_HCP_ID


######################
## Digging in sheet A
summary(as.factor(A$physician_specialty_desc))

sub_opht = (A$physician_specialty_desc == "OPHTHALMOLOGY")
par(mfrow=c(1,2))
hist(A$Patient_Count)
hist(A$Patient_Count[sub_opht])

A_opht = A[sub_opht,]
A_opht_sort = A_opht[order(A_opht$Patient_Count,decreasing=TRUE),]
A_opht_sort

#######################
## Digging into sheet B

B[order(B$STUDY_CODE_ALIAS),]
xx = B[order(B$PI_SURNAME),]

# Performance?
plot(B$pat_TARGET_TREATMENT,B$pat_ENTERED_TREATMENT);abline(0,1)
plot(B$pat_ENTERED_TREATMENT / B$pat_TARGET_TREATMENT , B$Performance)

# Patient count in A vs patient entered in B ? 
B_sumpat = aggregate(x=B[c("pat_TARGET_TREATMENT",
                           "pat_ENTERED_TREATMENT")],
                     by=list(Doctor_ID=B$Doctor_ID),FUN=sum)
yy = B_sumpat[order(B_sumpat$Doctor_ID),]

A_sumpat = aggregate(x=A$Patient_Count,
                     by=list(Doctor_ID=B$Doctor_ID),FUN=sum)


xx = A[order(A$Doctor_ID),c("Patient_Count","Doctor_ID")]

sum(A$Doctor_ID %in% B$Doctor_ID)
sum(A$Doctor_ID[A$physician_specialty_desc=="OPHTHALMOLOGY"] %in% B$Doctor_ID)
sum(B$Doctor_ID %in% A$Doctor_ID)

## understanding B$`PI Tier`
qq = quantile(B$Performance,prob=c(1/3,2/3))

mytier = numeric(nrow(B))
qq = quantile(B$Performance,prob=c(1/3,2/3))
subs = B$Performance <= qq[1] 
mytier[subs]  = 3
subs = (B$Performance > qq[1]  & B$Performance <= qq[2]) 
mytier[subs]  = 2
subs = B$Performance > qq[2] 
mytier[subs]  = 1

mean(mytier!=B$`PI Tier`)

table(B$`PI Tier`,B$`PI Risk Score`)
table(B$`PI Tier`)
table(B$`PI Risk Score`)




