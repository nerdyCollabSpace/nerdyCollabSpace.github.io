#author:  Group7
#purpose: project1, Pregnancy
#date:    2018.10.06
#version: R version 3.5.1

##global adjustments starts 01
rm(list=ls())
#seed for replication
set.seed(101)
oldwarnval <- getOption("warn") #preserve old value
options(warn = -1) #warnings are silent now

#library
#install.packages("data.table")
library(data.table)
library("caTools")




#Data file locations
fl <- "~/Downloads/Tony/ProjectFour/Data/LoanStats3a.csv"
fl2 <- "~/Downloads/Tony/ProjectFour/Data/LoanStats3a.xlsx"
##global adjustments ends 01





#$# Part A. What is in a credit score?
#$#1
#Download the Lending Club data (https://www.lendingclub.com/info/download-data.action)
#for the year 2014, and read the data into a data frame in R.

## read data in df
headers = read.csv(fl, skip = 1, header = F, nrows = 1, as.is = T)
df = data.frame(read.csv(fl, skip = 2, header = F))
colnames(df)= headers
head(df)

#$#2a
# data cleansing with limited colums
# grade: A<B<C<D<E<F<G, from A to E risk increases, retrun increases, interest increases
#       grading of borrowers
# sub_grade: 1<2<3<4<5 similar, within grades in 5 categories
# zip_code, 
# term: MATURITY time frame, months
# loan_amnt
# annual_inc:
# verification_status: of borrower, his history with lending club
# purpose: Categories e.g. credir card repament, new car 
# tax_liens: A lien means that the government has the first legal claim to 
#            your property, which it can seize and sell to pay off your tax debt.
# pct_tl_nvr_dlq
# int_rate:
# loan_status:

df2 <- df[,c("grade", "sub_grade", "zip_code", "term", "loan_amnt", "annual_inc",
             "verification_status", "purpose", "tax_liens", "pct_tl_nvr_dlq",
             "int_rate", "loan_status")]

#$#2bi
#i.	How are each of the variables stored within R?  Is this the format that is most useful?
#For any variables stored in the “wrong” format, devise a strategy to convert these to a
#more useful format.
summary(df2)
# columns to be adjusted as we proceed
#ZIP
str(df2$zip_code) # the factor and level as integers, should be treated as text
df2$zip_code <- as.character(df2$zip_code) # change it in character
typeof(df2$zip_code)
#term
str(df2$term)
#sub_grade
str(df2$sub_grade)


#$#2bii
#ii.	For continuous variables, are there any outliers or skew? Think about the 
#likely consequences of these characteristics and any practical solutions.  
#You should address these issues in how you aggregate and analyze the data, 
#and describe any relevant issues in your report.

# columns to be adjusted as we proceed




#$#3
#3.	The Lending Club data are at the loan level.  Produce two alternative 
#“aggregated” data sets, which we’ll examine at different points throughout 
# the analysis.

#$#3i
#i.	ZIP x Verification Status. A data set in which each observation is an 
#aggregation of all of the loans with the same combination of 
#(zip_code, verification_status).

##idea to add new columns with percentages.
#df2$loan_status > two categories "Fully Paid", "Current" a new column "loan_status2"
unique(df2$loan_status)
# [1] Fully Paid       ->   Fully Paid                                
# [2] Charged Off      ->   Current                                  
# [3]                  ->   Current                                  
# [4] Does not meet the credit policy. Status:Fully Paid    ->   Fully Paid 
# [5] Does not meet the credit policy. Status:Charged Off  ->   Current

df2$loan_status2 <- ifelse(df2$loan_status == "Does not meet the credit policy. Status:Fully Paid"
                           | df2$loan_status == "Fully Paid"
                          , "Fully Paid" 
                          , "Current")

df2$ls_FullyPaid <- ifelse(df2$loan_status2 == "Fully Paid", 1, 0)
df2$ls_Current <- ifelse(df2$loan_status2 == "Current", 1, 0)

head(df2)

#“aggregated” at ZIP x Verification Status
df_ag31 <- aggregate(cbind(loan_amnt, annual_inc, ls_FullyPaid, ls_Current) 
                     ~ zip_code + verification_status, FUN = sum, data = df2)

#fractions for loan status
total_loans <- df_ag31$ls_FullyPaid + df_ag31$ls_Current
df_ag31$ls_FullyPaid <- df_ag31$ls_FullyPaid/total_loans
df_ag31$ls_Current <- df_ag31$ls_Current/total_loans


head(df_ag31, 10) #final with   loan Status

#$#3ii
#ii.	ZIP x Verification Status x Term x Subgrade. A data set in which each 
#observation is an aggregation of all of the loans with the same combination of 
#(zip_code, verification_status, term, sub_grade).
df_ag32 <- aggregate(cbind(loan_amnt, annual_inc, ls_FullyPaid, ls_Current) 
                     ~ zip_code + verification_status 
                     + term + sub_grade , FUN = sum, data = df2)

#loan status
total_loans <- df_ag32$ls_FullyPaid + df_ag32$ls_Current
df_ag32$ls_FullyPaid <- df_ag32$ls_FullyPaid/total_loans
df_ag32$ls_Current <- df_ag32$ls_Current/total_loans

head(df_ag32,100) #final


#$#
#Some guidance on how to aggregate within each group. 
#For the continuous variables, compute the average within each group.  
#For loan_status, compute two fractions: the fraction of loans that are current, 
#and the fraction of loans that are fully paid. For home_ownership, 
#compute the fraction of loans with a homeowner.  For purpose, 
#compute fraction of debt consolidations.















###global adjustments start 2
options(warn = oldwarnval) #reset the warning message
###global adjustments end 2

