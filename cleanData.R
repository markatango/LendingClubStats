library(foreach)
library(iterators)
library(reshape2)
library(ggplot2)
library(plyr)
library(caret)
library(randomForest)

source("helpers.R")

# sampledPHData <- read.csv("D:/LendingClubData/PMTHIST_INVESTOR_20150505_v3_sample_5000.csv",
#                    colClasses="character",
#                    header=TRUE,
#                    stringsAsFactors=FALSE, 
#                    na.strings=c("NA","","MISSING"))
# temporary
#sampledPHData <- sampledPHData[,-1]

# reclass columns
numericColumns <- c("PBAL_BEG_PERIOD_INVESTORS", 
                    "PRNCP_PAID_INVESTORS",
                    "INT_PAID_INVESTORS",
                    "FEE_PAID_INVESTORS",
                    "DUE_AMT_INVESTORS",
                    "RECEIVED_AMT_INVESTORS",
                    "PBAL_END_PERIOD_INVESTORS", 
                    "MONTHLYPAYMENT_INVESTORS",
                    "COAMT_INVESTORS",
                    "InterestRate",
                    "dti",
                    "MonthlyIncome",
                    "OpenCREDITLines",
                    "TotalCREDITLines",
                    "RevolvingCREDITBalance",
                    "RevolvingLineUtilization",
                    "Inquiries6M",
                    "DQ2yrs",
                    "MonthsSinceDQ",
                    "PublicRec",
                    "MonthsSinceLastRec",
                    "term",
                    "PCO_RECOVERY_INVESTORS",
                    "PCO_COLLECTION_FEE_INVESTORS")

dateColumns <- c("RECEIVED_D",
                 "Month",
                 "IssuedDate")

numericColInd <- which(names(sampledPHData) %in% numericColumns)
dateColInd <- which(names(sampledPHData) %in% dateColumns)
factorColInd <- setdiff(1:dim(sampledPHData)[2],union(numericColInd,dateColInd))
factorVarNames <- names(sampledPHData)[factorColInd]
sampledPHData[,factorColInd] <- lapply(sampledPHData[,factorColInd],as.character)
sampledPHData[,numericColumns] <- lapply(sampledPHData[,numericColumns],as.numeric)

# add features
# add min and max FICO stores for each pay period
extractMinFICO <- function(x){
  unlist(strsplit(as.character(x),split="-",fixed=TRUE))[1]
}
extractMaxFICO <- function(x){
  unlist(strsplit(as.character(x),split="-",fixed=TRUE))[2]
}

sampledPHData$minLastFico <- sapply(sampledPHData$Last_FICO_BAND,extractMinFICO)
sampledPHData$maxLastFico <- sapply(sampledPHData$Last_FICO_BAND,extractMaxFICO)

# these produce NAs so ignore warnings
sampledPHData$minLastFico <- as.numeric(sampledPHData$minLastFico)
sampledPHData$maxLastFico <- as.numeric(sampledPHData$maxLastFico)

# do not convert to Date class prior to melting
# prePend1 <- function(ch){
#   paste0("1",ch)
# }
# sampledPHData[,dateColumns] <- lapply(sampledPHData[,dateColumns],prePend1)
# sampledPHData[,dateColumns] <- lapply(sampledPHData[,dateColumns],strptime,format="%e%b%Y")

# end reclassing

# add features and tidy data
# income to loan ratio
sampledPHData$incToPaymentRatio <- sampledPHData$MonthlyIncome / sampledPHData$DUE_AMT_INVESTORS



