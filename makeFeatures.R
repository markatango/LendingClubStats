library(foreach)
library(iterators)
library(reshape2)
library(ggplot2)
library(plyr)
library(caret)
library(randomForest)

source("helpers.R")

# create feature dataset
sphd.dd <- ddply(sampledPHData,.(LOAN_ID),summarize,
                 noNCS=sum( ( (PERIOD_END_LSTAT!="Current")&(PERIOD_END_LSTAT!="Fully Paid") )*1),
                 chargedOff = "Charged Off" %in% PERIOD_END_LSTAT,
                 noFicoBands = length(unique(Last_FICO_BAND)),
                 minFicoScore = min(correctForAllAreNA(minLastFico), na.rm=TRUE),
                 grade=mode.2(grade)[[1]],
                 state=mode.2(State)[[1]],
                 homeOwnership=mode.2(HomeOwnership)[[1]],
                 earlyCreditLine=as.character(mode.2(EarliestCREDITLine))[[1]],
                 interestRate=as.numeric(mode.1(InterestRate))[[1]],
                 incToPaymentRatio=as.numeric(mode.1(incToPaymentRatio)[[1]]),
                 term=as.numeric(mode.1(term))[[1]],
                 openCreditLines=as.numeric(mode.1(OpenCREDITLines)[[1]]),
                 totalCreditLines=as.numeric(mode.1(TotalCREDITLines)[[1]]),
                 revolvingLineUtilization=as.numeric(mode.1(RevolvingLineUtilization)[[1]]),
                 revolvingCreditBalance=as.numeric(mode.1(RevolvingCREDITBalance)[[1]]),
                 monthsSinceDQ=as.numeric(mode.1(MonthsSinceDQ)[[1]]),
                 goodMonths= countContOK(PERIOD_END_LSTAT)
)

# add the period to period change in fico scores
fs24 <- ficoSlopes2(24)

sphd.dd <- cbind(sphd.dd, ddply(sampledPHData,.(LOAN_ID),function(x)fs24(x[,"minLastFico"])$res)[,-1])
sphd.dd <- cbind(sphd.dd, ddply(sampledPHData,.(LOAN_ID),function(x)fs24(x[,"minLastFico"])$cum)[,-1])


# add labels for condition of loan
u.ncs <- intersect(which(sampledPHData[,"PERIOD_END_LSTAT"]!="Current"),
                   which(sampledPHData[,"PERIOD_END_LSTAT"]!="Fully Paid")
)

WithNonCurrentStatus <- sampledPHData[u.ncs,]
rm(u.ncs)

# unique ID sets
# all IDs
IDs <- unique(sampledPHData[,"LOAN_ID"])

# IDS that have had at least one non-current entry of any type
IDs.ncs <- unique(WithNonCurrentStatus[,"LOAN_ID"])

# IDs that resulted in a "Charged Off" condition
# these are unique by default since only one entry per unique LOAN_ID is labeled "charged off"
u.co <-which(sampledPHData[,"PERIOD_END_LSTAT"]=="Charged Off")
IDs.co <- sampledPHData[u.co,"LOAN_ID"]
rm(u.co)

# IDs that have had some kind of non-current status but did not result in "Charged Off"
IDs.ncsNOTco <- unique(setdiff(IDs.ncs, IDs.co))

# IDs with no non-current status at all
IDs.noNCS <- unique(setdiff(unique(IDs),IDs.ncs))

# create labeled unique ID list
IDs.noNCS.df <- data.frame(LOAN_ID=IDs.noNCS, label="noNCS", stringsAsFactors=FALSE)
IDs.ncsNOTco.df <- data.frame(LOAN_ID=IDs.ncsNOTco, label="ncsNOTco", stringsAsFactors=FALSE)
IDs.co.df <- data.frame(LOAN_ID=IDs.co, label="co", stringsAsFactors=FALSE)
IDs.all <- rbind(IDs.noNCS.df, IDs.ncsNOTco.df, IDs.co.df)
IDs.all <- IDs.all[order(IDs.all[,"LOAN_ID"]),]
rm(IDs, IDs.ncs, IDs.co, IDs.ncsNOTco, IDs.noNCS, IDs.noNCS.df, IDs.ncsNOTco.df, IDs.co.df)

sphd.dd <- merge(sphd.dd, IDs.all, by="LOAN_ID")
rm(IDs.all)

