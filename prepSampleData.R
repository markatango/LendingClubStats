library(foreach)
library(iterators)
phData <- read.csv("D:/LendingClubData/PMTHIST_INVESTOR_20150505_v3.csv",
                   colClasses="character",
                   header=TRUE,
                   stringsAsFactors=FALSE)

# split the dataframe into a list of dataframes by LOAN_ID value
lphData <- split(phData,phData["LOAN_ID"])
rm(phData) # free up some memory

# sample the list
set.seed(1340)
SAMPLE_SIZE <- 5000

lphData.len <- length(lphData)
u <- sample(1:lphData.len, SAMPLE_SIZE)
shortList <-lphData[u]
rm(lphData)
rm(lphData.len)
rm(u)

# combine the shorter list of dataframes back into a single dataframe and store it for later use
sampledPHData <- foreach(b=iter(shortList),.combine=rbind) %do% b
rm(b)
rm(shortList)
write.csv(sampledPHData,
          paste0( "D:/LendingClubData/PMTHIST_INVESTOR_20150505_v3_sample_",SAMPLE_SIZE,".csv"),
          row.names=FALSE)



