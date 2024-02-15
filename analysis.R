#Start with Kent Survey data

kentbirds<-read.csv(file="data/kentsurvey.csv", header=TRUE, stringsAsFactors=FALSE, 
                    na.strings=c(NA, "NA","N/A","na", "n/a", "Na") )

#view how R interpreted variable names
names(kentbirds)
str(kentbirds)

######data cleaning

#get date into ISO format
library(lubridate)
kentbirds$ISOdate<-mdy(kentbirds$Date)


#clean up the yes and no data from the survey- replace yes with 1, no with 0

kentbirds[5:12]<-lapply(kentbirds[5:12], gsub, pattern="Yes", replacement=1)
kentbirds[5:12]<-lapply(kentbirds[5:12], gsub, pattern="yes", replacement=1)
kentbirds[5:12]<-lapply(kentbirds[5:12], gsub, pattern="No", replacement=0)
kentbirds[5:12]<-lapply(kentbirds[5:12], gsub, pattern="no", replacement=0)
kentbirds[5:12]<-lapply(kentbirds[5:12],as.numeric)


str(kentbirds)


#use these data to tally up the number of birds per day and observations per day

kentbirds$dailyobs<-rowSums(!is.na(kentbirds[5:12]))
kentbirds$dailytot<-rowSums(kentbirds[5:12], na.rm=T)


#plot observations per building
#plot cumulative observations over season


