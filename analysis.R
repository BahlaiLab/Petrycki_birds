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
kentbirds$birds.per.building<-kentbirds$dailytot/kentbirds$dailyobs


#exploratory plots
library(ggplot2)
library(tidyr)
library(plyr)

#R color chart here: https://derekogle.com/NCGraphing/resources/colors

#plot cumulative observations per building
ggplot(kentbirds, aes(x=ISOdate, y=cumsum(replace_na(dailytot, 0))))+geom_line()+ 
  #all the cumulative functions require replace_na(x,0) because they choke on NAs
  geom_line(aes(x=ISOdate, y=cumsum(replace_na(Cunningham, 0))), color="magenta")+
  geom_line(aes(x=ISOdate, y=cumsum(replace_na(ISB, 0))), color="green")+
  geom_line(aes(x=ISOdate, y=cumsum(replace_na(Taylor, 0))), color="darkorange")+
  #add in all the buildings here
  theme_classic()+
  ylab("Cumulative Observations")+xlab("Date")

#raw counts by survey date. line is birds per building surveyed
ggplot(kentbirds, aes(x=ISOdate, y=dailytot))+geom_point()+ #these are the points
  geom_line(aes(x=ISOdate, y=birds.per.building))+#this is the line
  theme_classic()+
  scale_y_continuous(    # Features of the first axis
    name = "Total observations",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name="Average observation by building"))+xlab("Date")


#plot observations by observer
#when I was looking up the code for this pie chart I found this linked in the tutorial: https://www.data-to-viz.com/caveat/pie.html
#whatever, I'm doing it

observer.table<-ddply(kentbirds, "Observer.Name", summarize, birds=sum(dailytot), buildings=sum(dailyobs))
observer.table$Observer.Name<-gsub("/", " and", observer.table$Observer.Name)

library(RColorBrewer)
myPalette <- brewer.pal(7, "Set2") 

pie(observer.table$birds, labels=observer.table$Observer.Name, col=myPalette, main="Proportion of birds observed") #number of birds seen
pie(observer.table$buildings, labels=observer.table$Observer.Name, col=myPalette, main="Proportion of sampling effort") #number of buildings surveyed



#plot observations by species


#plot observations by wind


#plot observations by sun

#plot by temperature
ggplot(kentbirds, aes(x=(Temperature..F.-32)*5/9, y=birds.per.building))+geom_point()+ 
  geom_smooth(method="lm")+
  theme_classic()+
  scale_y_continuous(name = "Events per building per day")+xlab("Temperature (\u00B0C)")

#counts by vegetation rating



