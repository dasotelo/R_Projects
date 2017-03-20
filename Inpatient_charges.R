##################################
#
#  Comparison of Medicare Avg Covered Charges, by US City
#  Date: 20170320
#  Author: Dave Sotelo
#  sotelo.d.a@gmail.com
#
##################################
#
#  Dataset available from data.gov, under 'Health' section
#
##################################

library("sqldf")
library("ggplot2")
library("scales")
library("ggthemes")

# setwd(@this is the name of your working directory@)

# Read in data and convert dollar value text strings to numerics
ipchrg<-read.csv("inpatientCharges.csv",header=TRUE,stringsAsFactors=FALSE)
ipchrg$Avg.Cov.Chrg<-as.numeric(sub('\\$','',ipchrg$Average.Covered.Charges))
ipchrg$Avg.Tot.Pmt<-as.numeric(sub('\\$','',ipchrg$Average.Total.Payments))
ipchrg$Avg.Mcare.Pmt<-as.numeric(sub('\\$','',ipchrg$Average.Medicare.Payments))
ipchrg<-ipchrg[,-c(10:12)]

ipchrg_reg<-ipchrg[,c('DRG.Definition','Hospital.Referral.Region.Description'
                      ,'Provider.Id','Total.Discharges','Avg.Cov.Chrg'
                      ,'Avg.Tot.Pmt','Avg.Mcare.Pmt')]

# First faceted histogram plot
ipchrg_reg_maj1<-ipchrg_reg[ipchrg_reg$Hospital.Referral.Region.Description %in% 
                           c('CA - San Francisco','NY - Manhattan','IL - Chicago',
                             'DC - Washington','TX - Houston','WA - Seattle',
                             'MO - St. Louis','OH - Cleveland','CA - Los Angeles',
                             'FL - Miami','MA - Boston','MN - Minneapolis',
                             'GA - Atlanta','CT - Hartford','AZ - Phoenix',
                             'LA - New Orleans') & 
                             ipchrg_reg$DRG.Definition=='690 - KIDNEY & URINARY TRACT INFECTIONS W/O MCC',]

ggplot(ipchrg_reg_maj1,aes(x=ipchrg_reg_maj1$Avg.Cov.Chrg)) +
  geom_histogram(bins=50,fill=rgb(75,172,198,maxColorValue=255),
    colour=rgb(50,114,198,maxColorValue=255)) +
  labs(x='Avg. Covered Charge Amount',y='Count of Hospitals',title='Kidney & UT Infections - No Complications') +
  scale_x_continuous(breaks=c(0,20000,40000),labels=dollar,limits=c(0,50001)) +
  scale_y_continuous(breaks=c(2,4,6,8,10,12)) +
  theme_minimal() +
  facet_wrap(~ipchrg_reg_maj1$Hospital.Referral.Region.Description) +
  theme(strip.text=element_text(colour='darkgray',size=9,hjust=0),axis.text=element_text(size=6),
    plot.title=element_text(size=13,face='bold'),axis.title=element_text(size=9))

# Second faceted histogram plot
ipchrg_reg_maj2<-ipchrg_reg[ipchrg_reg$Hospital.Referral.Region.Description %in% 
                              c('CA - San Francisco','NY - Manhattan','IL - Chicago',
                                'DC - Washington','TX - Houston','WA - Seattle',
                                'MO - St. Louis','OH - Cleveland','CA - Los Angeles',
                                'FL - Miami','MA - Boston','MN - Minneapolis',
                                'GA - Atlanta','CT - Hartford','AZ - Phoenix',
                                'LA - New Orleans') & 
                              ipchrg_reg$DRG.Definition=='470 - MAJOR JOINT REPLACEMENT OR REATTACHMENT OF LOWER EXTREMITY W/O MCC',]

ggplot(ipchrg_reg_maj2,aes(x=ipchrg_reg_maj2$Avg.Cov.Chrg)) +
  geom_histogram(bins=50,fill=rgb(125,201,125,maxColorValue=255),
                 colour=rgb(48,136,44,maxColorValue=255)) +
  labs(x='Avg. Covered Charge Amount',y='Count of Hospitals',title='Major Joint Replacement/Limb Reattachment - No Complications') +
  scale_x_continuous(breaks=c(0,30000,60000,90000),labels=dollar,limits=c(0,100001)) +
  scale_y_continuous(breaks=c(2,4,6,8,10,12)) +
  theme_minimal() +
  facet_wrap(~ipchrg_reg_maj2$Hospital.Referral.Region.Description) +
  theme(strip.text=element_text(colour='darkgray',size=9,hjust=0),axis.text=element_text(size=6),
    plot.title=element_text(size=13,face='bold'))