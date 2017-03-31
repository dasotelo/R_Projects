##############################
# 
#  Mapping CA Hospitals by Volume and
#  Medicare Charge Amount as a Percent
#  of Medicare Payments
#
##############################
#
#  Author: Dave Sotelo
#  Date: March 28, 2017
#
##############################
#
#  Source: CMS.GOV
#  Inpatient Charge Data, FY2014
#  https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Inpatient2014.html
#
##############################

library('ggplot2')
library('ggrepel')
library('rgdal')
library('rgeos')
library('ggthemes')
library("plyr")
library('ggmap')

setwd('C:/BB/CMS')

MCIPD<-read.csv('Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv',
  header=TRUE,stringsAsFactors=FALSE)

# Data Munge - Data is broken out by DRG category.  Aggregate to Hospital
# Level
MCIPD_CA<-MCIPD[MCIPD$Provider.State=='CA',]
Hosp_Info<-unique(MCIPD_CA[,2:7])
MCIPD_CA$Tot.Cov.Charges<-MCIPD_CA$Total.Discharges*MCIPD_CA$Average.Covered.Charges
MCIPD_CA$Tot.Payments<-MCIPD_CA$Total.Discharges*MCIPD_CA$Average.Total.Payments
MCIPD_CA$Tot.MCR.Payments<-MCIPD_CA$Total.Discharges*MCIPD_CA$Average.Medicare.Payments
Hosp_Measure<-MCIPD_CA[,c(2,9,13,14,15)]
Hosp_Meas_Agg<-aggregate(.~Provider.Id,Hosp_Measure,FUN=sum)
Hosp_Meas_Agg$Avg.Cov.Charge<-Hosp_Meas_Agg$Tot.Cov.Charges/Hosp_Meas_Agg$Total.Discharges
Hosp_Meas_Agg$Avg.MCR.Payment<-Hosp_Meas_Agg$Tot.MCR.Payments/Hosp_Meas_Agg$Total.Discharges
Hosp_MeasF<-Hosp_Meas_Agg[,c(1,2,6,7)]
MCIPD_CA_F<-merge.data.frame(Hosp_Info,Hosp_MeasF,by='Provider.Id')

MCIPD_CA_F<-MCIPD_CA_F[order(-MCIPD_CA_F$Total.Discharges),]
MCIPD_CA_F$Full.Address<-paste(MCIPD_CA_F$Provider.Street.Address,', ',MCIPD_CA_F$Provider.City,' ',MCIPD_CA_F$Provider.State,sep='')

# Identify Top 50 and get lat/lon from Data Science Toolkit
# www.datasciencetoolkit.org/
MCIPD_CA_T50<-MCIPD_CA_F[1:50,]
t<-geocode(MCIPD_CA_T50$Full.Address,output='latlon',source='dsk')
MCIPD_CA_T50$Latitude<-t$lat
MCIPD_CA_T50$Longitude<-t$lon
MCIPD_CA_T50$MCR.Percent<-(MCIPD_CA_T50$Avg.Cov.Charge/MCIPD_CA_T50$Avg.MCR.Payment)

# Read CA shapefile with county polygons, from US Census Bureau
ca.edt<-readOGR(dsn='.',layer='CA_counties')
ca.edt@data$id=rownames(ca.edt@data)
ca.edt.points=fortify(ca.edt,region='id')
ca.edt.df=join(ca.edt.points,ca.edt@data,by='id')

# Cut Catalina Island from shape dataframe
ca.edt.df=ca.edt.df[(ca.edt.df$lat>33.55)|(ca.edt.df$lon>-118),]

max_lat=max(MCIPD_CA_T50$Latitude)+0.1
min_lat=min(MCIPD_CA_T50$Latitude)-0.1
max_long=max(MCIPD_CA_T50$Longitude)+0.1
min_long=min(MCIPD_CA_T50$Longitude)-0.1
area.ca=(max_lat-min_lat)*(max_long-min_long)
height.ca=(max_lat-min_lat)
width.ca=(max_long-min_long)
height.scaler=10/height.ca

ggplot(ca.edt.df,aes(long,lat)) +
  geom_polygon(fill=rep('white',length(ca.edt.df$id)),aes(group=id),
    color=rgb(200,200,200,maxColorValue=255),size=.8) +
  geom_point(data=MCIPD_CA_T50,aes(x=MCIPD_CA_T50$Longitude,
    y=MCIPD_CA_T50$Latitude,size=(MCIPD_CA_T50$Total.Discharges**2),
    fill=MCIPD_CA_T50$MCR.Percent),shape=21,
    colour=rgb(55,55,55,maxColorValue=255),stroke=1.2) +
  geom_label_repel(data=MCIPD_CA_T50,aes(x=MCIPD_CA_T50$Longitude,
    y=MCIPD_CA_T50$Latitude,label=MCIPD_CA_T50$Provider.Name,
    fill=MCIPD_CA_T50$MCR.Percent),size=2,nudge_x=0.45,nudge_y=0.08,
    colour=rgb(0,0,0,maxColorValue=255),alpha=0.8,
    segment.colour=rgb(100,100,100,maxColorValue=255),
    label.padding=unit(0.13,'lines'),force=5) +
  scale_fill_gradient2(name='% MCR',
    low=rgb(25,0,210,maxColorValue=255),
    mid=rgb(255,250,0,maxColorValue=255),
    high=rgb(255,0,0,maxColorValue=255),
    midpoint=6,breaks=c(3,4,5,6,7,8,9,10),
    labels=c('300%','400%','500%','600%','700%','800%','900%','1000%'),
    guide='colourbar',space='Lab') +
  scale_size_area(max_size=170/(max_lat-min_lat)) +
  guides(size=FALSE) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle('Top 50 CA Hospitals by Medicare Discharges',
    subtitle=paste('Bubble Size: No. of Discharges, Bubble Color: Covered',
      ' Charges as % of Mcare Payments',sep='')) +
  coord_cartesian(xlim=c(min_long,max_long),ylim=c(min_lat,max_lat)) +
  theme_classic() +
  theme(legend.position='right',axis.line=element_blank(),
    axis.ticks=element_blank(),axis.text=element_blank(),
    panel.background=element_rect(fill='darkgrey',size=0.7,colour='black'),
    plot.title=element_text(size=16,face='bold.italic'))

