############################################
#
#    Exploring Kaggle Seattle AirBnB Data
#    Author: Dave Sotelo
#    sotelo.d.a@gmail.com
#    February 26th, 2017
#
############################################
#
#    Data Source: Scrape of AirBnB.com, via Kaggle
#
#    Seattle map shapefile from US Census Bureau
#    2010 Census - 5-Digit Zip Code Tabulation Area (zcta) shapefile
#    Washington State
#
############################################

library("tmap")
library("sqldf")
library("rgdal")
library("rgeos")

### Read and scrub shapefile data

zcta510<-readOGR(dsn=".",layer="zcta510",stringsAsFactors=FALSE)
sea_zips<-zcta510[((zcta510$ZCTA5CE10>=98101 & zcta510$ZCTA5CE10<=98144) | zcta510$ZCTA5CE10==98154 | zcta510$ZCTA5CE10==98164 | zcta510$ZCTA5CE10==98174 | zcta510$ZCTA5CE10==98177 | zcta510$ZCTA5CE10==98195 | zcta510$ZCTA5CE10==98199) & zcta510$ZCTA5CE10!=98110,]
sea_zips2<-sea_zips[,c("ZCTA5CE10","POP10")]
rm(sea_zips)
sea_zips<-sea_zips2
rm(sea_zips2)

zipkey<-data.frame(zip5=c("98101","98102","98103","98104","98105","98106","98107","98108","98109","98112","98115","98116","98117","98118","98119","98121","98122","98125","98126","98133","98134","98136","98144","98154","98164","98174","98177","98195","98199"),neighborhood=c("Downtown-Denny-Capitol Hill","East Lake","Wallingford-Green Lake","Downtown-Denny-Capitol Hill","University","Delridge","Ballard","Georgetown","Queen Anne","Madison Park","Northeast","West Seattle","Ballard","Rainier-Seward Park","Queen Anne","Downtown-Denny-Capitol Hill","Central District","Northeast","West Seattle","Northwest","SoDo","West Seattle","Central District","Downtown-Denny-Capitol Hill","Downtown-Denny-Capitol Hill","Downtown-Denny-Capitol Hill","Northwest","University","Magnolia"))
sea_zips2<-merge(sea_zips,zipkey,by.x="ZCTA5CE10",by.y="zip5")
rm(sea_zips)
sea_zips<-sea_zips2
rm(sea_zips2)

census_data_zip<-sea_zips@data
sea_ngh<-gUnaryUnion(sea_zips,id=sea_zips@data$neighborhood)
dmy<-data.frame(paste("obs",as.character(1:16)))
sea_ngh_df<-SpatialPolygonsDataFrame(sea_ngh,dmy,match.ID=FALSE)
rm(dmy)
rm(sea_ngh)
sea_ngh_df@data$neighborhood<-c("Ballard","Central District","Delridge","Downtown-Denny-Capitol Hill","East Lake","Georgetown","Madison Park","Magnolia","Northeast","Northwest","Queen Anne","Rainier-Seward Park","SoDo","University","Wallingford-Green Lake","West Seattle")
sea_ngh_df<-sea_ngh_df[,2]

### Read, scrub & aggregate AirBnB listings rate data

listings<-read.csv("listings.csv",header=TRUE,stringsAsFactors=FALSE)
lst_attr<-listings[,c("id","host_id","zipcode","property_type","room_type","accommodates","price","bedrooms","minimum_nights","number_of_reviews","review_scores_rating")]
occup_data<-read.csv("calendar.csv",header=TRUE,stringsAsFactors=FALSE)

lst_attr2<-merge(lst_attr,zipkey,by.x="zipcode",by.y="zip5")
rm(lst_attr)
lst_attr<-lst_attr2
rm(lst_attr2)
lst_attr$price2<-as.numeric(sub('\\$','',as.character(lst_attr$price)))

### Merge listing data and Seattle spatial polygons data

lst_attr_m<-sqldf("select neighborhood,sum(accommodates) accommodates,sum(bedrooms) bedrooms,sum(price2) price from lst_attr group by neighborhood")
sea_ngh_df2<-merge(sea_ngh_df,lst_attr_m,by.x="neighborhood",by.y="neighborhood",all.x=TRUE)
rm(sea_ngh_df)
sea_ngh_df<-sea_ngh_df2
rm(sea_ngh_df2)

census_data_m<-sqldf("select neighborhood,sum(POP10) pop_10_census from census_data_zip group by neighborhood")
sea_ngh_df2<-merge(sea_ngh_df,census_data_m,by.x="neighborhood",by.y="neighborhood",all.x=TRUE)
rm(sea_ngh_df)
sea_ngh_df<-sea_ngh_df2
rm(sea_ngh_df2)

sea_ngh_df@data$price2<-as.numeric(sea_ngh_df@data$price)
sea_ngh_df<-sea_ngh_df[,-4]

### Create summary statistics

sea_ngh_df@data$avg_bdr_price<-sea_ngh_df@data$price2/sea_ngh_df@data$bedrooms
sea_ngh_df@data$avg_bdr_p100<-sea_ngh_df@data$bedrooms*100/sea_ngh_df@data$pop_10_census
sea_ngh_df@data$avg_accom_price<-sea_ngh_df@data$price2/sea_ngh_df@data$accommodates
sea_ngh_df@data$avg_accom_per_bed<-sea_ngh_df@data$accommodates/sea_ngh_df@data$bedrooms
sea_ngh_df@data$ngh_abbrev<-c("Bal","CnD","Del","Dwn","ELk","Grg","Mad","Mgn","NE","NW","QAn","RSP","SDo","Uni","WGL","WS")

### Side-by-side chloropleth maps

tm_shape(sea_ngh_df) +
tm_polygons(c("bedrooms","avg_bdr_price"),palette=list("Greens","Purples"),n=9,title=c("Bedrooms","Price per Bdrm")) +
tm_layout(legend.show=TRUE,legend.position=c("RIGHT","TOP"),legend.bg.color="gray",legend.width=-.48,legend.frame=TRUE,frame=FALSE,legend.text.size=.5,legend.title.size=.8) +
tm_text(c("ngh_abbrev","ngh_abbrev"),size=.5,legend.size.show=FALSE) +
tm_style_beaver()

tm_shape(sea_ngh_df) +
tm_polygons(c("bedrooms","avg_bdr_p100"),palette=list("Greens","Reds"),n=9,title=c("Bedrooms","Bdrms per 100")) +
tm_layout(legend.show=TRUE,legend.position=c("RIGHT","TOP"),legend.bg.color="gray",legend.width=-.48,legend.frame=TRUE,frame=FALSE,legend.text.size=.5,legend.title.size=.8) +
tm_text(c("ngh_abbrev","ngh_abbrev"),size=.5,legend.size.show=FALSE) +
tm_style_beaver()

### Plot average review score on annual availability rate

occup_agg<-sqldf("select listing_id,available,count(listing_id) status_count from occup_data group by listing_id,available")
occup_agg<-occup_agg[occup_agg$available=="t",]
occup_agg$avlbl_per<-occup_agg$status_count/365
occup_agg<-occup_agg[,c(1,4)]
lst_attr<-merge(lst_attr,occup_agg,by.x="id",by.y="listing_id",all.x=TRUE)
avlbl_on_rvw<-lst_attr[,c(1,11,13,14)]

smoothScatter(avlbl_on_rvw$avlbl_per,avlbl_on_rvw$review_scores_rating,xlab=list("Percent of Yr Available",cex=0.8),ylab=list("Avg. Review Score",cex=0.8),main=list("Average Review Score on Availability Rate",cex=0.9))
avlbl_lm<-lm(avlbl_on_rvw$review_scores_rating~avlbl_on_rvw$avlbl_per)
abline(avlbl_lm,col="red")

avlbl_on_rvw$transf_rvw_sc<-(avlbl_on_rvw$review_scores_rating/100)^8*100
smoothScatter(avlbl_on_rvw$avlbl_per,avlbl_on_rvw$transf_rvw_sc,xlab=list("Percent of Yr Available",cex=0.8),ylab=list("Transformed Avg. Review Score",cex=0.8),main=list("Transformed Average Review Score on Availability Rate",cex=0.9))
avlbl_lm2<-lm(avlbl_on_rvw$transf_rvw_sc~avlbl_on_rvw$avlbl_per)
abline(avlbl_lm2,col="purple")