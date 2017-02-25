##############################
#
#	Analysis of MLB Pitcher 'Down Year'
#	Author: Dave Sotelo
#	sotelo.d.a@gmail.com
#	February 6th, 2017
#
##############################
#
#	Data Source: Retrosheet.org event files
#	1974-2016
#
#	Retrosheet data must first be extracted using BEVENT.exe tool
#
#	Retrosheet fields to extract: 0,14,34,40,58-61,75-77
#
#	Save extracted files to R working directory using naming convention:
#	'01_datayy.csv' where yy is the 2-digit season year (e.g. 2005=05)
#
##############################


### Read in extracted event files:

aa_data<-""
input.files<-c("01_data74.csv","01_data75.csv","01_data76.csv","01_data77.csv","01_data78.csv","01_data79.csv","01_data80.csv",
"01_data81.csv","01_data82.csv","01_data83.csv","01_data84.csv","01_data85.csv","01_data86.csv","01_data87.csv","01_data88.csv",
"01_data89.csv","01_data90.csv","01_data91.csv","01_data92.csv","01_data93.csv","01_data94.csv","01_data95.csv","01_data96.csv",
"01_data97.csv","01_data98.csv","01_data99.csv","01_data00.csv","01_data01.csv","01_data02.csv","01_data03.csv","01_data04.csv",
"01_data05.csv","01_data06.csv","01_data07.csv","01_data08.csv","01_data09.csv","01_data10.csv","01_data11.csv","01_data12.csv",
"01_data13.csv","01_data14.csv","01_data15.csv","01_data16.csv")
for(i in 1:length(input.files)){
file<-read.csv(input.files[i],header=FALSE,stringsAsFactors=FALSE)
aa_data<-rbind(aa_data,file)}
names(aa_data)<-c("game.id","c.pitcher","event","outs.on.play","b.dest","f.dest","s.dest","t.dest","f.pitcher","s.pitcher","t.pitcher")
ab_data<-aa_data[-1,]
rm(aa_data)
rm(file)

### Data manipulation and creation of cumulative stats

ab_data$year<-substr(ab_data$game.id,start=4,stop=7)
ab_data$game.id<-NULL
ab_data$walks<-ifelse(ab_data$event==14,1,0)
ab_data$hits<-ifelse(ab_data$event>=20&ab_data$event<=23,1,0)
ab_data$c.earned.runs<-ifelse(ab_data$b.dest==4,1,0)
ab_data$f.earned.runs<-ifelse(ab_data$f.dest==4,1,0)
ab_data$s.earned.runs<-ifelse(ab_data$s.dest==4,1,0)
ab_data$t.earned.runs<-ifelse(ab_data$t.dest==4,1,0)
ab_pitch.stats<-sqldf("select [c.pitcher],[f.pitcher],[s.pitcher],[t.pitcher],year,sum([outs.on.play]) outs,sum(walks) walks,sum(hits) hits,sum([c.earned.runs]) [c.earned.runs],sum([f.earned.runs]) [f.earned.runs],sum([s.earned.runs]) [s.earned.runs],sum([t.earned.runs]) [t.earned.runs] from ab_data group by [c.pitcher],[f.pitcher],[s.pitcher],[t.pitcher],year")
rm(ab_data)
ac_pitch.stats.c<-sqldf("select [c.pitcher] [pitcher.ID],year,sum(outs) outs,sum(walks) walks,sum(hits) hits, sum([c.earned.runs]) [earned.runs] from [ab_pitch.stats] group by [c.pitcher],year")
ac_pitch.stats.f<-sqldf("select [f.pitcher] [pitcher.ID],year,sum([f.earned.runs]) [earned.runs] from [ab_pitch.stats] group by [f.pitcher],year")
ac_pitch.stats.s<-sqldf("select [s.pitcher] [pitcher.ID],year,sum([s.earned.runs]) [earned.runs] from [ab_pitch.stats] group by [s.pitcher],year")
ac_pitch.stats.t<-sqldf("select [t.pitcher] [pitcher.ID],year,sum([t.earned.runs]) [earned.runs] from [ab_pitch.stats] group by [t.pitcher],year")
ad_pitch.stats<-sqldf("select a.[pitcher.ID],a.year,a.outs,a.walks,a.hits,(a.[earned.runs]+b.[earned.runs]+c.[earned.runs]+d.[earned.runs]) [earned.runs] from [ac_pitch.stats.c] a left join [ac_pitch.stats.f] b on a.[pitcher.ID]=b.[pitcher.ID] and a.year=b.year left join [ac_pitch.stats.s] c on a.[pitcher.ID]=c.[pitcher.ID] and a.year=c.year left join [ac_pitch.stats.t] d on a.[pitcher.ID]=d.[pitcher.ID] and a.year=d.year")
ae_pitch.stats<-ad_pitch.stats[order(ad_pitch.stats$pitcher.ID,ad_pitch.stats$year),]
ae_pitch.stats$earned.runs<-ifelse(is.na(ae_pitch.stats$earned.runs),0,ae_pitch.stats$earned.runs)
player.index<-read.table("players.txt",sep="\t",quote="",header=TRUE)
af_pitch.stats<-sqldf("select a.*,b.LAST as [last.nm],b.FIRST as [first.nm],b.DEBUT as [debut.dt] 
from [ae_pitch.stats] a left join [player.index] b on a.[pitcher.ID]=b.ID")
af_pitch.stats$innings.pitched<-af_pitch.stats$outs / 3
af_pitch.stats$outs<-NULL
af_pitch.stats$ERA<-(af_pitch.stats$earned.runs*9)/af_pitch.stats$innings.pitched
af_pitch.stats$WHIP<-(af_pitch.stats$walks+af_pitch.stats$hits)/af_pitch.stats$innings.pitched
af_pitch.stats$debut.yr<-substr(af_pitch.stats$debut.dt,7,10)
af_pitch.stats$debut.dt<-NULL
ag_pitch.stats<-af_pitch.stats[order(af_pitch.stats$pitcher.ID,af_pitch.stats$year),]
ag_pitch.stats$cuml.ip<-ave(ag_pitch.stats$innings.pitched,ag_pitch.stats$pitcher.ID,FUN=cumsum)
ag_pitch.stats$cuml.er<-ave(ag_pitch.stats$earned.runs,ag_pitch.stats$pitcher.ID,FUN=cumsum)
ag_pitch.stats$cuml.hits<-ave(ag_pitch.stats$hits,ag_pitch.stats$pitcher.ID,FUN=cumsum)
ag_pitch.stats$cuml.walks<-ave(ag_pitch.stats$walks,ag_pitch.stats$pitcher.ID,FUN=cumsum)
ag_pitch.stats$cuml.ip.m1<-ag_pitch.stats$cuml.ip-ag_pitch.stats$innings.pitched
ag_pitch.stats$cuml.ERA.m1<-((ag_pitch.stats$cuml.er-ag_pitch.stats$earned.runs)*9)/ag_pitch.stats$cuml.ip.m1
ag_pitch.stats$cuml.WHIP.m1<-(ag_pitch.stats$cuml.walks-ag_pitch.stats$walks+ag_pitch.stats$cuml.hits-ag_pitch.stats$hits)/ag_pitch.stats$cuml.ip.m1
ag_pitch.stats$ERA.var<-ag_pitch.stats$ERA/ag_pitch.stats$cuml.ERA.m1
ag_pitch.stats$WHIP.var<-ag_pitch.stats$WHIP/ag_pitch.stats$cuml.WHIP.m1
ag_pitch.stats$ERA.down.yr<-ifelse(ag_pitch.stats$innings.pitched<45|ag_pitch.stats$cuml.ip.m1<45,"exclude",
ifelse(ag_pitch.stats$ERA.var>=2,"Down Year","Normal Year"))
ag_pitch.stats$WHIP.down.yr<-ifelse(ag_pitch.stats$innings.pitched<45|ag_pitch.stats$cuml.ip.m1<45,"exclude",
ifelse(ag_pitch.stats$WHIP.var>=1.3,"Down Year","Normal Year"))
ag_pitch.stats$counter<-rep(1,each=nrow(ag_pitch.stats))
ag_pitch.stats$mlb.yrs<-ave(ag_pitch.stats$counter,ag_pitch.stats$pitcher.ID,FUN=cumsum)
ag_pitch.stats$counter<-NULL
down.yr.f<-ag_pitch.stats

### Produce down year by mlb data frames and other results objects

results.dy.ERA.cnt<-sqldf("select [ERA.down.yr],count([pitcher.ID]) as count from [down.yr.f] group by [ERA.down.yr]")
results.dy.WHIP.cnt<-sqldf("select [WHIP.down.yr],count([pitcher.ID]) as count from [down.yr.f] group by [WHIP.down.yr]")
results.dy.ERA.by.mlb.yrs<-sqldf("select [mlb.yrs],count([pitcher.ID]) as [dy.count] from [down.yr.f] where [ERA.down.yr]='Down Year' group by [mlb.yrs]")
results.ny.ERA.by.mlb.yrs<-sqldf("select [mlb.yrs],count([pitcher.ID]) as [ny.count] from [down.yr.f] where [ERA.down.yr]='Normal Year' group by [mlb.yrs]")
results.ERA.by.mlb.yrs<-sqldf("select a.[mlb.yrs],a.[ny.count],b.[dy.count] from [results.ny.ERA.by.mlb.yrs] a left join [results.dy.ERA.by.mlb.yrs] b on a.[mlb.yrs]=b.[mlb.yrs]")
results.ERA.by.mlb.yrs$proportion<-results.ERA.by.mlb.yrs$dy.count/(results.ERA.by.mlb.yrs$ny.count+results.ERA.by.mlb.yrs$dy.count)
results.ERA.by.mlb.yrs$propor.x1000<-results.ERA.by.mlb.yrs$proportion*1000
results.ERA.by.mlb.yrs$proportion<-NULL
results.dy.WHIP.by.mlb.yrs<-sqldf("select [mlb.yrs],count([pitcher.ID]) as [dy.count] from [down.yr.f] where [WHIP.down.yr]='Down Year' group by [mlb.yrs]")
results.ny.WHIP.by.mlb.yrs<-sqldf("select [mlb.yrs],count([pitcher.ID]) as [ny.count] from [down.yr.f] where [WHIP.down.yr]='Normal Year' group by [mlb.yrs]")
results.WHIP.by.mlb.yrs<-sqldf("select a.[mlb.yrs],a.[ny.count],b.[dy.count] from [results.ny.WHIP.by.mlb.yrs] a left join [results.dy.WHIP.by.mlb.yrs] b on a.[mlb.yrs]=b.[mlb.yrs]")
results.WHIP.by.mlb.yrs$proportion<-results.WHIP.by.mlb.yrs$dy.count/(results.WHIP.by.mlb.yrs$ny.count+results.WHIP.by.mlb.yrs$dy.count)
results.WHIP.by.mlb.yrs$propor.x1000<-results.WHIP.by.mlb.yrs$proportion*1000
results.WHIP.by.mlb.yrs$proportion<-NULL
rm(results.dy.ERA.by.mlb.yrs,results.dy.WHIP.by.mlb.yrs,results.ny.ERA.by.mlb.yrs,results.ny.WHIP.by.mlb.yrs)
subset.dyERA.only<-subset(down.yr.f,ERA.down.yr=="Down Year")
subset.dyWHIP.only<-subset(down.yr.f,WHIP.down.yr=="Down Year")
subset.dyERA.p1<-sqldf("select a.* from [down.yr.f] a join [subset.dyERA.only] b on a.[pitcher.ID]=b.[pitcher.ID] and a.year=(b.year+1)")
subset.dyWHIP.p1<-sqldf("select a.* from [down.yr.f] a join [subset.dyWHIP.only] b on a.[pitcher.ID]=b.[pitcher.ID] and a.year=(b.year+1)")
results.ERA.by.mlb.yrs$lm.weights<-(results.ERA.by.mlb.yrs$ny.count+results.ERA.by.mlb.yrs$dy.count)^(0.5)
results.WHIP.by.mlb.yrs$lm.weights<-(results.WHIP.by.mlb.yrs$ny.count+results.WHIP.by.mlb.yrs$dy.count)^(0.5)

### plot results and create other output

barplot(results.ERA.by.mlb.yrs$dy.count,names.arg=results.ERA.by.mlb.yrs$mlb.yrs,main="Down Year Count - ERA",xlab="MLB Year")
barplot(results.WHIP.by.mlb.yrs$dy.count,names.arg=results.WHIP.by.mlb.yrs$mlb.yrs,main="Down Year Count - WHIP",xlab="MLB Year")
plot(results.ERA.by.mlb.yrs$mlb.yrs[1:20],results.ERA.by.mlb.yrs$propor.x1000[1:20],xlab="MLB Years",ylab="Down Year Proportion x1000",main="Down Year Proportion - ERA")
plot(results.WHIP.by.mlb.yrs$mlb.yrs[1:20],results.WHIP.by.mlb.yrs$propor.x1000[1:20],xlab="MLB Years",ylab="Down Year Proportion x1000",main="Down Year Proportion - WHIP")
symbols(results.ERA.by.mlb.yrs$mlb.yrs[1:20],results.ERA.by.mlb.yrs$propor.x1000[1:20],circles=results.ERA.by.mlb.yrs$lm.weights[1:20],inches=0.2,bg="red",xlab="MLB Years",ylab="Down Year Proportion x1000",main="Down Year Proportion - ERA")
lm.ERA.mlb.yrs<-lm(results.ERA.by.mlb.yrs$propor.x1000[1:20]~results.ERA.by.mlb.yrs$mlb.yrs[1:20],weights=results.ERA.by.mlb.yrs$lm.weights[1:20])
abline(lm.ERA.mlb.yrs)
symbols(results.WHIP.by.mlb.yrs$mlb.yrs[1:20],results.WHIP.by.mlb.yrs$propor.x1000[1:20],circles=results.WHIP.by.mlb.yrs$lm.weights[1:20],inches=0.2,bg="red",xlab="MLB Years",ylab="Down Year Proportion x1000",main="Down Year Proportion - WHIP")
lm.WHIP.mlb.yrs<-lm(results.WHIP.by.mlb.yrs$propor.x1000[1:20]~results.WHIP.by.mlb.yrs$mlb.yrs[1:20],weights=results.WHIP.by.mlb.yrs$lm.weights[1:20])
abline(lm.WHIP.mlb.yrs)
dy.to.p1.ERA<-sqldf("select a.year,a.[pitcher.ID],a.ERA [dy.ERA],b.ERA [p1.ERA] from [subset.dyERA.only] a left join [subset.dyERA.p1] b on a.[pitcher.ID]=b.[pitcher.ID] and a.year=(b.year-1)")
write.csv(dy.to.p1.ERA,"06_dy.to.p1.ERA.csv")

