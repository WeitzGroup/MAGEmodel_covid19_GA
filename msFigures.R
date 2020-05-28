## Scripts to plot figures for the manuscript:

#Beckett S.J., Dominguez-Mirazo, M., Lee, S., Andris C., Weitz J.S. Spread of COVID-19 through Georgia, USA. Near-term projections and impacts of social distancing via a metapopulation model.

#R version 4.0.0

## load dependencies
library(sf)     # v0.9.3
library(tmap)   # v3.0
options(stringsAsFactors = FALSE)
#source extra functions to aid analysis
source("Functions_for_msFigs.R")


########LOAD DATASETS########

###Pop size###
POP = read.csv("./data/GA_county_decade_age.csv")
POPSIZE = sum(POP$POPSIZE)

#######load shapefile########
counties <- st_read("./data/counties.shp")
GA <- counties[counties$STATE_NAME == "Georgia", ]
FIPlist <- GA$FIPSnumb

#######load Hospital ICU availability########
Hdat = read.csv("./data/hospital_CountyData.csv")
INDs = sort(Hdat$County,index.return=TRUE)
Hdat = Hdat[INDs$ix,]
Hdat$ICU[Hdat$ICU==0]=NA
Cap_ICU = Hdat$ICU
Hdat$FIPS = sort(FIPlist)
GA = merge(GA, Hdat, by = "FIPS")


########load HOSP_REGION #########
HRdat = read.csv("./data/ga_hosp_regions.csv")
GA = merge(GA,HRdat,by="FIPS")

#put ICUs at regional scale
REGIONS = unique(GA$HospRegion)
LIST = c()
for(aa in REGIONS){
	INDX = which(GA$HospRegion == aa)
	ICU_reg = rep(sum(GA$ICU[INDX],na.rm=TRUE),length(INDX))
	LIST[INDX] = ICU_reg
}
GA$Reg_ICU = LIST


#######load NYT dataset########
nyt <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
galong <- nyt[nyt$state == "Georgia",] 
names(galong)[names(galong) == "fips"] <- "FIPS"
for(d in unique(galong$date)){
THISDAY = galong[which(galong$date == d),]
INDS = setdiff(GA$FIPSnumb,THISDAY$FIPS)
	if(length(INDS)>0){
		for(aa in INDS){
		index = which(GA$FIPSnumb == aa)
			galong= rbind(galong, c(d,GA$NAME[index],"Georgia",aa,0,0))
		}
	}
}
GAtmap <- merge(GA, galong, by = "FIPS")

GAtmap$deaths = as.numeric(GAtmap$deaths)
GAtmap$cases = as.numeric(GAtmap$cases)
GAtmap$casesp100_000 = GAtmap$cases/GAtmap$POPULATION*100000

casebreaks =  c(0:log10(floor(max(GAtmap$cases))), log10(max(GAtmap$cases)))
deathbreaks = c(0:log10(floor(max(GAtmap$deaths))), log10(max(GAtmap$deaths)))
casep100_000breaks = c(-2:log10(floor(max(GAtmap$casesp100_000))), log10(max(GAtmap$casesp100_000)))

GAtmap$cases[GAtmap$cases==0] = NA
GAtmap$casesp100_000 = GAtmap$cases/GAtmap$POPULATION*100000
GAtmap$deaths[GAtmap$deaths==0] = NA


### Figure 1 -- timelapse of cases


recordDATESINTEREST = c("2020-03-07","2020-03-14","2020-03-21","2020-03-28","2020-04-04","2020-04-11","2020-04-18","2020-04-25","2020-05-02","2020-05-09","2020-05-16","2020-05-23")
record2DATESINTEREST = c("2020-03-28","2020-04-04","2020-04-11","2020-04-18","2020-04-25","2020-05-02","2020-05-09","2020-05-16","2020-05-23","2020-05-30")
INDEX=c()
INDEX2=c()
D_tot_rec=c()
for(aa in 1:length(recordDATESINTEREST)){
 INDEX <-c(INDEX,which(GAtmap$date == recordDATESINTEREST[aa]))
 D_tot_rec[aa] = sum(GAtmap$deaths[GAtmap$date == recordDATESINTEREST[aa]],na.rm=TRUE)
 }
 for(aa in 1:length(record2DATESINTEREST)){
  INDEX2 <-c(INDEX2,which(GAtmap$date == record2DATESINTEREST[aa]))
}
GAtmap2 = GAtmap[INDEX,]
GAtmap3 = GAtmap[INDEX2,]

NCOL = 7

m1 = tm_shape(GAtmap2) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "cases", style = "log10", breaks = casebreaks, textNA = "No data",
          colorNA = "lightgrey", 
          palette = "Reds",
		  title ="cumulative cases") +
  tm_facets(by = "date", free.coords = FALSE ,ncol=NCOL ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
       legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .12)
  
 m2 = tm_shape(GAtmap2) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "casesp100_000", style = "log10", breaks = casep100_000breaks, textNA = "No data",
          colorNA = "lightgrey", 
          palette = "Reds",
		  title ="cumulative cases \n per 100,000") +
  tm_facets(by = "date", free.coords = FALSE, ncol=NCOL ) +
    tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .12)

 m3 =  tmap_arrange(m1,m2,nrow=2)
 tmap_save(m3, width = 1800, height = 1800/1.7, units="px", filename = paste("./msfigures/Figure1.png", sep=""))
  tmap_save(m3, width = 1800, height = 1800/1.7, units="px", filename = paste("./msfigures/Figure1.pdf", sep=""))



#data taken from GDPH daily status reports https://dph.georgia.gov/covid-19-daily-status-report
#note that we were unable to measure the records on 2020/4/18
recordedCases= c(1097,1387,1643,2198,2446,2683,3032,4117,4748,5444,5967,6383,6742,7558,9156,9901,10885,11859,12261,12550,13621,14578,15409,16369,17432,18489,19398,20166,21102,21883,22491,23216,23481,23913,24854,25646,26155,27492,28331,28671,29442,30526,30738,31600,32181,32586,33508,34002,34848,35427,35977,36772,37212,37552,38283,38855,39801,40663,41482,42242,42902,43400,43983,44638)
recordedDeaths = c(38,47,56,65,79,83,102,125,154,176,198,208,219,294,348,362,412,425,432,442,480,524,579,617,668,689,774,818,846,881,899,907,916,971,1036,1096,1120,1167,1175,1179,1244,1302,1327,1352,1399,1401,1405,1444,1494,1517,1544,1588,1598,1606,1649,1675,1697,1775,1808,1822,1827,1848,1895,1933)
recordedHospitalised = c(361,438,509,607,660,678,773,885,1013,1129,1222,1266,1296,1393,1899,1993,2298,2454,2491,2518,2702,2858,3024,3260,3395,3489,3702,3885,4018,4154,4322,4353,4377,4681,4897,5076,5156,5311,5388,5405,5537,5699,5785,5866,5976,5989,5999,NA,6227,6308,NA,NA,NA,6835,6992,7076,7171,7289,7376,7431,7450,7507,7640,7745) 

#LAST UPDATE 7pm 27th May 2020

LREC = length(recordedCases)

#calculate cases per day and deaths per day
recordedCPD = recordedCases[2:LREC] - recordedCases[1:(LREC-1)]
recordedDPD = recordedDeaths[2:LREC] - recordedDeaths[1:(LREC-1)]

#simulation names to load in.
INPUTSIMNAME = c("March28_eig_RunApr23","March28_sim_RunApr23","28thApr_Run_int_1stMay")

#Note the dates of interest for plotting
#note that we were unable to measure the records on 2020/4/18
DATESRECORD = c("2020-03-24","2020-03-25","2020-03-26","2020-03-27","2020-03-28","2020-03-29","2020-03-30","2020-03-31","2020-04-01","2020-04-02","2020-04-03","2020-04-04","2020-04-05","2020-04-06","2020-04-07","2020-04-08","2020-04-09","2020-04-10","2020-04-11","2020-04-12","2020-04-13","2020-04-14","2020-04-15","2020-04-16","2020-04-17","2020-04-19","2020-04-20","2020-04-21","2020-04-22","2020-04-23","2020-04-24","2020-04-25","2020-04-26","2020-04-27","2020-04-28","2020-04-29","2020-04-30","2020-05-01","2020-05-02","2020-05-03","2020-05-04","2020-05-05","2020-05-06","2020-05-07","2020-05-08","2020-05-09","2020-05-10","2020-05-11","2020-05-12","2020-05-13","2020-05-14","2020-05-15","2020-05-16","2020-05-17","2020-05-18","2020-05-19","2020-05-20","2020-05-21","2020-05-22","2020-05-23","2020-05-24","2020-05-25","2020-05-26","2020-05-27")
modDatesRec = as.Date(DATESRECORD)

DATESINTEREST = seq(as.Date("2020-03-28"), as.Date("2020-07-04"), by="days") #Evaluation and Projection period
modDates = as.Date(DATESINTEREST)

DATESINTEREST2 = seq(as.Date("2020-03-28"), as.Date("2020-04-28"), by="days") #Evaluation phase
modDates2 = as.Date(DATESINTEREST2)
DATESINTEREST2a = DATESINTEREST2[-22] #no records on the 2020/4/18 #for comparing the model and data timeseries.

DATESINTEREST3 = seq(as.Date("2020-04-28"), as.Date("2020-07-04"), by="days") #Projection phase
modDates3 = as.Date(DATESINTEREST3)

SDname <- c("Social_1","Social_2","Social_3","Social_4") #social distancing file extensions


## Read in the simulation data

#1. plot against data using social distancing of 50% [evaluation model] (March 28-April 28)
E_3 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[3],GA,DATESINTEREST)

#2. plot out from April 28 at 50% SD, then from May 1for 3 SD with 0%, 50% and 75% reductions in transmission rates.
EE1= READ_SIM_DATA(INPUTSIMNAME[3],SDname[1],GA,DATESINTEREST3)
EE3= READ_SIM_DATA(INPUTSIMNAME[3],SDname[3],GA,DATESINTEREST3)
EE4= READ_SIM_DATA(INPUTSIMNAME[3],SDname[4],GA,DATESINTEREST3)


#Ancillary evaluation stages (March 28-April 28)
E_1 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[1],GA,DATESINTEREST)
E_2 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[2],GA,DATESINTEREST)
E_4 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[4],GA,DATESINTEREST)
S_1 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[1],GA,DATESINTEREST)
S_2 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[2],GA,DATESINTEREST)
S_3 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[3],GA,DATESINTEREST)
S_4 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[4],GA,DATESINTEREST)


# Want to compare model to data in a relative sense.
# do so by summing the squared the log-ratios.

LLL=35 #index of 28 April
Dvec=c()
Hvec=c()
Dvec[1]=SumLoggedSquares(ExtractSumThisDate(E_1$deaths,E_1$date,DATESINTEREST2a),  recordedDeaths[5:LLL])
Dvec[2]=SumLoggedSquares(ExtractSumThisDate(E_3$deaths,E_3$date,DATESINTEREST2a),  recordedDeaths[5:LLL])
Dvec[3]=SumLoggedSquares(ExtractSumThisDate(E_4$deaths,E_4$date,DATESINTEREST2a),  recordedDeaths[5:LLL])
Dvec[4]=SumLoggedSquares(ExtractSumThisDate(S_1$deaths,S_1$date,DATESINTEREST2a),  recordedDeaths[5:LLL])
Dvec[5]=SumLoggedSquares(ExtractSumThisDate(S_3$deaths,S_3$date,DATESINTEREST2a),  recordedDeaths[5:LLL])
Dvec[6]=SumLoggedSquares(ExtractSumThisDate(S_4$deaths,S_4$date,DATESINTEREST2a),  recordedDeaths[5:LLL])


Hvec[1]=SumLoggedSquares(ExtractSumThisDate(E_1$cumH,E_1$date,DATESINTEREST2a),  recordedHospitalised[5:LLL])
Hvec[2]=SumLoggedSquares(ExtractSumThisDate(E_3$cumH,E_3$date,DATESINTEREST2a),  recordedHospitalised[5:LLL])
Hvec[3]=SumLoggedSquares(ExtractSumThisDate(E_4$cumH,E_4$date,DATESINTEREST2a),  recordedHospitalised[5:LLL])
Hvec[4]=SumLoggedSquares(ExtractSumThisDate(S_1$cumH,S_1$date,DATESINTEREST2a), recordedHospitalised[5:LLL])
Hvec[5]=SumLoggedSquares(ExtractSumThisDate(S_3$cumH,S_3$date,DATESINTEREST2a), recordedHospitalised[5:LLL])
Hvec[6]=SumLoggedSquares(ExtractSumThisDate(S_4$cumH,S_4$date,DATESINTEREST2a),  recordedHospitalised[5:LLL])


### Figure S1 -- Evaluation phase plot
#a)Deaths b) deaths per day
#c) cases d) cases per day
#e) cum H f) Model evaluation.

LLL = 35 #index of 28 April.
LLL2 = LREC #num records.
LWD=2
PSZ=1
CL="black"
CL2 = "grey50"
PCH1 =19
PCH2 =21
PCH3=25
CLS = 1
NCOL = 1
TXT = 0.8

TIMES= as.Date(c("2020-03-24","2020-03-28","2020-04-01","2020-04-28","2020-05-01","2020-06-01","2020-07-01","2020-07-04"))

dev.new(width=13*0.81, height=9*0.81, unit="in")
LOGC = ""
par(mfrow=c(3,2),mar= c(3.8,5.5,1,1))
X1 = 18345 #March 24th
X2 = X1 + 36  #Apr29th
#a) deaths

plot(modDatesRec[5:LLL],recordedDeaths[5:LLL], xlim =c(X1 ,X2),ylim=c(1,2000),xlab="",ylab="Cumulative deaths",cex.lab=CLS,pch=PCH2,bg=NA,col=NA, xaxs='i',xaxt="n",log=LOGC)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html

lines(modDates2,ExtractSumThisDate(E_3$deaths,E_3$date,DATESINTEREST2),col="royalblue",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(E_1$deaths,E_1$date,DATESINTEREST2),col="skyblue",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(E_4$deaths,E_4$date,DATESINTEREST2),col="darkblue",lty=1,lwd=LWD)

lines(modDates2,ExtractSumThisDate(S_1$deaths,S_1$date,DATESINTEREST2),col="green",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(S_3$deaths,S_3$date,DATESINTEREST2),col="olivedrab",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(S_4$deaths,S_4$date,DATESINTEREST2),col="darkgreen",lty=1,lwd=LWD)

points(modDatesRec[LLL+1:LLL2],recordedDeaths[LLL+1:LLL2],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL],recordedDeaths[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedDeaths[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)

legend("topleft",bty="n",legend=c("Prior data","Evaluation data","SLM 0","SLM 50","SLM 75","CSM 0","CSM 50","CSM 75"), col=c(CL,CL,"blue","royalblue","darkblue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,rep(NA,6)),lty=c(NA,NA,1,1,1,1,1,1),pch=c(1,PCH2,rep(NA,6)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,2,2,2,2,2,2))

#b) deaths per day

#deaths per day
plot(modDates2,ExtractSumThisDate(E_3$dpd,E_3$date,DATESINTEREST2),col="white",ylim=c(1,150),xlim=c(X1,X2),cex.lab=CLS,xlab="",ylab="Deaths per day", xaxs='i',xaxt="n",log=LOGC)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
lines(modDates2,ExtractSumThisDate(E_3$dpd,E_3$date,DATESINTEREST2),col="royalblue",lwd=LWD)
lines(modDates2,ExtractSumThisDate(E_1$dpd,E_1$date,DATESINTEREST2),col="skyblue",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(E_4$dpd,E_4$date,DATESINTEREST2),col="darkblue",lty=1,lwd=LWD)

lines(modDates2,ExtractSumThisDate(S_1$dpd,S_1$date,DATESINTEREST2),col="green",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(S_3$dpd,S_3$date,DATESINTEREST2),col="olivedrab",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(S_4$dpd,S_4$date,DATESINTEREST2),col="darkgreen",lty=1,lwd=LWD)


points(modDatesRec[1:4]-0.5,c(recordedDPD[1:4]),col=CL,pch=PCH2,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL]-0.5,recordedDPD[5:(LLL)],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[LLL+1:LLL2]-0.5,recordedDPD[LLL+1:(LLL2)],col=CL2,pch=PCH3,bg=NA,cex=PSZ)

legend("topleft",bty="n",legend=c("Prior data","Evaluation data","SLM 0","SLM 50","SLM 75","CSM 0","CSM 50","CSM 75"), col=c(CL,CL,"blue","royalblue","darkblue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,rep(NA,6)),lty=c(NA,NA,1,1,1,1,1,1),pch=c(1,PCH2,rep(NA,6)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,2,2,2,2,2,2))

#c) cases

DRABOLIVE = rgb(107/255,142/255,35/255,0.5)
DARKGREEN = rgb(0,100/255,0,0.7)
GREEN = rgb(0,255/255,0,0.5)

LOW=5
HIGH=10
#cases
#Note: here, we down project cases based on there being 5-10 extra cases per recorded case.
plot(modDatesRec[5:LLL2],recordedCases[5:LLL2], xlim =c(X1 ,X2),ylim=c(1000,120000),xlab="",ylab="Cumulative recorded cases",cex.lab=CLS,pch=PCH2,bg=NA,col=NA, xaxs='i',xaxt="n",log=LOGC)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)


lines(modDates2,POPSIZE- ExtractSumThisDate(E_3$Suscep,E_3$date,DATESINTEREST2),col="royalblue",lwd=LWD)
lines(modDates2,POPSIZE- ExtractSumThisDate(E_1$Suscep,E_1$date,DATESINTEREST2),col="skyblue",lty=1,lwd=LWD)
lines(modDates2,POPSIZE- ExtractSumThisDate(E_4$Suscep,E_4$date,DATESINTEREST2),col="darkblue",lty=1,lwd=LWD)

lines(modDates2,POPSIZE- ExtractSumThisDate(S_1$Suscep,S_1$date,DATESINTEREST2),col="green",lty=1,lwd=LWD)
lines(modDates2,POPSIZE- ExtractSumThisDate(S_3$Suscep,S_3$date,DATESINTEREST2),col="olivedrab",lty=1,lwd=LWD)
lines(modDates2,POPSIZE- ExtractSumThisDate(S_4$Suscep,S_4$date,DATESINTEREST2),col="darkgreen",lty=1,lwd=LWD)


points(modDatesRec[LLL+1:LLL2],recordedCases[LLL+1:LLL2],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL],recordedCases[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedCases[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)

legend("topleft",bty="n",legend=c("Prior data","Evaluation data","SLM 0","SLM 50","SLM 75","CSM 0","CSM 50","CSM 75"), col=c(CL,CL,"blue","royalblue","darkblue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,rep(NA,6)),lty=c(NA,NA,1,1,1,1,1,1),pch=c(1,PCH2,rep(NA,6)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,2,2,2,2,2,2))

#d) cases per day
#recorded cases per day
#Note: here, we down project cases based on there being 5-10 extra cases per recorded case.
plot(modDates2,ExtractSumThisDate(E_3$cpd,E_3$date,DATESINTEREST2),col=NA,ylim=c(1,40000),xlim=c(X1,X2),cex.lab=CLS,xlab="",ylab="Recorded cases per day", xaxs='i',xaxt="n",log=LOGC)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
#evaluation phase

lines(modDates2,ExtractSumThisDate(E_3$cpd,E_3$date,DATESINTEREST2),col="royalblue",lwd=LWD)
lines(modDates2,ExtractSumThisDate(E_1$cpd,E_1$date,DATESINTEREST2),col="skyblue",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(E_4$cpd,E_4$date,DATESINTEREST2),col="darkblue",lty=1,lwd=LWD)

lines(modDates2,ExtractSumThisDate(S_1$cpd,S_1$date,DATESINTEREST2),col="green",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(S_3$cpd,S_3$date,DATESINTEREST2),col="olivedrab",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(S_4$cpd,S_4$date,DATESINTEREST2),col="darkgreen",lty=1,lwd=LWD)

points(modDatesRec[1:4]-0.5,c(recordedCPD[1:4]),col=CL,pch=PCH2,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL]-0.5,recordedCPD[5:(LLL)],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[LLL+1:LLL2]-0.5,recordedCPD[LLL+1:(LLL2)],col=CL2,pch=PCH3,bg=NA,cex=PSZ)

legend("topleft",bty="n",legend=c("Prior data","Evaluation data","SLM 0","SLM 50","SLM 75","CSM 0","CSM 50","CSM 75"), col=c(CL,CL,"blue","royalblue","darkblue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,rep(NA,6)),lty=c(NA,NA,1,1,1,1,1,1),pch=c(1,PCH2,rep(NA,6)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,2,2,2,2,2,2))


#e) cumulative Hospitalisations
#hospitalised

plot(modDatesRec[5:LLL2],recordedHospitalised[5:LLL2], xlim =c(X1 ,X2),ylim=c(1,10000),xlab="",ylab="Cumulative hospitalised",cex.lab=CLS,pch=PCH2,bg=NA,col=NA, xaxs='i',xaxt="n",log=LOGC)
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
points(modDatesRec[LLL+1:LLL2],recordedHospitalised[LLL+1:LLL2],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL],recordedHospitalised[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedHospitalised[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)
lines(modDates2,ExtractSumThisDate(E_3$cumH,E_3$date,DATESINTEREST2),col="royalblue",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(E_1$cumH,E_1$date,DATESINTEREST2),col="skyblue",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(E_4$cumH,E_4$date,DATESINTEREST2),col="darkblue",lty=1,lwd=LWD)

lines(modDates2,ExtractSumThisDate(S_1$cumH,S_1$date,DATESINTEREST2),col="green",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(S_3$cumH,S_3$date,DATESINTEREST2),col="olivedrab",lty=1,lwd=LWD)
lines(modDates2,ExtractSumThisDate(S_4$cumH,S_4$date,DATESINTEREST2),col="darkgreen",lty=1,lwd=LWD)

legend("topleft",bty="n",legend=c("Prior data","Evaluation data","SLM 0","SLM 50","SLM 75","CSM 0","CSM 50","CSM 75"), col=c(CL,CL,"blue","royalblue","darkblue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,rep(NA,6)),lty=c(NA,NA,1,1,1,1,1,1),pch=c(1,PCH2,rep(NA,6)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,2,2,2,2,2,2))

#f) fitting errors

LIST = c("SLM 0","SLM 50","SLM 75","CSM 0","CSM 50","CSM 75")

barplot(Dvec+Hvec,log="y",names.arg=LIST,ylab="Relative error to \ncumulative hospitalisations and deaths",col=c("skyblue","royalblue","darkblue","green","olivedrab","darkgreen"),border=NA)

dev.copy2pdf(file="msfigures/FigureS1.pdf")
dev.copy(png,file="msfigures/FigureS1.png",width=14*0.81, height=9*0.81, unit="in",res=300)
dev.off()



###### Figure 3 - short term projections for GA
#a)Deaths b) deaths per day
#c) cases d) cases per day
#e) cum H f) ICU required.  (w/ ICU limit line; and 3x surge?)

LLL = 35 #index of 28 April.
LLL2 = LREC #num records.
LWD=2
PSZ=1
CL="black"
CL2 = "grey50"
PCH1 =19
PCH2 =21
PCH3=25
CLS = 1
NCOL = 1
TXT = 0.8

TIMES= as.Date(c("2020-03-24","2020-03-28","2020-04-01","2020-04-28","2020-05-01","2020-06-01","2020-07-01","2020-07-04"))

dev.new(width=13*0.81, height=9*0.81, unit="in")
par(mfrow=c(3,2),mar= c(3.8,4.5,1,1))
X1 = 18345 #March 24th
X2 = X1 + 102  #Jul 4th
#a) deaths

plot(modDatesRec[5:LLL2],recordedDeaths[5:LLL2], xlim =c(X1 ,X2),ylim=c(0,5000),xlab="",ylab="Cumulative deaths",cex.lab=CLS,pch=PCH2,bg=NA,col=NA, xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html

lines(modDates2,ExtractSumThisDate(E_3$deaths,E_3$date,DATESINTEREST2),col="blue",lty=1,lwd=LWD)

lines(modDates3,ExtractSumThisDate(EE1$deaths,EE1$date,DATESINTEREST3),col="green",lty=1,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE3$deaths,EE3$date,DATESINTEREST3),col="olivedrab",lty=2,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE4$deaths,EE4$date,DATESINTEREST3),col="darkgreen",lty=4,lwd=LWD)

points(modDatesRec[LLL+1:LLL2],recordedDeaths[LLL+1:LLL2],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL],recordedDeaths[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedDeaths[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)

legend("topleft",bty="n",legend=c("Prior data","Evaluation data","Assessment data","Evaluation model","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,CL2,"blue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,NA,rep(NA,4)),lty=c(NA,NA,NA,1,1,2,4),pch=c(1,PCH2,PCH3,rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,1,2,2,2,2))

#b) deaths per day

#deaths per day
plot(modDates2,ExtractSumThisDate(E_3$dpd,E_3$date,DATESINTEREST2),col="white",log="",ylim=c(1,275),xlim=c(X1,X2),cex.lab=CLS,xlab="",ylab="Deaths per day", xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
lines(modDates2,ExtractSumThisDate(E_3$dpd,E_3$date,DATESINTEREST2),col="blue",lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE1$dpd,EE1$date,DATESINTEREST3),col="green",lty=1,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE3$dpd,EE3$date,DATESINTEREST3),col="olivedrab",lty=2,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE4$dpd,EE4$date,DATESINTEREST3),col="darkgreen",lwd=LWD,lty=4)
points(modDatesRec[1:4]-0.5,c(recordedDPD[1:4]),col=CL,pch=PCH2,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL]-0.5,recordedDPD[5:(LLL)],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[LLL+1:LLL2]-0.5,recordedDPD[LLL+1:(LLL2)],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
legend("topleft",bty="n",legend=c("Prior data","Evaluation data","Assessment Data","Evaluation model","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,CL2,"blue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,NA,rep(NA,4)),lty=c(NA,NA,NA,1,1,2,4),pch=c(1,PCH2,PCH3,rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,1,2,2,2,2))


#c) cases

DRABOLIVE = rgb(107/255,142/255,35/255,0.5)
DARKGREEN = rgb(0,100/255,0,0.7)
GREEN = rgb(0,255/255,0,0.5)

LOW=5
HIGH=10
#cases
#Note: here, we down project cases based on there being 5-10 extra cases per recorded case.
plot(modDatesRec[5:LLL2],recordedCases[5:LLL2], xlim =c(X1 ,X2),ylim=c(1000,120000),xlab="",ylab="Cumulative recorded cases",cex.lab=CLS,pch=PCH2,bg=NA,col=NA, xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)

XXX = c(modDates2,rev(modDates2))
THI = POPSIZE- ExtractSumThisDate(E_3$Suscep,E_3$date,DATESINTEREST2)
YYY = c(THI/HIGH,rev(THI/LOW))
#rgb(1, 0, 0,0.5)
polygon(XXX,YYY,col=rgb(0, 0, 1, 0.5),border=NA)

MminDH = (POPSIZE-ExtractSumThisDate(EE3$Suscep,EE4$date,DATESINTEREST3)[1])/HIGH - recordedCases[LLL]
MminDL = (POPSIZE-ExtractSumThisDate(EE3$Suscep,EE4$date,DATESINTEREST3)[1])/LOW - recordedCases[LLL]
MminML = (POPSIZE-ExtractSumThisDate(EE3$Suscep,EE4$date,DATESINTEREST3)[1])/7.5 - recordedCases[LLL]

XXX = c(modDates3,rev(modDates3))

THI = POPSIZE- ExtractSumThisDate(EE4$Suscep,EE4$date,DATESINTEREST3)
YYY = c(THI/HIGH-MminML,rev(THI/LOW -MminML))
polygon(XXX,YYY,col=DARKGREEN,border=NA)

lines(modDates3,THI/7.5-MminML,col='darkgreen',lwd=2)

THI = POPSIZE- ExtractSumThisDate(EE3$Suscep,EE3$date,DATESINTEREST3)
YYY = c(THI/HIGH-MminML,rev(THI/LOW -MminML))
polygon(XXX,YYY,col=DRABOLIVE,border=NA)

lines(modDates3,THI/7.5-MminML,col=rgb(107/255,152/255,35/255,1),lwd=2)

THI = POPSIZE- ExtractSumThisDate(EE1$Suscep,EE1$date,DATESINTEREST3)
YYY = c(THI/HIGH-MminML,rev(THI/LOW -MminML))
polygon(XXX,YYY,col=GREEN,border=NA)

lines(modDates3,THI/7.5 -MminML,col='green',lwd=2)

points(modDatesRec[LLL+1:LLL2],recordedCases[LLL+1:LLL2],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL],recordedCases[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedCases[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)

legend("topleft",bty="n",legend=c("Prior data","Evaluation data","Assessment Data","Ascertainment uncertainty","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,CL2,rgb(0, 0, 1, 0.5),"green","olivedrab","darkgreen"),pt.bg=c(NA,CL,NA,rep(NA,4)),lty=c(NA,NA,NA,1,1,1,1),pch=c(1,PCH2,PCH3,rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,1,2,2,2,2))


#d) cases per day
#recorded cases per day
#Note: here, we down project cases based on there being 5-10 extra cases per recorded case.
plot(modDates2,ExtractSumThisDate(E_3$cpd,E_3$date,DATESINTEREST2),col=NA,log="",ylim=c(1,5200),xlim=c(X1,X2),cex.lab=CLS,xlab="",ylab="Recorded cases per day", xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
#evaluation phase
XXX = c(modDates2,rev(modDates2))
THI = ExtractSumThisDate(E_3$cpd,E_3$date,DATESINTEREST2)
YYY = c(THI/HIGH,rev(THI/LOW))
polygon(XXX,YYY,col=rgb(0, 0, 1, 0.5),border=NA)

#forcast phase
XXX = c(modDates3,rev(modDates3))
THI = ExtractSumThisDate(EE4$cpd,EE4$date,DATESINTEREST3)
YYY = c(THI/HIGH,rev(THI/LOW))
polygon(XXX,YYY,col=DARKGREEN,border=NA)

THI = ExtractSumThisDate(EE3$cpd,EE3$date,DATESINTEREST3)
YYY = c(THI/HIGH,rev(THI/LOW))
polygon(XXX,YYY,col=DRABOLIVE,border=NA)

THI = ExtractSumThisDate(EE1$cpd,EE1$date,DATESINTEREST3)
YYY = c(THI/HIGH,rev(THI/LOW))
polygon(XXX,YYY,col=GREEN,border=NA)
points(modDatesRec[1:4]-0.5,c(recordedCPD[1:4]),col=CL,pch=PCH2,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL]-0.5,recordedCPD[5:(LLL)],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[LLL+1:LLL2]-0.5,recordedCPD[LLL+1:(LLL2)],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
legend("topleft",bty="n",legend=c("Prior data","Evaluation data","Assessment Data","Ascertainment uncertainty","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,CL2,rgb(0, 0, 1, 0.5),"green","olivedrab","darkgreen"),pt.bg=c(NA,CL,NA,rep(NA,4)),lty=c(NA,NA,NA,1,1,1,1),pch=c(1,PCH2,PCH3,rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,1,2,2,2,2))



#e) cumulative Hospitalisations
#hospitalised
MminD = ExtractSumThisDate(EE1$cumH,EE1$date,DATESINTEREST3)[1] - recordedHospitalised[LLL]

plot(modDatesRec[5:LLL2],recordedHospitalised[5:LLL2], xlim =c(X1 ,X2),ylim=c(0,25000),xlab="",ylab="Cumulative hospitalised",cex.lab=CLS,pch=PCH2,bg=NA,col=NA, xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
points(modDatesRec[LLL+1:LLL2],recordedHospitalised[LLL+1:LLL2],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL],recordedHospitalised[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedHospitalised[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)
lines(modDates2,ExtractSumThisDate(E_3$cumH,E_3$date,DATESINTEREST2),col="blue",lty=1,lwd=LWD)

lines(modDates3,ExtractSumThisDate(EE1$cumH,EE1$date,DATESINTEREST3)-MminD,col="green",lty=1,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE3$cumH,EE3$date,DATESINTEREST3)-MminD,col="olivedrab",lty=2,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE4$cumH,EE4$date,DATESINTEREST3)-MminD,col="darkgreen",lty=4,lwd=LWD)
legend("topleft",bty="n",legend=c("Prior data","Evaluation data","Assessment Data","Evaluation model","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,CL2,"blue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,NA,rep(NA,4)),lty=c(NA,NA,NA,1,1,2,4),pch=c(1,PCH2,PCH3,rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,1,2,2,2,2))

#f) ICU requirements


plot(modDatesRec[5:LLL2],recordedHospitalised[5:LLL2], xlim =c(X1 ,X2),ylim=c(0,4500),xlab="",ylab="ICU beds required",cex.lab=CLS,pch=PCH2,bg=NA,col=NA, xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
lines(modDates2,ExtractSumThisDate(E_3$ICUreq,E_3$date,DATESINTEREST2),col="blue",lty=1,lwd=LWD)

abline(h=2508,col="grey")
text(X2-15,2508+200,"Pre-surge ICU capacity",col="grey",cex=0.6)

lines(modDates3,ExtractSumThisDate(EE1$ICUreq,EE1$date,DATESINTEREST3),col="green",lty=1,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE3$ICUreq,EE3$date,DATESINTEREST3),col="olivedrab",lty=2,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE4$ICUreq,EE4$date,DATESINTEREST3),col="darkgreen",lty=4,lwd=LWD)


legend("topleft",bty="n",legend=c("Evaluation model","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c("blue","green","olivedrab","darkgreen"),pt.bg=c(rep(NA,4)),lty=c(1,1,2,4),pch=c(rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=2)

dev.copy2pdf(file="msfigures/Figure3.pdf")
dev.copy(png,file="msfigures/Figure3.png",width=14*0.81, height=9*0.81, unit="in",res=300)
dev.off()



### Figure 6 -- summary plot using eig method and matched to data up to mid september
#a)Deaths b) deaths per day
#c) cases d) cases per day
#e) cum H f) ICU required.  (w/ ICU limit line; and 3x surge?)

DATESINTEREST3 = seq(as.Date("2020-04-28"), as.Date("2020-09-24"), by="days") #Projecton phase
modDates3 = as.Date(DATESINTEREST3)

SDname <- c("Social_1","Social_2","Social_3","Social_4") #social distancing file extensions

## Read in the simulation data

#1. plot against data using social distancing of 50% [evaluation model]
E_3 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[3],GA,DATESINTEREST)

#2. plot out from April 28 at 50% SD, then from May 1for 3 SD with 0%, 50% and 75% reductions in transmission rates.
EE1= READ_SIM_DATA(INPUTSIMNAME[3],SDname[1],GA,DATESINTEREST3)
EE3= READ_SIM_DATA(INPUTSIMNAME[3],SDname[3],GA,DATESINTEREST3)
EE4= READ_SIM_DATA(INPUTSIMNAME[3],SDname[4],GA,DATESINTEREST3)


LLL = 35 #index of 28 April.
LLL2 = LREC #num records.
LWD=2
PSZ=1
CL="black"
CL2 = "grey50"
PCH1 =19
PCH2 =21
PCH3=25
CLS = 1
NCOL = 1
TXT = 0.8

TIMES= as.Date(c("2020-03-24","2020-03-28","2020-04-01","2020-04-28","2020-05-01","2020-06-01","2020-07-01","2020-07-04","2020-08-01","2020-09-01"))

dev.new(width=14*0.81, height=9*0.81, unit="in")
par(mfrow=c(3,2),mar= c(3.8,4.5,1,1))
X1 = 18345 #March 24th
#X2 = X1 + 102  #Jul 4th
X2 = X1 + 184 #September 25th
#a) deaths

plot(modDatesRec[5:LLL2],recordedDeaths[5:LLL2], xlim =c(X1 ,X2),ylim=c(0,10000),xlab="",ylab="Cumulative deaths",cex.lab=CLS,pch=PCH2,bg=NA,col=NA, xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html

lines(modDates2,ExtractSumThisDate(E_3$deaths,E_3$date,DATESINTEREST2),col="blue",lty=1,lwd=LWD)

lines(modDates3,ExtractSumThisDate(EE1$deaths,EE1$date,DATESINTEREST3),col="green",lty=1,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE3$deaths,EE3$date,DATESINTEREST3),col="olivedrab",lty=2,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE4$deaths,EE4$date,DATESINTEREST3),col="darkgreen",lty=4,lwd=LWD)

points(modDatesRec[LLL+1:LLL2],recordedDeaths[LLL+1:LLL2],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL],recordedDeaths[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedDeaths[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)

legend("topleft",bty="n",legend=c("Prior data","Evaluation data","Assessment data","Evaluation model","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,CL2,"blue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,NA,rep(NA,4)),lty=c(NA,NA,NA,1,1,2,4),pch=c(1,PCH2,PCH3,rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,1,2,2,2,2))

#b) deaths per day

#deaths per day
plot(modDates2,ExtractSumThisDate(E_3$dpd,E_3$date,DATESINTEREST2),col="white",log="",ylim=c(1,275),xlim=c(X1,X2),cex.lab=CLS,xlab="",ylab="Deaths per day", xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
lines(modDates2,ExtractSumThisDate(E_3$dpd,E_3$date,DATESINTEREST2),col="blue",lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE1$dpd,EE1$date,DATESINTEREST3),col="green",lty=1,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE3$dpd,EE3$date,DATESINTEREST3),col="olivedrab",lty=2,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE4$dpd,EE4$date,DATESINTEREST3),col="darkgreen",lwd=LWD,lty=4)
points(modDatesRec[1:4]-0.5,c(recordedDPD[1:4]),col=CL,pch=PCH2,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL]-0.5,recordedDPD[5:(LLL)],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[LLL+1:LLL2]-0.5,recordedDPD[LLL+1:(LLL2)],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
legend("topleft",bty="n",legend=c("Prior data","Evaluation data","Assessment Data","Evaluation model","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,CL2,"blue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,NA,rep(NA,4)),lty=c(NA,NA,NA,1,1,2,4),pch=c(1,PCH2,PCH3,rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,1,2,2,2,2))


#c) cases

DRABOLIVE = rgb(107/255,142/255,35/255,0.5)
DARKGREEN = rgb(0,100/255,0,0.7)
GREEN = rgb(0,255/255,0,0.5)

LOW=5
HIGH=10
#cases
#Note: here, we down project cases based on there being 5-10 extra cases per recorded case.
plot(modDatesRec[5:LLL2],recordedCases[5:LLL2], xlim =c(X1 ,X2),ylim=c(1000,160000),xlab="",ylab="Cumulative recorded cases",cex.lab=CLS,pch=PCH2,bg=NA,col=NA, xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)

XXX = c(modDates2,rev(modDates2))
THI = POPSIZE- ExtractSumThisDate(E_3$Suscep,E_3$date,DATESINTEREST2)
YYY = c(THI/HIGH,rev(THI/LOW))
#rgb(1, 0, 0,0.5)
polygon(XXX,YYY,col=rgb(0, 0, 1, 0.5),border=NA)

MminDH = (POPSIZE-ExtractSumThisDate(EE3$Suscep,EE4$date,DATESINTEREST3)[1])/HIGH - recordedCases[LLL]
MminDL = (POPSIZE-ExtractSumThisDate(EE3$Suscep,EE4$date,DATESINTEREST3)[1])/LOW - recordedCases[LLL]
MminML = (POPSIZE-ExtractSumThisDate(EE3$Suscep,EE4$date,DATESINTEREST3)[1])/7.5 - recordedCases[LLL]

XXX = c(modDates3,rev(modDates3))

THI = POPSIZE- ExtractSumThisDate(EE4$Suscep,EE4$date,DATESINTEREST3)
YYY = c(THI/HIGH-MminML,rev(THI/LOW -MminML))
polygon(XXX,YYY,col=DARKGREEN,border=NA)

lines(modDates3,THI/7.5-MminML,col='darkgreen',lwd=2)

THI = POPSIZE- ExtractSumThisDate(EE3$Suscep,EE3$date,DATESINTEREST3)
YYY = c(THI/HIGH-MminML,rev(THI/LOW -MminML))
polygon(XXX,YYY,col=DRABOLIVE,border=NA)

lines(modDates3,THI/7.5-MminML,col=rgb(107/255,152/255,35/255,1),lwd=2)

THI = POPSIZE- ExtractSumThisDate(EE1$Suscep,EE1$date,DATESINTEREST3)
YYY = c(THI/HIGH-MminML,rev(THI/LOW -MminML))
polygon(XXX,YYY,col=GREEN,border=NA)

lines(modDates3,THI/7.5 -MminML,col='green',lwd=2)

points(modDatesRec[LLL+1:LLL2],recordedCases[LLL+1:LLL2],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL],recordedCases[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedCases[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)

legend("topleft",bty="n",legend=c("Prior data","Evaluation data","Assessment Data","Ascertainment uncertainty","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,CL2,rgb(0, 0, 1, 0.5),"green","olivedrab","darkgreen"),pt.bg=c(NA,CL,NA,rep(NA,4)),lty=c(NA,NA,NA,1,1,1,1),pch=c(1,PCH2,PCH3,rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,1,2,2,2,2))


#d) cases per day
#recorded cases per day
#Note: here, we down project cases based on there being 5-10 extra cases per recorded case.
plot(modDates2,ExtractSumThisDate(E_3$cpd,E_3$date,DATESINTEREST2),col=NA,log="",ylim=c(1,5200),xlim=c(X1,X2),cex.lab=CLS,xlab="",ylab="Recorded cases per day", xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
#evaluation phase
XXX = c(modDates2,rev(modDates2))
THI = ExtractSumThisDate(E_3$cpd,E_3$date,DATESINTEREST2)
YYY = c(THI/HIGH,rev(THI/LOW))
polygon(XXX,YYY,col=rgb(0, 0, 1, 0.5),border=NA)

#forcast phase
XXX = c(modDates3,rev(modDates3))
THI = ExtractSumThisDate(EE4$cpd,EE4$date,DATESINTEREST3)
YYY = c(THI/HIGH,rev(THI/LOW))
polygon(XXX,YYY,col=DARKGREEN,border=NA)

THI = ExtractSumThisDate(EE3$cpd,EE3$date,DATESINTEREST3)
YYY = c(THI/HIGH,rev(THI/LOW))
polygon(XXX,YYY,col=DRABOLIVE,border=NA)

THI = ExtractSumThisDate(EE1$cpd,EE1$date,DATESINTEREST3)
YYY = c(THI/HIGH,rev(THI/LOW))
polygon(XXX,YYY,col=GREEN,border=NA)
points(modDatesRec[1:4]-0.5,c(recordedCPD[1:4]),col=CL,pch=PCH2,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL]-0.5,recordedCPD[5:(LLL)],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[LLL+1:LLL2]-0.5,recordedCPD[LLL+1:(LLL2)],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
legend("topleft",bty="n",legend=c("Prior data","Evaluation data","Assessment Data","Ascertainment uncertainty","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,CL2,rgb(0, 0, 1, 0.5),"green","olivedrab","darkgreen"),pt.bg=c(NA,CL,NA,rep(NA,4)),lty=c(NA,NA,NA,1,1,1,1),pch=c(1,PCH2,PCH3,rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,1,2,2,2,2))



#e) cumulative Hospitalisations
#hospitalised
MminD = ExtractSumThisDate(EE1$cumH,EE1$date,DATESINTEREST3)[1] - recordedHospitalised[LLL]

plot(modDatesRec[5:LLL2],recordedHospitalised[5:LLL2], xlim =c(X1 ,X2),ylim=c(0,50000),xlab="",ylab="Cumulative hospitalised",cex.lab=CLS,pch=PCH2,bg=NA,col=NA, xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
points(modDatesRec[LLL+1:LLL2],recordedHospitalised[LLL+1:LLL2],col=CL2,pch=PCH3,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL],recordedHospitalised[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedHospitalised[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)
lines(modDates2,ExtractSumThisDate(E_3$cumH,E_3$date,DATESINTEREST2),col="blue",lty=1,lwd=LWD)

lines(modDates3,ExtractSumThisDate(EE1$cumH,EE1$date,DATESINTEREST3)-MminD,col="green",lty=1,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE3$cumH,EE3$date,DATESINTEREST3)-MminD,col="olivedrab",lty=2,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE4$cumH,EE4$date,DATESINTEREST3)-MminD,col="darkgreen",lty=4,lwd=LWD)
legend("topleft",bty="n",legend=c("Prior data","Evaluation data","Assessment Data","Evaluation model","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,CL2,"blue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,NA,rep(NA,4)),lty=c(NA,NA,NA,1,1,2,4),pch=c(1,PCH2,PCH3,rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=c(1,1,1,2,2,2,2))

#f) ICU requirements


plot(modDatesRec[5:LLL2],recordedHospitalised[5:LLL2], xlim =c(X1 ,X2),ylim=c(0,9000),xlab="",ylab="ICU beds required",cex.lab=CLS,pch=PCH2,bg=NA,col=NA, xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
lines(modDates2,ExtractSumThisDate(E_3$ICUreq,E_3$date,DATESINTEREST2),col="blue",lty=1,lwd=LWD)

abline(h=2508,col="grey")
text(X2-35,2508+200,"Pre-surge ICU capacity",col="grey",cex=0.6)

lines(modDates3,ExtractSumThisDate(EE1$ICUreq,EE1$date,DATESINTEREST3),col="green",lty=1,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE3$ICUreq,EE3$date,DATESINTEREST3),col="olivedrab",lty=2,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE4$ICUreq,EE4$date,DATESINTEREST3),col="darkgreen",lty=4,lwd=LWD)


legend("topleft",bty="n",legend=c("Evaluation model","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c("blue","green","olivedrab","darkgreen"),pt.bg=c(rep(NA,4)),lty=c(1,1,2,4),pch=c(rep(NA,4)),ncol=NCOL,cex=TXT,pt.cex=PSZ,lwd=2)

dev.copy2pdf(file="msfigures/Figure6.pdf")
dev.copy(png,file="msfigures/Figure6.png",width=14*0.81, height=9*0.81, unit="in",res=300)
dev.off()





### Figure 4 -- death maps 
#Think about this as timings do not exactly match models

DATESINTERESTDMAP = c("2020-03-28","2020-04-04","2020-04-11","2020-04-18","2020-04-25") #Eval phase
modDates2 = as.Date(DATESINTERESTDMAP)
DATESINTEREST22 = c("2020-05-02","2020-05-09","2020-05-16","2020-05-23","2020-05-30") #Projection phase
modDates22 = as.Date(DATESINTEREST22)


#1. plot against data using social distancing of 50% [evaluation model]
E_3 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[3],GA,DATESINTERESTDMAP)

#2. plot out from April 28 at 50% SD, then from May 1for 3 SD with 0%, 50% and 75% reductions in transmission rates.
EE1= READ_SIM_DATA(INPUTSIMNAME[3],SDname[1],GA,DATESINTEREST22)
EE3= READ_SIM_DATA(INPUTSIMNAME[3],SDname[3],GA,DATESINTEREST22)
EE4= READ_SIM_DATA(INPUTSIMNAME[3],SDname[4],GA,DATESINTEREST22)



MXD =max(GAtmap3$deaths,E_3$deaths,EE3$deaths,na.rm=TRUE)
MXI =max(GAtmap3$ICUreq,E_3$ICUreq,EE3$ICUreq,na.rm=TRUE)
MXdr =max(GAtmap3$dpd,E_3$dpd,EE3$dpd,na.rm=TRUE)
MXcr =max(GAtmap3$cpd,E_3$cpd,EE3$cpd,na.rm=TRUE)
MXtH =max(GAtmap3$totHosp,E_3$totHosp,EE3$totHosp,na.rm=TRUE)



MXD =max(GAtmap3$deaths,E_3$deaths,EE3$deaths,EE1$deaths,EE4$deaths,na.rm=TRUE)
MXI =max(GAtmap3$ICUreq,E_3$ICUreq,EE3$ICUreq,EE1$ICUreq,EE4$ICUreq,na.rm=TRUE)
MXdr =max(GAtmap3$dpd,E_3$dpd,EE3$dpd,EE1$dpd,EE4$dpd,na.rm=TRUE)
MXcr =max(GAtmap3$cpd,E_3$cpd,EE3$cpd,EE1$cpd,EE4$cpd,na.rm=TRUE)
MXtH =max(GAtmap3$totHosp,E_3$totHosp,EE3$totHosp,EE1$totHosp,EE4$totHosp,na.rm=TRUE)

deathbreaks2 = c(-1:log10(floor(max(MXD))), log10(MXD))
icubreaks2 = c(-1:log10(floor(max(MXI))), log10(MXI))
dpdbreaks2 = c(-1:log10(floor(max(MXdr))), log10(MXdr))
cpdbreaks2 = c(-1:log10(floor(max(MXcr))), log10(MXcr))
tHbreaks2 = c(-1:log10(floor(max(MXtH))), log10(MXtH))

NCOLS = 10
PLS = 0.5
### deaths_summed
PAL = "Blues"
m1 = tm_shape(GAtmap3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "deaths", style = "cont",breaks = 10^deathbreaks2, textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="cumulative deaths") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= NCOLS ) +

  tm_layout(panel.label.size = PLS,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

EE_COMB = rbind(E_3,EE1)

m2 = tm_shape(EE_COMB) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "deaths", style = "cont", breaks = 10^deathbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected cumulative deaths \nwith back to business as usual") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= NCOLS ) +
  tm_layout(panel.label.size = PLS,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)
		  
		  
EE_COMB = rbind(E_3,EE3)

m3 = tm_shape(EE_COMB) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "deaths", style = "cont", breaks = 10^deathbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected cumulative deaths \nwith 50% reduction in transmission") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= NCOLS ) +
  tm_layout(panel.label.size = PLS,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

EE_COMB = rbind(E_3,EE4)

m4 = tm_shape(EE_COMB) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "deaths", style = "cont", breaks = 10^deathbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected cumulative deaths \nwith 75% reduction in transmission") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= NCOLS ) +
  tm_layout(panel.label.size = PLS,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)


m6 = tmap_arrange(m1,m2,m3,m4,nrow=4)
 tmap_save(m6, width = 7.5, height = 4, units="in", filename = paste("./msfigures/Figure4.png", sep=""),dpi=400)
  tmap_save(m6, width = 7.5, height = 4, units="in", filename = paste("./msfigures/Figure4.pdf", sep=""),dpi = 400)





### Figure 5 --  ICU maps

### ICU ratio 2 D/C
PAL = "-RdBu"
#tmaptools::tmap.pal.info

BREK =c(0,1,1.5,2,5)
		  
m1 = tm_shape(GA)+
 tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = c("ICU","Reg_ICU"), style = "cont" , breaks = c(0,100,200,600,1000), textNA = "No data",
          colorNA = "lightgrey", 
          palette = "Reds",
		  title =c("county ICU capacity","regional ICU capacity")) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)
		  

EE_COMB = rbind(E_3,EE1)
m2 = tm_shape(EE_COMB) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUrat2", style = "cont", breaks= BREK,midpoint=1, textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU excess \nwith back to business as usual    ") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= NCOLS ) +
  tm_layout(panel.label.size = PLS,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)
		  

EE_COMB = rbind(E_3,EE3)
m3 = tm_shape(EE_COMB) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUrat2", style = "cont", breaks= BREK,midpoint=1, textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU excess \nwith 50% reduction in transmission") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= NCOLS ) +
  tm_layout(panel.label.size = PLS,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)		  

		  

EE_COMB = rbind(E_3,EE4)
m4 = tm_shape(EE_COMB) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUrat2", style = "cont", breaks= BREK,midpoint=1, textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU excess \nwith 75% reduction in transmission") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= NCOLS ) +
  tm_layout(panel.label.size = PLS,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)	
		  

m6 = tmap_arrange(m1,m2,m3,m4,nrow=4)
 tmap_save(m6, width = 7.5, height = 4, units="in", filename = paste("./msfigures/Figure5.png", sep=""),dpi=400)
  tmap_save(m6, width = 7.5, height = 4, units="in", filename = paste("./msfigures/Figure5.pdf", sep=""),dpi = 400)
###

