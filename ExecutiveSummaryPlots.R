#R scripts to make figures for executive summary report

#load dependencies
library(sf)
library(tmap)
options(stringsAsFactors = FALSE)


READ_SIM_DATA <- function(INPUTSIMNAME,SDNAME,GA,DATES){
	
	SIMNAME = paste("./SimulationOutput/",INPUTSIMNAME,"/",SDNAME,"/",sep="")
	DD<-read.csv(paste(SIMNAME,"GA_county_Killed.csv",sep="")) #deaths
	EE <-read.csv(paste(SIMNAME,"GA_county_criticalHospitalised.csv",sep="")) #ICU
	FF <-read.csv(paste(SIMNAME,"GA_county_deathrateperday.csv",sep="")) # deaths per day
	GG <-read.csv(paste(SIMNAME,"GA_county_newcaserateperday.csv",sep="")) #new cases per day
	HH <-read.csv(paste(SIMNAME,"GA_county_totalHospitalised.csv",sep="")) # total hospitalised
	II <- read.csv(paste(SIMNAME,"GA_county_Susceptibles.csv",sep="")) # total susceptibles
	CC <- read.csv(paste(SIMNAME,"GA_county_cumulativeHospitalised.csv",sep="")) # cumulative hospitalised
	
	SZ <- dim(DD)[2]
	NAMES = colnames(DD)[3:(SZ-2)]
	newnames=c()
	noX = strsplit(NAMES,"X")
	for(aa in 1:length(noX)){
		this = strsplit(noX[[aa]][2],"[.]")
		now = paste(this[[1]][1],"-",this[[1]][2],"-",this[[1]][3],sep="")
		newnames[aa] = now
	}
	
	INDXdate = c()
	for(aa in 1:length(DATES)){
	INDXdate[aa] = which(newnames == DATES[aa])
	}
	
	DD<-DD[,c(1,2,INDXdate+2)]
	EE<-EE[,c(1,2,INDXdate+2)]
	FF<-FF[,c(1,2,INDXdate+2)]
	GG<-GG[,c(1,2,INDXdate+2)]
	HH<-HH[,c(1,2,INDXdate+2)]
	II<-II[,c(1,2,INDXdate+2)]
	CC<-CC[,c(1,2,INDXdate+2)]
	
	newnames = DATES

	REGIONS=unique(GA$HospRegion)
	RAT = matrix(0,dim(DD)[1],length(INDXdate))
	RAT2 = RAT
	for(aa in REGIONS){
		INDX = which(GA$HospRegion==aa)
		if(length(INDXdate)>1){
		ICUdemand = colSums(EE[INDX,3:(2+length(INDXdate))])
		}else{
		ICUdemand = sum(EE[INDX,])
		}
		Capacity = GA$Reg_ICU[INDX[1]]
		for(bb in INDX){
		RAT[bb,] = (Capacity - ICUdemand)/(Capacity+ICUdemand)
		RAT2[bb,] = ICUdemand/Capacity
		}
	}
	

	JJ=rep(DD$FIP,length(newnames))
	KK = rep(DD$County,length(newnames))
	Dat = c()
	NUMS = c()
	NUMS2 = c()
	NUMS3 = c()
	NUMS4 = c()
	NUMS5 = c()
	NUMS6 =c()
	NUMS7 =c()
	NUMS8 =c()
	NUMS9 = c()
	for (aa in 1:length(newnames)){
		Dat = c(Dat,rep(newnames[aa],159))
		NUMS = c(NUMS,DD[,aa+2])
		NUMS2 = c(NUMS2,EE[,aa+2])
		NUMS3 = c(NUMS3,FF[,aa+2])
		NUMS4 = c(NUMS4,GG[,aa+2])
		NUMS5 = c(NUMS5,HH[,aa+2])	
		NUMS6 = c(NUMS6,RAT[,aa])
		NUMS7 = c(NUMS7,II[,aa+2])
		NUMS8 = c(NUMS8,RAT2[,aa])
		NUMS9 = c(NUMS9,CC[,aa+2])
	}
	
	gad = list(FIPS=JJ, County = KK, date=Dat, deaths = NUMS, ICUreq = NUMS2, dpd = NUMS3,cpd = NUMS4, totHosp = NUMS5,ICUrat = NUMS6, Suscep = NUMS7,ICUrat2 = NUMS8,cumH = NUMS9)
	GAtmap_sim <- merge(GA, gad, by = "FIPS")
	
	return(GAtmap_sim)
}



#######load shapefile########
counties <- st_read("./data/counties.shp")
GA <- counties[counties$STATE_NAME == "Georgia", ]

#data taken from GDPH daily status reports https://dph.georgia.gov/covid-19-daily-status-report
recordedCases= c(1097,1387,1643,2198,2446,2683,3032,4117,4748,5444,5967,6383,6742,7558,9156,9901,10885,11859,12261,12550,13621,14578,15409,16369,17432,18489,19398,20166,21102,21883,22491,23216,23481,23913,24861)
recordedDeaths = c(38,47,56,65,79,83,102,125,154,176,198,208,219,294,348,362,412,425,432,442,480,524,579,617,668,689,774,818,846,881,899,907,916,971,1036)
recordedHospitalised = c(361,438,509,607,660,678,773,885,1013,1129,1222,1266,1296,1393,1899,1993,2298,2454,2491,2518,2702,2858,3024,3260,3395,3489,3702,3885,4018,4154,4322,4353,4377,4681,4898)

LREC = length(recordedCases)

#calculate cases per day and deaths per day
recordedCPD = recordedCases[2:LREC] - recordedCases[1:(LREC-1)]
recordedDPD = recordedDeaths[2:LREC] - recordedDeaths[1:(LREC-1)]

#simulation names to load in.
INPUTSIMNAME = c("March28_eig_RunApr23","March28_sim_RunApr23","28thApr_Run_int_1stMay")

#Note the dates of interest for plotting
DATESRECORD = c("2020-03-24","2020-03-25","2020-03-26","2020-03-27","2020-03-28","2020-03-29","2020-03-30","2020-03-31","2020-04-01","2020-04-02","2020-04-03","2020-04-04","2020-04-05","2020-04-06","2020-04-07","2020-04-08","2020-04-09","2020-04-10","2020-04-11","2020-04-12","2020-04-13","2020-04-14","2020-04-15","2020-04-16","2020-04-17","2020-04-19","2020-04-20","2020-04-21","2020-04-22","2020-04-23","2020-04-24","2020-04-25","2020-04-26","2020-04-27","2020-04-28")
modDatesRec = as.Date(DATESRECORD)

DATESINTEREST = seq(as.Date("2020-03-28"), as.Date("2020-07-04"), by="days")
modDates = as.Date(DATESINTEREST)

DATESINTEREST2 = seq(as.Date("2020-03-28"), as.Date("2020-04-28"), by="days")
modDates2 = as.Date(DATESINTEREST2)

DATESINTEREST3 = seq(as.Date("2020-04-28"), as.Date("2020-07-04"), by="days")
modDates3 = as.Date(DATESINTEREST3)

SDname <- c("Social_1","Social_2","Social_3","Social_4") #social distancing file extensions


## Read in the simulation data

#1. plot against data using social distancing of 50% [evaluation model]
E_3 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[3],GA,DATESINTEREST)

#2. plot out from April 28 at 50% SD, then from May 1for 3 SD with 0%, 50% and 75% reductions in transmission rates.
EE1= READ_SIM_DATA(INPUTSIMNAME[3],SDname[1],GA,DATESINTEREST3)
EE3= READ_SIM_DATA(INPUTSIMNAME[3],SDname[3],GA,DATESINTEREST3)
EE4= READ_SIM_DATA(INPUTSIMNAME[3],SDname[4],GA,DATESINTEREST3)



###MAKE FIGURES

# FIG 1: Show state wide deaths, hospitalised and cases

LLL = length(DATESRECORD)
LWD=2
PSZ=1.2
CL="black"
dev.new(width=7*0.81, height=9*0.81, unit="in")
PCH1 =19
PCH2 =21

TIMES= as.Date(c("2020-03-24","2020-03-28","2020-04-01","2020-04-28","2020-05-01","2020-06-01","2020-07-01","2020-07-04"))

par(mfrow=c(3,1),mar= c(3.8,4.5,1,2))
X1 = 18345 #March 24th
X2 = X1 + 102  #Jul 4th

#deaths
plot(modDatesRec[5:LLL],recordedDeaths[5:LLL], xlim =c(X1 ,X2),ylim=c(0,5000),xlab="",ylab="Cumulative deaths",cex.lab=CLS,pch=PCH2,bg=CL,col=CL, xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html
points(modDatesRec[5:LLL],recordedDeaths[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedDeaths[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)
lines(modDates2,ExtractSumThisDate(E_3$deaths,E_3$date,DATESINTEREST2),col="blue",lty=1,lwd=LWD)

lines(modDates3,ExtractSumThisDate(EE1$deaths,EE1$date,DATESINTEREST3),col="green",lty=1,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE3$deaths,EE3$date,DATESINTEREST3),col="olivedrab",lty=2,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE4$deaths,EE4$date,DATESINTEREST3),col="darkgreen",lty=4,lwd=LWD)

legend("topleft",bty="n",legend=c("Prior data","Data","Evaluation model","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,"blue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,rep(NA,4)),lty=c(NA,NA,1,1,2,4),pch=c(1,PCH2,rep(NA,4)),ncol=1,cex=1.25,pt.cex=PSZ,lwd=2)

MminD = ExtractSumThisDate(EE1$cumH,EE1$date,DATESINTEREST3)[1] - recordedHospitalised[LLL]

#hospitalised
plot(modDatesRec[5:LLL],recordedHospitalised[5:LLL], xlim =c(X1 ,X2),ylim=c(0,25000),xlab="",ylab="Cumulative hospitalised",cex.lab=CLS,pch=PCH2,bg=CL,col=CL, xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
points(modDatesRec[5:LLL],recordedHospitalised[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedHospitalised[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)
lines(modDates2,ExtractSumThisDate(E_3$cumH,E_3$date,DATESINTEREST2),col="blue",lty=1,lwd=LWD)

lines(modDates3,ExtractSumThisDate(EE1$cumH,EE1$date,DATESINTEREST3)-MminD,col="green",lty=1,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE3$cumH,EE3$date,DATESINTEREST3)-MminD,col="olivedrab",lty=2,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE4$cumH,EE4$date,DATESINTEREST3)-MminD,col="darkgreen",lty=4,lwd=LWD)
legend("topleft",bty="n",legend=c("Prior data","Data","Evaluation model","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,"blue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,rep(NA,4)),lty=c(NA,NA,1,1,2,4),pch=c(1,PCH2,rep(NA,4)),ncol=1,cex=1.25,pt.cex=PSZ,lwd=2)


DRABOLIVE = rgb(107/255,142/255,35/255,0.5)
DARKGREEN = rgb(0,100/255,0,0.7)
GREEN = rgb(0,255/255,0,0.5)

LOW=5
HIGH=10
#cases
#Note: here, we down project cases based on there being 5-10 extra cases per recorded case.
plot(modDatesRec[5:LLL],recordedCases[5:LLL], xlim =c(X1 ,X2),ylim=c(1000,120000),xlab="",ylab="Cumulative recorded cases",cex.lab=CLS,pch=PCH2,bg=CL,col=CL, xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)

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

points(modDatesRec[5:LLL],recordedCases[5:LLL],col=CL,pch=PCH2,bg=CL,cex=PSZ)
points(modDatesRec[1:4],recordedCases[1:4],bg=NA,col=CL,pch=PCH2,cex=PSZ)

legend("topleft",bty="n",legend=c("Prior data","Data","Ascertainment uncertainty","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,rgb(0, 0, 1, 0.5),"green","olivedrab","darkgreen"),pt.bg=c(NA,CL,rep(NA,4)),lty=c(NA,NA,1,1,1,1),pch=c(1,PCH2,rep(NA,4)),ncol=1,cex=1.25,pt.cex=PSZ,lwd=2)


dev.copy2pdf(file="figures/Summary1a.pdf")
dev.copy(png,file="figures/Summary1a.png",width=7*0.81, height=9*0.81, unit="in",res=300)
dev.off()



# FIG 2: recorded cases and deaths per day

dev.new(width=7*0.81, height=7*0.81, unit="in")
par(mfrow=c(2,1),mar= c(3.8,4.5,1,2))

#recorded cases per day
#Note: here, we down project cases based on there being 5-10 extra cases per recorded case.
plot(modDates2,ExtractSumThisDate(E_3$cpd,E_3$date,DATESINTEREST2),col="white",log="",ylim=c(1,5200),xlim=c(X1,X2),xlab="",ylab="Recorded cases per day", xaxs='i',xaxt="n")
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
points(modDatesRec[1:4],c(0,recordedCPD[1:3]),col=CL,pch=PCH2,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL],recordedCPD[4:(LLL-1)],col=CL,pch=PCH2,bg=CL,cex=PSZ)
legend("topleft",bty="n",legend=c("Prior data","Data","Ascertainment uncertainty","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,rgb(0, 0, 1, 0.5),"green","olivedrab","darkgreen"),pt.bg=c(NA,CL,rep(NA,4)),lty=c(NA,NA,1,1,1,1),pch=c(1,PCH2,rep(NA,4)),ncol=1,cex=0.8,pt.cex=PSZ,lwd=2)

#deaths per day
plot(modDates2,ExtractSumThisDate(E_3$dpd,E_3$date,DATESINTEREST2),col="white",log="",ylim=c(1,250),xlim=c(X1,X2),xlab="",ylab="Deaths per day", xaxs='i',xaxt="n")
axis.Date(1,at=TIMES,format="%e %b",las=2,cex.axis=0.8) # https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html)
lines(modDates2,ExtractSumThisDate(E_3$dpd,E_3$date,DATESINTEREST2),col="blue",lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE1$dpd,EE1$date,DATESINTEREST3),col="green",lty=1,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE3$dpd,EE3$date,DATESINTEREST3),col="olivedrab",lty=2,lwd=LWD)
lines(modDates3,ExtractSumThisDate(EE4$dpd,EE4$date,DATESINTEREST3),col="darkgreen",lwd=LWD,lty=4)
points(modDatesRec[1:4],c(0,recordedDPD[1:3]),col=CL,pch=PCH2,bg=NA,cex=PSZ)
points(modDatesRec[5:LLL],recordedDPD[4:(LLL-1)],col=CL,pch=PCH2,bg=CL,cex=PSZ)
legend("topleft",bty="n",legend=c("Prior data","Data","Evaluation model","Model with back to business as usual","Model with 50% transmission reduction","Model with 75% transmission reduction"), col=c(CL,CL,"blue","green","olivedrab","darkgreen"),pt.bg=c(NA,CL,rep(NA,4)),lty=c(NA,NA,1,1,2,4),pch=c(1,PCH2,rep(NA,4)),ncol=1,cex=0.8,pt.cex=PSZ,lwd=2)


dev.copy2pdf(file="figures/Summary2.pdf")
dev.copy(png,file="figures/Summary2.png",width=7*0.81, height=6*0.81, unit="in",res=300)
dev.off()


#FIG 3: Maps of cumulative deaths on 4 July 2020.


#read in data for maps
EM_1 = READ_SIM_DATA(INPUTSIMNAME[3],SDname[1],GA,c("2020-07-04","2020-07-03"))
EM_3 = READ_SIM_DATA(INPUTSIMNAME[3],SDname[3],GA,c("2020-07-04","2020-07-03"))
EM_4 = READ_SIM_DATA(INPUTSIMNAME[3],SDname[4],GA,c("2020-07-04","2020-07-03"))


MXD =max(EM_1$deaths,EM_4$deaths,na.rm=TRUE)
deathbreaks2 = c(-1:log10(floor(max(MXD))), log10(MXD))


TIT = ">30,000 deaths with back to BAU model projections by 2020-07-04"
TIT2 = "~2,000 deaths with 75% reduction in transmission rates by 2020-07-04"

d = sort(unique(EM_1$date))[1]
	ThisGAtmap <- EM_1[EM_1$date == d,]
m1 = tm_shape(ThisGAtmap ) + 
  tm_borders(alpha=0.6,lwd=0.5)+
   tm_fill(col = "deaths", style = "cont",breaks = 10^deathbreaks2, textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected\ncumulative\ndeaths") +  
  tm_layout(title=TIT, title.position = c("LEFT","BOTTOM"),attr.outside=TRUE,attr.position=c("right","top"),panel.label.size = 1,legend.title.size = 1,
          legend.text.size = 0.5, legend.outside.position = "right" , legend.outside.size = .6)

	ThisGAtmap <- EM_4[EM_4$date == d,]
m2 = tm_shape(ThisGAtmap ) + 
  tm_borders(alpha=0.6,lwd=0.5)+
   tm_fill(col = "deaths", style = "cont", breaks = 10^deathbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected\ncumulative\ndeaths") +
 tm_layout(title=TIT2, title.position = c("LEFT","BOTTOM"),attr.outside=TRUE,attr.position=c("RIGHT","top"),attr.just="right",panel.label.size = 1,legend.title.size = 1,
          legend.text.size = 0.5, legend.outside.position = "RIGHT" , legend.outside.size = .6)

M3 = tmap_arrange(m1,m2,nrow=2)
tmap_save(M3, width = 4*0.81, height = 9*0.81, units="in", filename = paste("figures/Summary1b.png", sep=""))
tmap_save(M3, width = 4*0.81, height = 9*0.81, units="in", filename = paste("figures/Summary1b.pdf", sep=""))









