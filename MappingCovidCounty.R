
#maps for COVID-19 county modelling paper

#load dependencies
library(sf)
library(tmap)
options(stringsAsFactors = FALSE)


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

#########load in Simulation data ##########

READ_SIM_DATA <- function(INPUTSIMNAME,SDNAME,GA,DATES){
	
	SIMNAME = paste("./SimulationOutput/",INPUTSIMNAME,"/",SDNAME,"/",sep="")
	DD<-read.csv(paste(SIMNAME,"GA_county_Killed.csv",sep="")) #deaths
	EE <-read.csv(paste(SIMNAME,"GA_county_criticalHospitalised.csv",sep="")) #ICU
	FF <-read.csv(paste(SIMNAME,"GA_county_deathrateperday.csv",sep="")) # deaths per day
	GG <-read.csv(paste(SIMNAME,"GA_county_newcaserateperday.csv",sep="")) #new cases per day
	HH <-read.csv(paste(SIMNAME,"GA_county_totalHospitalised.csv",sep="")) # total hospitalised
	II <- read.csv(paste(SIMNAME,"GA_county_Susceptibles.csv",sep="")) # total susceptibles
	
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
	
	newnames = DATES

	REGIONS=unique(GA$HospRegion)
	RAT = matrix(0,dim(DD)[1],length(INDXdate))
	RAT2 = RAT
	for(aa in REGIONS){
		INDX = which(GA$HospRegion==aa)
		ICUdemand = colSums(EE[INDX,3:(2+length(INDXdate))])
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
	}
	
	gad = list(FIPS=JJ, County = KK, date=Dat, deaths = NUMS, ICUreq = NUMS2, dpd = NUMS3,cpd = NUMS4, totHosp = NUMS5,ICUrat = NUMS6, Suscep = NUMS7,ICUrat2 = NUMS8)
	GAtmap_sim <- merge(GA, gad, by = "FIPS")
	
	return(GAtmap_sim)
}


SDname <- c("Social_1","Social_2","Social_3","Social_4") #social distancing file extensions
SDname2 <- c("Social_1relaxed","Social_2relaxed","Social_3relaxed","Social_4relaxed") #social distancing file extensions
INPUTSIMNAME = c("March28_eig","March28_sim")
DATESINTEREST = c("2020-03-28","2020-04-04","2020-04-11","2020-04-18","2020-04-25","2020-05-02")

E_1 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[1],GA,DATESINTEREST)
E_2 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[2],GA,DATESINTEREST)
E_3 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[3],GA,DATESINTEREST)
E_4 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[4],GA,DATESINTEREST)
S_1 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[1],GA,DATESINTEREST)
S_2 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[2],GA,DATESINTEREST)
S_3 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[3],GA,DATESINTEREST)
S_4 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[4],GA,DATESINTEREST)

E_1_relax = READ_SIM_DATA(INPUTSIMNAME[1],SDname2[1],GA,DATESINTEREST)
E_2_relax = READ_SIM_DATA(INPUTSIMNAME[1],SDname2[2],GA,DATESINTEREST)
E_3_relax = READ_SIM_DATA(INPUTSIMNAME[1],SDname2[3],GA,DATESINTEREST)
E_4_relax = READ_SIM_DATA(INPUTSIMNAME[1],SDname2[4],GA,DATESINTEREST)
S_1_relax = READ_SIM_DATA(INPUTSIMNAME[2],SDname2[1],GA,DATESINTEREST)
S_2_relax = READ_SIM_DATA(INPUTSIMNAME[2],SDname2[2],GA,DATESINTEREST)
S_3_relax = READ_SIM_DATA(INPUTSIMNAME[2],SDname2[3],GA,DATESINTEREST)
S_4_relax = READ_SIM_DATA(INPUTSIMNAME[2],SDname2[4],GA,DATESINTEREST)

#make maps
#cases map - from data

recordDATESINTEREST = c("2020-03-07","2020-03-14","2020-03-21","2020-03-28","2020-04-04","2020-04-11","2020-04-18")
record2DATESINTEREST = c("2020-03-28","2020-04-04","2020-04-11","2020-04-18")
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

m1 = tm_shape(GAtmap2) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "cases", style = "log10", breaks = casebreaks, textNA = "No data",
          colorNA = "lightgrey", 
          palette = "Reds",
		  title ="cumulative cases") +
  tm_facets(by = "date", free.coords = FALSE ,ncol=7 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
       legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .12)
  
 m2 = tm_shape(GAtmap2) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "casesp100_000", style = "log10", breaks = casep100_000breaks, textNA = "No data",
          colorNA = "lightgrey", 
          palette = "Reds",
		  title ="cumulative cases \n per 100,000") +
  tm_facets(by = "date", free.coords = FALSE, ncol=7 ) +
    tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .12)

 m3 =  tmap_arrange(m1,m2,nrow=2)
 tmap_save(m3, width = 1800, height = 1800/1.7, units="px", filename = paste("./figures/casesplusselect_dates.png", sep=""))
  tmap_save(m3, width = 1800, height = 1800/1.7, units="px", filename = paste("./figures/casesplusselect_dates.pdf", sep=""))



ExtractSumThisDate <- function(NUMS,DATES,DATESWANT){
	NUMS_by_date = c()
	for(aa in 1:length(DATESWANT)){
		INDY = which(DATES == DATESWANT[aa])
		NUMS_by_date[aa] = sum(NUMS[INDY])
	}
	return(NUMS_by_date)
}







DATESINTEREST = c("2020-03-28","2020-04-04","2020-04-11","2020-04-18","2020-04-25","2020-05-02","2020-05-09","2020-05-14","2020-05-16","2020-05-23","2020-05-30","2020-06-06","2020-06-13","2020-06-20","2020-06-27","2020-07-04")
modDates = as.Date(DATESINTEREST)
DATESINTEREST2 = c("2020-03-28","2020-04-04","2020-04-11","2020-04-18","2020-04-25","2020-05-02")
modDates2 = as.Date(DATESINTEREST2)


E_1 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[1],GA,DATESINTEREST)
E_2 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[2],GA,DATESINTEREST)
E_3 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[3],GA,DATESINTEREST)
E_4 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[4],GA,DATESINTEREST)
S_1 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[1],GA,DATESINTEREST)
S_2 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[2],GA,DATESINTEREST)
S_3 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[3],GA,DATESINTEREST)
S_4 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[4],GA,DATESINTEREST)

E_1_relax = READ_SIM_DATA(INPUTSIMNAME[1],SDname2[1],GA,DATESINTEREST)
E_2_relax = READ_SIM_DATA(INPUTSIMNAME[1],SDname2[2],GA,DATESINTEREST)
E_3_relax = READ_SIM_DATA(INPUTSIMNAME[1],SDname2[3],GA,DATESINTEREST)
E_4_relax = READ_SIM_DATA(INPUTSIMNAME[1],SDname2[4],GA,DATESINTEREST)
S_1_relax = READ_SIM_DATA(INPUTSIMNAME[2],SDname2[1],GA,DATESINTEREST)
S_2_relax = READ_SIM_DATA(INPUTSIMNAME[2],SDname2[2],GA,DATESINTEREST)
S_3_relax = READ_SIM_DATA(INPUTSIMNAME[2],SDname2[3],GA,DATESINTEREST)
S_4_relax = READ_SIM_DATA(INPUTSIMNAME[2],SDname2[4],GA,DATESINTEREST)


CLS=1.5

par(mfrow=c(3,2),mar = c(3, 4.5, 2, 2))
plot(modDates2,ExtractSumThisDate(E_1$deaths,E_1$date,DATESINTEREST2),col="darkgreen",log="y",ylim=c(50,5000),xlab="",ylab="Cumulative deaths",cex.lab=CLS)
lines(modDates2,ExtractSumThisDate(E_1$deaths,E_1$date,DATESINTEREST2),col="darkgreen")
#lines(modDates2,ExtractSumThisDate(E_2$deaths,E_1$date,DATESINTEREST2))
#points(modDates2,ExtractSumThisDate(E_2$deaths,E_1$date,DATESINTEREST2))
lines(modDates2,ExtractSumThisDate(E_3$deaths,E_1$date,DATESINTEREST2),col="olivedrab")
points(modDates2,ExtractSumThisDate(E_3$deaths,E_1$date,DATESINTEREST2),col="olivedrab")
lines(modDates2,ExtractSumThisDate(E_4$deaths,E_1$date,DATESINTEREST2),col="green")
points(modDates2,ExtractSumThisDate(E_4$deaths,E_1$date,DATESINTEREST2),col="green")
lines(modDates2,ExtractSumThisDate(S_1$deaths,E_1$date,DATESINTEREST2),col="darkblue")
points(modDates2,ExtractSumThisDate(S_1$deaths,E_1$date,DATESINTEREST2),col="darkblue")
#lines(modDates2,ExtractSumThisDate(S_2$deaths,E_1$date,DATESINTEREST2))
#points(modDates2,ExtractSumThisDate(S_2$deaths,E_1$date,DATESINTEREST2))
lines(modDates2,ExtractSumThisDate(S_3$deaths,E_1$date,DATESINTEREST2),col="royalblue")
points(modDates2,ExtractSumThisDate(S_3$deaths,E_1$date,DATESINTEREST2),col="royalblue")
lines(modDates2,ExtractSumThisDate(S_4$deaths,E_1$date,DATESINTEREST2),col="skyblue")
points(modDates2,ExtractSumThisDate(S_4$deaths,E_1$date,DATESINTEREST2),col="skyblue")

lines(as.Date(recordDATESINTEREST),D_tot_rec,lwd=3,col="red")
points(as.Date(recordDATESINTEREST),D_tot_rec,lwd=3,col="red")
legend("bottomright",bty="n",legend=c("Data","SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("red","darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates2,POPSIZE- ExtractSumThisDate(E_1$Suscep,E_1$date,DATESINTEREST2),col="darkgreen",log="y",ylim=c(10000,5000000),xlab="",ylab="Cumulative exposed",cex.lab=CLS)
lines(modDates2,POPSIZE- ExtractSumThisDate(E_1$Suscep,E_1$date,DATESINTEREST2),col="darkgreen")
#lines(modDates2,POPSIZE- ExtractSumThisDate(E_2$Suscep,E_1$date,DATESINTEREST2))
#points(modDates2,POPSIZE- ExtractSumThisDate(E_2$Suscep,E_1$date,DATESINTEREST2))
lines(modDates2,POPSIZE- ExtractSumThisDate(E_3$Suscep,E_1$date,DATESINTEREST2),col="olivedrab")
points(modDates2,POPSIZE- ExtractSumThisDate(E_3$Suscep,E_1$date,DATESINTEREST2),col="olivedrab")
lines(modDates2,POPSIZE- ExtractSumThisDate(E_4$Suscep,E_1$date,DATESINTEREST2),col="green")
points(modDates2,POPSIZE- ExtractSumThisDate(E_4$Suscep,E_1$date,DATESINTEREST2),col="green")
lines(modDates2,POPSIZE- ExtractSumThisDate(S_1$Suscep,E_1$date,DATESINTEREST2),col="darkblue")
points(modDates2,POPSIZE- ExtractSumThisDate(S_1$Suscep,E_1$date,DATESINTEREST2),col="darkblue")
#lines(modDates2,POPSIZE- ExtractSumThisDate(S_2$Suscep,E_1$date,DATESINTEREST2))
#points(modDates2,POPSIZE- ExtractSumThisDate(S_2$Suscep,E_1$date,DATESINTEREST2))
lines(modDates2,POPSIZE- ExtractSumThisDate(S_3$Suscep,E_1$date,DATESINTEREST2),col="royalblue")
points(modDates2,POPSIZE- ExtractSumThisDate(S_3$Suscep,E_1$date,DATESINTEREST2),col="royalblue")
lines(modDates2,POPSIZE- ExtractSumThisDate(S_4$Suscep,E_1$date,DATESINTEREST2),col="skyblue")
points(modDates2,POPSIZE- ExtractSumThisDate(S_4$Suscep,E_1$date,DATESINTEREST2),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates2,ExtractSumThisDate(E_1$totHosp,E_1$date,DATESINTEREST2),col="darkgreen",log="y",ylim=c(200,25000),xlab="",ylab="Hospitalised",cex.lab=CLS)
lines(modDates2,ExtractSumThisDate(E_1$totHosp,E_1$date,DATESINTEREST2),col="darkgreen")
#lines(modDates2,ExtractSumThisDate(E_2$totHosp,E_1$date,DATESINTEREST2))
#points(modDates2,ExtractSumThisDate(E_2$totHosp,E_1$date,DATESINTEREST2))
lines(modDates2,ExtractSumThisDate(E_3$totHosp,E_1$date,DATESINTEREST2),col="olivedrab")
points(modDates2,ExtractSumThisDate(E_3$totHosp,E_1$date,DATESINTEREST2),col="olivedrab")
lines(modDates2,ExtractSumThisDate(E_4$totHosp,E_1$date,DATESINTEREST2),col="green")
points(modDates2,ExtractSumThisDate(E_4$totHosp,E_1$date,DATESINTEREST2),col="green")
lines(modDates2,ExtractSumThisDate(S_1$totHosp,E_1$date,DATESINTEREST2),col="darkblue")
points(modDates2,ExtractSumThisDate(S_1$totHosp,E_1$date,DATESINTEREST2),col="darkblue")
#lines(modDates2,ExtractSumThisDate(S_2$totHosp,E_1$date,DATESINTEREST2))
#points(modDates2,ExtractSumThisDate(S_2$totHosp,E_1$date,DATESINTEREST2))
lines(modDates2,ExtractSumThisDate(S_3$totHosp,E_1$date,DATESINTEREST2),col="royalblue")
points(modDates2,ExtractSumThisDate(S_3$totHosp,E_1$date,DATESINTEREST2),col="royalblue")
lines(modDates2,ExtractSumThisDate(S_4$totHosp,E_1$date,DATESINTEREST2),col="skyblue")
points(modDates2,ExtractSumThisDate(S_4$totHosp,E_1$date,DATESINTEREST2),col="skyblue")
legend("topleft",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates2,ExtractSumThisDate(E_1$ICUreq,E_1$date,DATESINTEREST2),col="darkgreen",log="y",ylim=c(50,10000),xlab="",ylab="ICU beds required",cex.lab=CLS)
lines(modDates2,ExtractSumThisDate(E_1$ICUreq,E_1$date,DATESINTEREST2),col="darkgreen")
#lines(modDates2,ExtractSumThisDate(E_2$ICUreq,E_1$date,DATESINTEREST2))
#points(modDates2,ExtractSumThisDate(E_2$ICUreq,E_1$date,DATESINTEREST2))
lines(modDates2,ExtractSumThisDate(E_3$ICUreq,E_1$date,DATESINTEREST2),col="olivedrab")
points(modDates2,ExtractSumThisDate(E_3$ICUreq,E_1$date,DATESINTEREST2),col="olivedrab")
lines(modDates2,ExtractSumThisDate(E_4$ICUreq,E_1$date,DATESINTEREST2),col="green")
points(modDates2,ExtractSumThisDate(E_4$ICUreq,E_1$date,DATESINTEREST2),col="green")
lines(modDates2,ExtractSumThisDate(S_1$ICUreq,E_1$date,DATESINTEREST2),col="darkblue")
points(modDates2,ExtractSumThisDate(S_1$ICUreq,E_1$date,DATESINTEREST2),col="darkblue")
#lines(modDates2,ExtractSumThisDate(S_2$ICUreq,E_1$date,DATESINTEREST2))
#points(modDates2,ExtractSumThisDate(S_2$ICUreq,E_1$date,DATESINTEREST2))
lines(modDates2,ExtractSumThisDate(S_3$ICUreq,E_1$date,DATESINTEREST2),col="royalblue")
points(modDates2,ExtractSumThisDate(S_3$ICUreq,E_1$date,DATESINTEREST2),col="royalblue")
lines(modDates2,ExtractSumThisDate(S_4$ICUreq,E_1$date,DATESINTEREST2),col="skyblue")
points(modDates2,ExtractSumThisDate(S_4$ICUreq,E_1$date,DATESINTEREST2),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates2,ExtractSumThisDate(E_1$dpd,E_1$date,DATESINTEREST2),col="darkgreen",xlab="",ylab="Deaths per day",log="y",ylim=c(5,750),cex.lab=CLS)
lines(modDates2,ExtractSumThisDate(E_1$dpd,E_1$date,DATESINTEREST2),col="darkgreen")
#lines(modDates2,ExtractSumThisDate(E_2$dpd,E_1$date,DATESINTEREST2))
#points(modDates2,ExtractSumThisDate(E_2$dpd,E_1$date,DATESINTEREST2))
lines(modDates2,ExtractSumThisDate(E_3$dpd,E_1$date,DATESINTEREST2),col="olivedrab")
points(modDates2,ExtractSumThisDate(E_3$dpd,E_1$date,DATESINTEREST2),col="olivedrab")
lines(modDates2,ExtractSumThisDate(E_4$dpd,E_1$date,DATESINTEREST2),col="green")
points(modDates2,ExtractSumThisDate(E_4$dpd,E_1$date,DATESINTEREST2),col="green")
lines(modDates2,ExtractSumThisDate(S_1$dpd,E_1$date,DATESINTEREST2),col="darkblue")
points(modDates2,ExtractSumThisDate(S_1$dpd,E_1$date,DATESINTEREST2),col="darkblue")
#lines(modDates2,ExtractSumThisDate(S_2$dpd,E_1$date,DATESINTEREST2))
#points(modDates2,ExtractSumThisDate(S_2$dpd,E_1$date,DATESINTEREST2))
lines(modDates2,ExtractSumThisDate(S_3$dpd,E_1$date,DATESINTEREST2),col="royalblue")
points(modDates2,ExtractSumThisDate(S_3$dpd,E_1$date,DATESINTEREST2),col="royalblue")
lines(modDates2,ExtractSumThisDate(S_4$dpd,E_1$date,DATESINTEREST2),col="skyblue")
points(modDates2,ExtractSumThisDate(S_4$dpd,E_1$date,DATESINTEREST2),col="skyblue")
legend("topleft",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates2,ExtractSumThisDate(E_1$cpd,E_1$date,DATESINTEREST2),col="darkgreen",xlab="",ylab="Cases per day",ylim=c(100,200000),log="y",cex.lab=CLS)
lines(modDates2,ExtractSumThisDate(E_1$cpd,E_1$date,DATESINTEREST2),col="darkgreen")
#lines(modDates2,ExtractSumThisDate(E_2$cpd,E_1$date,DATESINTEREST2))
#points(modDates2,ExtractSumThisDate(E_2$cpd,E_1$date,DATESINTEREST2))
lines(modDates2,ExtractSumThisDate(E_3$cpd,E_1$date,DATESINTEREST2),col="olivedrab")
points(modDates2,ExtractSumThisDate(E_3$cpd,E_1$date,DATESINTEREST2),col="olivedrab")
lines(modDates2,ExtractSumThisDate(E_4$cpd,E_1$date,DATESINTEREST2),col="green")
points(modDates2,ExtractSumThisDate(E_4$cpd,E_1$date,DATESINTEREST2),col="green")
lines(modDates2,ExtractSumThisDate(S_1$cpd,E_1$date,DATESINTEREST2),col="darkblue")
points(modDates2,ExtractSumThisDate(S_1$cpd,E_1$date,DATESINTEREST2),col="darkblue")
#lines(modDates2,ExtractSumThisDate(S_2$cpd,E_1$date,DATESINTEREST2))
#points(modDates2,ExtractSumThisDate(S_2$cpd,E_1$date,DATESINTEREST2))
lines(modDates2,ExtractSumThisDate(S_3$cpd,E_1$date,DATESINTEREST2),col="royalblue")
points(modDates2,ExtractSumThisDate(S_3$cpd,E_1$date,DATESINTEREST2),col="royalblue")
lines(modDates2,ExtractSumThisDate(S_4$cpd,E_1$date,DATESINTEREST2),col="skyblue")
points(modDates2,ExtractSumThisDate(S_4$cpd,E_1$date,DATESINTEREST2),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

dev.size=c(9.427083,8.187500)
dev.copy2pdf(file="./figuresstatelevel_shortterm.pdf")



# keep lockdown - medium term

par(mfrow=c(3,2),mar = c(3, 4.5, 2, 2))
plot(modDates,ExtractSumThisDate(E_1$deaths,E_1$date,DATESINTEREST),col="darkgreen",log="y",ylim=c(50,50000),xlab="",ylab="Cumulative deaths",cex.lab=CLS)
lines(modDates,ExtractSumThisDate(E_1$deaths,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,ExtractSumThisDate(E_2$deaths,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(E_2$deaths,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(E_3$deaths,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,ExtractSumThisDate(E_3$deaths,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,ExtractSumThisDate(E_4$deaths,E_1$date,DATESINTEREST),col="green")
points(modDates,ExtractSumThisDate(E_4$deaths,E_1$date,DATESINTEREST),col="green")
lines(modDates,ExtractSumThisDate(S_1$deaths,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,ExtractSumThisDate(S_1$deaths,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,ExtractSumThisDate(S_2$deaths,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(S_2$deaths,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(S_3$deaths,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,ExtractSumThisDate(S_3$deaths,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,ExtractSumThisDate(S_4$deaths,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,ExtractSumThisDate(S_4$deaths,E_1$date,DATESINTEREST),col="skyblue")

lines(as.Date(recordDATESINTEREST),D_tot_rec,lwd=3,col="red")
points(as.Date(recordDATESINTEREST),D_tot_rec,lwd=3,col="red")
legend("bottomright",bty="n",legend=c("Data","SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("red","darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates,POPSIZE- ExtractSumThisDate(E_1$Suscep,E_1$date,DATESINTEREST),col="darkgreen",log="y",ylim=c(10000,10000000),xlab="",ylab="Cumulative exposed",cex.lab=CLS)
lines(modDates,POPSIZE- ExtractSumThisDate(E_1$Suscep,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,POPSIZE- ExtractSumThisDate(E_2$Suscep,E_1$date,DATESINTEREST))
#points(modDates,POPSIZE- ExtractSumThisDate(E_2$Suscep,E_1$date,DATESINTEREST))
lines(modDates,POPSIZE- ExtractSumThisDate(E_3$Suscep,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,POPSIZE- ExtractSumThisDate(E_3$Suscep,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,POPSIZE- ExtractSumThisDate(E_4$Suscep,E_1$date,DATESINTEREST),col="green")
points(modDates,POPSIZE- ExtractSumThisDate(E_4$Suscep,E_1$date,DATESINTEREST),col="green")
lines(modDates,POPSIZE- ExtractSumThisDate(S_1$Suscep,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,POPSIZE- ExtractSumThisDate(S_1$Suscep,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,POPSIZE- ExtractSumThisDate(S_2$Suscep,E_1$date,DATESINTEREST))
#points(modDates,POPSIZE- ExtractSumThisDate(S_2$Suscep,E_1$date,DATESINTEREST))
lines(modDates,POPSIZE- ExtractSumThisDate(S_3$Suscep,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,POPSIZE- ExtractSumThisDate(S_3$Suscep,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,POPSIZE- ExtractSumThisDate(S_4$Suscep,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,POPSIZE- ExtractSumThisDate(S_4$Suscep,E_1$date,DATESINTEREST),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates,ExtractSumThisDate(E_1$totHosp,E_1$date,DATESINTEREST),col="darkgreen",log="y",ylim=c(200,100000),xlab="",ylab="Hospitalised",cex.lab=CLS)
lines(modDates,ExtractSumThisDate(E_1$totHosp,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,ExtractSumThisDate(E_2$totHosp,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(E_2$totHosp,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(E_3$totHosp,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,ExtractSumThisDate(E_3$totHosp,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,ExtractSumThisDate(E_4$totHosp,E_1$date,DATESINTEREST),col="green")
points(modDates,ExtractSumThisDate(E_4$totHosp,E_1$date,DATESINTEREST),col="green")
lines(modDates,ExtractSumThisDate(S_1$totHosp,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,ExtractSumThisDate(S_1$totHosp,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,ExtractSumThisDate(S_2$totHosp,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(S_2$totHosp,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(S_3$totHosp,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,ExtractSumThisDate(S_3$totHosp,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,ExtractSumThisDate(S_4$totHosp,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,ExtractSumThisDate(S_4$totHosp,E_1$date,DATESINTEREST),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates,ExtractSumThisDate(E_1$ICUreq,E_1$date,DATESINTEREST),col="darkgreen",log="y",ylim=c(50,50000),xlab="",ylab="ICU beds required",cex.lab=CLS)
lines(modDates,ExtractSumThisDate(E_1$ICUreq,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,ExtractSumThisDate(E_2$ICUreq,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(E_2$ICUreq,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(E_3$ICUreq,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,ExtractSumThisDate(E_3$ICUreq,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,ExtractSumThisDate(E_4$ICUreq,E_1$date,DATESINTEREST),col="green")
points(modDates,ExtractSumThisDate(E_4$ICUreq,E_1$date,DATESINTEREST),col="green")
lines(modDates,ExtractSumThisDate(S_1$ICUreq,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,ExtractSumThisDate(S_1$ICUreq,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,ExtractSumThisDate(S_2$ICUreq,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(S_2$ICUreq,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(S_3$ICUreq,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,ExtractSumThisDate(S_3$ICUreq,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,ExtractSumThisDate(S_4$ICUreq,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,ExtractSumThisDate(S_4$ICUreq,E_1$date,DATESINTEREST),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates,ExtractSumThisDate(E_1$dpd,E_1$date,DATESINTEREST),col="darkgreen",xlab="",ylab="Deaths per day",log="y",ylim=c(5,1250),cex.lab=CLS)
lines(modDates,ExtractSumThisDate(E_1$dpd,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,ExtractSumThisDate(E_2$dpd,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(E_2$dpd,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(E_3$dpd,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,ExtractSumThisDate(E_3$dpd,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,ExtractSumThisDate(E_4$dpd,E_1$date,DATESINTEREST),col="green")
points(modDates,ExtractSumThisDate(E_4$dpd,E_1$date,DATESINTEREST),col="green")
lines(modDates,ExtractSumThisDate(S_1$dpd,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,ExtractSumThisDate(S_1$dpd,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,ExtractSumThisDate(S_2$dpd,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(S_2$dpd,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(S_3$dpd,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,ExtractSumThisDate(S_3$dpd,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,ExtractSumThisDate(S_4$dpd,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,ExtractSumThisDate(S_4$dpd,E_1$date,DATESINTEREST),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates,ExtractSumThisDate(E_1$cpd,E_1$date,DATESINTEREST),col="darkgreen",xlab="",ylab="Cases per day",ylim=c(100,200000),log="y",cex.lab=CLS)
lines(modDates,ExtractSumThisDate(E_1$cpd,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,ExtractSumThisDate(E_2$cpd,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(E_2$cpd,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(E_3$cpd,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,ExtractSumThisDate(E_3$cpd,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,ExtractSumThisDate(E_4$cpd,E_1$date,DATESINTEREST),col="green")
points(modDates,ExtractSumThisDate(E_4$cpd,E_1$date,DATESINTEREST),col="green")
lines(modDates,ExtractSumThisDate(S_1$cpd,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,ExtractSumThisDate(S_1$cpd,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,ExtractSumThisDate(S_2$cpd,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(S_2$cpd,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(S_3$cpd,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,ExtractSumThisDate(S_3$cpd,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,ExtractSumThisDate(S_4$cpd,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,ExtractSumThisDate(S_4$cpd,E_1$date,DATESINTEREST),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

dev.size=c(9.427083,8.187500)
dev.copy2pdf(file="./figures/statelevel_keeplockdown.pdf")

#removal of social distancing
DATERELEASE=as.Date("2020-05-15")

par(mfrow=c(3,2),mar = c(3, 4.5, 2, 2))
plot(modDates,ExtractSumThisDate(E_1_relax$deaths,E_1$date,DATESINTEREST),col="darkgreen",log="y",ylim=c(50,50000),xlab="",ylab="Cumulative deaths",cex.lab=CLS)
abline(v=DATERELEASE,col="grey",lwd=5)
lines(modDates,ExtractSumThisDate(E_1_relax$deaths,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,ExtractSumThisDate(E_2_relax$deaths,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(E_2_relax$deaths,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(E_3_relax$deaths,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,ExtractSumThisDate(E_3_relax$deaths,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,ExtractSumThisDate(E_4_relax$deaths,E_1$date,DATESINTEREST),col="green")
points(modDates,ExtractSumThisDate(E_4_relax$deaths,E_1$date,DATESINTEREST),col="green")
lines(modDates,ExtractSumThisDate(S_1_relax$deaths,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,ExtractSumThisDate(S_1_relax$deaths,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,ExtractSumThisDate(S_2_relax$deaths,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(S_2_relax$deaths,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(S_3_relax$deaths,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,ExtractSumThisDate(S_3_relax$deaths,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,ExtractSumThisDate(S_4_relax$deaths,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,ExtractSumThisDate(S_4_relax$deaths,E_1$date,DATESINTEREST),col="skyblue")

lines(as.Date(recordDATESINTEREST),D_tot_rec,lwd=3,col="red")
points(as.Date(recordDATESINTEREST),D_tot_rec,lwd=3,col="red")
legend("bottomright",bty="n",legend=c("Data","SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("red","darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates,POPSIZE- ExtractSumThisDate(E_1_relax$Suscep,E_1$date,DATESINTEREST),col="darkgreen",ylim=c(10000,10000000),log="y",xlab="",ylab="Cumulative exposed",cex.lab=CLS)
abline(v=DATERELEASE,col="grey",lwd=5)
lines(modDates,POPSIZE- ExtractSumThisDate(E_1_relax$Suscep,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,POPSIZE- ExtractSumThisDate(E_2_relax$Suscep,E_1$date,DATESINTEREST))
#points(modDates,POPSIZE- ExtractSumThisDate(E_2_relax$Suscep,E_1$date,DATESINTEREST))
lines(modDates,POPSIZE- ExtractSumThisDate(E_3_relax$Suscep,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,POPSIZE- ExtractSumThisDate(E_3_relax$Suscep,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,POPSIZE- ExtractSumThisDate(E_4_relax$Suscep,E_1$date,DATESINTEREST),col="green")
points(modDates,POPSIZE- ExtractSumThisDate(E_4_relax$Suscep,E_1$date,DATESINTEREST),col="green")
lines(modDates,POPSIZE- ExtractSumThisDate(S_1_relax$Suscep,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,POPSIZE- ExtractSumThisDate(S_1_relax$Suscep,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,POPSIZE- ExtractSumThisDate(S_2_relax$Suscep,E_1$date,DATESINTEREST))
#points(modDates,POPSIZE- ExtractSumThisDate(S_2_relax$Suscep,E_1$date,DATESINTEREST))
lines(modDates,POPSIZE- ExtractSumThisDate(S_3_relax$Suscep,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,POPSIZE- ExtractSumThisDate(S_3_relax$Suscep,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,POPSIZE- ExtractSumThisDate(S_4_relax$Suscep,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,POPSIZE- ExtractSumThisDate(S_4_relax$Suscep,E_1$date,DATESINTEREST),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates,ExtractSumThisDate(E_1_relax$totHosp,E_1$date,DATESINTEREST),col="darkgreen",log="y",ylim=c(200,100000),xlab="",ylab="Hospitalised",cex.lab=CLS)
abline(v=DATERELEASE,col="grey",lwd=5)
lines(modDates,ExtractSumThisDate(E_1_relax$totHosp,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,ExtractSumThisDate(E_2_relax$totHosp,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(E_2_relax$totHosp,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(E_3_relax$totHosp,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,ExtractSumThisDate(E_3_relax$totHosp,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,ExtractSumThisDate(E_4_relax$totHosp,E_1$date,DATESINTEREST),col="green")
points(modDates,ExtractSumThisDate(E_4_relax$totHosp,E_1$date,DATESINTEREST),col="green")
lines(modDates,ExtractSumThisDate(S_1_relax$totHosp,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,ExtractSumThisDate(S_1_relax$totHosp,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,ExtractSumThisDate(S_2_relax$totHosp,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(S_2_relax$totHosp,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(S_3_relax$totHosp,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,ExtractSumThisDate(S_3_relax$totHosp,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,ExtractSumThisDate(S_4_relax$totHosp,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,ExtractSumThisDate(S_4_relax$totHosp,E_1$date,DATESINTEREST),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates,ExtractSumThisDate(E_1_relax$ICUreq,E_1$date,DATESINTEREST),col="darkgreen",log="y",ylim=c(50,50000),xlab="",ylab="ICU beds required",cex.lab=CLS)
abline(v=DATERELEASE,col="grey",lwd=5)
lines(modDates,ExtractSumThisDate(E_1_relax$ICUreq,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,ExtractSumThisDate(E_2_relax$ICUreq,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(E_2_relax$ICUreq,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(E_3_relax$ICUreq,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,ExtractSumThisDate(E_3_relax$ICUreq,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,ExtractSumThisDate(E_4_relax$ICUreq,E_1$date,DATESINTEREST),col="green")
points(modDates,ExtractSumThisDate(E_4_relax$ICUreq,E_1$date,DATESINTEREST),col="green")
lines(modDates,ExtractSumThisDate(S_1_relax$ICUreq,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,ExtractSumThisDate(S_1_relax$ICUreq,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,ExtractSumThisDate(S_2_relax$ICUreq,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(S_2_relax$ICUreq,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(S_3_relax$ICUreq,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,ExtractSumThisDate(S_3_relax$ICUreq,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,ExtractSumThisDate(S_4_relax$ICUreq,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,ExtractSumThisDate(S_4_relax$ICUreq,E_1$date,DATESINTEREST),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

plot(modDates,ExtractSumThisDate(E_1_relax$dpd,E_1$date,DATESINTEREST),col="darkgreen",xlab="",ylab="Deaths per day",log="y",ylim=c(5,1250),cex.lab=CLS)
abline(v=DATERELEASE,col="grey",lwd=5)
lines(modDates,ExtractSumThisDate(E_1_relax$dpd,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,ExtractSumThisDate(E_2_relax$dpd,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(E_2_relax$dpd,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(E_3_relax$dpd,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,ExtractSumThisDate(E_3_relax$dpd,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,ExtractSumThisDate(E_4_relax$dpd,E_1$date,DATESINTEREST),col="green")
points(modDates,ExtractSumThisDate(E_4_relax$dpd,E_1$date,DATESINTEREST),col="green")
lines(modDates,ExtractSumThisDate(S_1_relax$dpd,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,ExtractSumThisDate(S_1_relax$dpd,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,ExtractSumThisDate(S_2_relax$dpd,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(S_2_relax$dpd,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(S_3_relax$dpd,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,ExtractSumThisDate(S_3_relax$dpd,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,ExtractSumThisDate(S_4_relax$dpd,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,ExtractSumThisDate(S_4_relax$dpd,E_1$date,DATESINTEREST),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)


plot(modDates,ExtractSumThisDate(E_1_relax$cpd,E_1$date,DATESINTEREST),col="darkgreen",xlab="",ylab="Cases per day",ylim=c(100,200000),log="y",cex.lab=CLS)
abline(v=DATERELEASE,col="grey",lwd=5)
lines(modDates,ExtractSumThisDate(E_1_relax$cpd,E_1$date,DATESINTEREST),col="darkgreen")
#lines(modDates,ExtractSumThisDate(E_2_relax$cpd,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(E_2_relax$cpd,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(E_3_relax$cpd,E_1$date,DATESINTEREST),col="olivedrab")
points(modDates,ExtractSumThisDate(E_3_relax$cpd,E_1$date,DATESINTEREST),col="olivedrab")
lines(modDates,ExtractSumThisDate(E_4_relax$cpd,E_1$date,DATESINTEREST),col="green")
points(modDates,ExtractSumThisDate(E_4_relax$cpd,E_1$date,DATESINTEREST),col="green")
lines(modDates,ExtractSumThisDate(S_1_relax$cpd,E_1$date,DATESINTEREST),col="darkblue")
points(modDates,ExtractSumThisDate(S_1_relax$cpd,E_1$date,DATESINTEREST),col="darkblue")
#lines(modDates,ExtractSumThisDate(S_2_relax$cpd,E_1$date,DATESINTEREST))
#points(modDates,ExtractSumThisDate(S_2_relax$cpd,E_1$date,DATESINTEREST))
lines(modDates,ExtractSumThisDate(S_3_relax$cpd,E_1$date,DATESINTEREST),col="royalblue")
points(modDates,ExtractSumThisDate(S_3_relax$cpd,E_1$date,DATESINTEREST),col="royalblue")
lines(modDates,ExtractSumThisDate(S_4_relax$cpd,E_1$date,DATESINTEREST),col="skyblue")
points(modDates,ExtractSumThisDate(S_4_relax$cpd,E_1$date,DATESINTEREST),col="skyblue")
legend("bottomright",bty="n",legend=c("SLM 0SD","SLM 50D","SLM 7SD","CSM 0SD","CSM 50SD","CSM 75SD"), col=c("darkgreen","olivedrab","green","darkblue","royalblue","skyblue"),lty=1,pch=1,ncol=2)

dev.size=c(9.427083,8.187500)
dev.copy2pdf(file="./figures/statelevel_removelockdown.pdf")



##### MAPS


E_1 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[1],GA,DATESINTEREST2)
E_2 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[2],GA,DATESINTEREST2)
E_3 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[3],GA,DATESINTEREST2)
E_4 = READ_SIM_DATA(INPUTSIMNAME[1],SDname[4],GA,DATESINTEREST2)
S_1 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[1],GA,DATESINTEREST2)
S_2 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[2],GA,DATESINTEREST2)
S_3 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[3],GA,DATESINTEREST2)
S_4 = READ_SIM_DATA(INPUTSIMNAME[2],SDname[4],GA,DATESINTEREST2)

MXD =max(GAtmap3$deaths,E_1$deaths,E_3$deaths,S_1$deaths,S_3$deaths,na.rm=TRUE)
MXI =max(GAtmap3$ICUreq,E_1$ICUreq,E_3$ICUreq,S_1$ICUreq,S_3$ICUreq,na.rm=TRUE)
MXdr =max(GAtmap3$dpd,E_1$dpd,E_3$dpd,S_1$dpd,S_3$dpd,na.rm=TRUE)
MXcr =max(GAtmap3$cpd,E_1$cpd,E_3$cpd,S_1$cpd,S_3$cpd,na.rm=TRUE)
MXtH =max(GAtmap3$totHosp,E_1$totHosp,E_3$totHosp,S_1$totHosp,S_3$totHosp,na.rm=TRUE)

deathbreaks2 = c(-1:log10(floor(max(MXD))), log10(MXD))
icubreaks2 = c(-1:log10(floor(max(MXI))), log10(MXI))
dpdbreaks2 = c(-1:log10(floor(max(MXdr))), log10(MXdr))
cpdbreaks2 = c(-1:log10(floor(max(MXcr))), log10(MXcr))
tHbreaks2 = c(-1:log10(floor(max(MXtH))), log10(MXtH))

### deaths_summed
PAL = "Blues"
m1 = tm_shape(GAtmap3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "deaths", style = "cont",breaks = 10^deathbreaks2, textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="cumulative deaths") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +

  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)


m2 = tm_shape(E_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "deaths", style = "cont", breaks = 10^deathbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected cumulative deaths \nSLM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m3 = tm_shape(S_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "deaths", style = "cont", breaks = 10^deathbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected cumulative deaths \nCSM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m4 = tm_shape(E_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "deaths", style = "cont", breaks = 10^deathbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected cumulative deaths \nSLM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)
		  
m5 = tm_shape(S_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "deaths", style = "cont", breaks = 10^deathbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected cumulative deaths \nCSM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m6 = tmap_arrange(m1,m2,m3,m4,m5,nrow=5)
 tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/deathsproject.png", sep=""))
  tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/deathsproject.pdf", sep=""))


##


### ICUdemand
PAL = "Purples"
m2 = tm_shape(E_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUreq", style = "cont", breaks = 10^icubreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU demand \nSLM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m3 = tm_shape(S_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUreq", style = "cont", breaks = 10^icubreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU demand \nCSM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m4 = tm_shape(E_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUreq", style = "cont", breaks = 10^icubreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU demand \nSLM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)
		  
m5 = tm_shape(S_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUreq", style = "cont", breaks = 10^icubreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU demand \nCSM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m6 = tmap_arrange(m2,m3,m4,m5,nrow=4)
 tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/ICUproject.png", sep=""))
  tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/ICUproject.pdf", sep=""))




### Tot Hosp
PAL = "Greens"
m2 = tm_shape(E_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "totHosp", style = "cont", breaks = 10^tHbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected hospitalisations \nSLM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m3 = tm_shape(S_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "totHosp", style = "cont", breaks = 10^tHbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected hospitalisations \nCSM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m4 = tm_shape(E_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "totHosp", style = "cont", breaks = 10^tHbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected hospitalisations \nSLM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)
		  
m5 = tm_shape(S_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "totHosp", style = "cont", breaks = 10^tHbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected hospitalisations \nCSM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m6 = tmap_arrange(m2,m3,m4,m5,nrow=4)
 tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/totHproject.png", sep=""))
  tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/totHproject.pdf", sep=""))



### Cpd
PAL = "Oranges"
m2 = tm_shape(E_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "cpd", style = "cont", breaks = 10^cpdbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected cases per day \nSLM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m3 = tm_shape(S_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "cpd", style = "cont", breaks = 10^cpdbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected cases per day \nCSM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m4 = tm_shape(E_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "cpd", style = "cont", breaks = 10^cpdbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected cases per day \nSLM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)
		  
m5 = tm_shape(S_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "cpd", style = "cont", breaks = 10^cpdbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected cases per day \nCSM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m6 = tmap_arrange(m2,m3,m4,m5,nrow=4)
 tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/CPDproject.png", sep=""))
  tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/CPDproject.pdf", sep=""))
  


### Dpd
PAL = "Oranges"
m2 = tm_shape(E_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "dpd", style = "cont", breaks = 10^dpdbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected deaths per day \nSLM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m3 = tm_shape(S_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "dpd", style = "cont", breaks = 10^dpdbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected deaths per day \nCSM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m4 = tm_shape(E_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "dpd", style = "cont", breaks = 10^dpdbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected deaths per day \nSLM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)
		  
m5 = tm_shape(S_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "dpd", style = "cont", breaks = 10^dpdbreaks2 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected deaths per day \nCSM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m6 = tmap_arrange(m2,m3,m4,m5,nrow=4)
 tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/DPDproject.png", sep=""))
  tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/DPDproject.pdf", sep=""))  
  

### ICU ratio 1 (C-D)/(C+D)
PAL = "RdBu"

m1 = tm_shape(GA)+
 tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICU", style = "log10",breaks = log10(c(1, 10, 100,600)),textNA = "No data",
          colorNA = "lightgrey", 
          palette = "Reds",
		  title ="reported ICU capacity") +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)


m2 = tm_shape(E_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUrat", style = "cont", breaks = c(-1,0,1), textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU excess \nSLM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m3 = tm_shape(S_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUrat", style = "cont", breaks = c(-1,0,1) , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU excess \nCSM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m4 = tm_shape(E_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUrat", style = "cont", breaks = c(-1,0,1) , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU excess \nSLM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)
		  
m5 = tm_shape(S_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUrat", style = "cont", breaks = c(-1,0,1) , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU excess \nCSM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m6 = tmap_arrange(m1,m2,m3,m4,m5,nrow=5)
 tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/ICUexcessproject.png", sep=""))
  tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/ICUexcessproject.pdf", sep=""))  



### ICU ratio 2 D/C
PAL = "-RdBu"
BREK =c(0,1,1.5,2,5,10)

m1 = tm_shape(GA)+
 tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICU", style = "log10",breaks = log10(c(1, 10, 100,600)),textNA = "No data",
          colorNA = "lightgrey", 
          palette = "Reds",
		  title ="reported ICU capacity") +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)


m2 = tm_shape(E_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUrat2", style = "cont", breaks= BREK,midpoint=1, textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU excess \nSLM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m3 = tm_shape(S_1) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUrat2", style = "cont", breaks= BREK,midpoint=1 , textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU excess \nCSM no social distancing") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m4 = tm_shape(E_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUrat2", style = "cont", breaks= BREK,midpoint=1, textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU excess \nSLM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)
		  
m5 = tm_shape(S_3) + 
  tm_borders(alpha=0.2,lwd=0.5)+
   tm_fill(col = "ICUrat2", style = "cont", breaks= BREK,midpoint=1, textNA = "No data",
          colorNA = "lightgrey", 
          palette = PAL,
		  title ="projected ICU excess \nCSM transmission at 50%") +
  tm_facets(by = "date", free.coords = FALSE ,ncol= 6 ) +
  tm_layout(panel.label.size = 0.8,legend.title.size = 0.8,
          legend.text.size = 0.4, legend.outside.position = "right" , legend.outside.size = .2)

m6 = tmap_arrange(m1,m2,m3,m4,m5,nrow=5)
 tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/ICUexcess2project.png", sep=""))
  tmap_save(m6, width = 1800, height = 1800*0.9, units="px", filename = paste("./figures/ICUexcess2project.pdf", sep=""))  

###
