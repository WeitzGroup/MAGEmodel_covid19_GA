#Functions.R


SumLoggedSquares <- function(x,y){
	if(length(x)==length(y)){
		return(  sum((log10(x/y))^2) /length(x)  )
	}
	else
	{
		stop("lengths do not match")
	}
}

#To read simulation data
READ_SIM_DATA <- function(INPUTSIMNAME,SDNAME,GA,DATES){
	
	SIMNAME = paste("./SimulationOutput/",INPUTSIMNAME,"/",SDNAME,"/",sep="")
	DD<-read.csv(paste(SIMNAME,"GA_county_Killed.csv",sep="")) #deaths
	EE <-read.csv(paste(SIMNAME,"GA_county_criticalHospitalised.csv",sep="")) #ICU
	EE2 <-read.csv(paste(SIMNAME,"GA_county_subacuteHospitalised.csv",sep="")) #subacute
	FF <-read.csv(paste(SIMNAME,"GA_county_deathrateperday.csv",sep="")) # deaths per day
	GG <-read.csv(paste(SIMNAME,"GA_county_newcaserateperday.csv",sep="")) #new cases per day
	HH <-read.csv(paste(SIMNAME,"GA_county_totalHospitalised.csv",sep="")) # total hospitalised
	II <- read.csv(paste(SIMNAME,"GA_county_Susceptibles.csv",sep="")) # total susceptibles
	CC <- read.csv(paste(SIMNAME,"GA_county_cumulativeHospitalised.csv",sep="")) # cumulative hospitalised
	
	SZ <- dim(DD)[2]
	NAMES = colnames(DD)[3:SZ]
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
	EE2<-EE2[,c(1,2,INDXdate+2)]
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
	NUMS10 =c()
	for (aa in 1:length(newnames)){
		Dat = c(Dat,rep(newnames[aa],159))
		NUMS = c(NUMS,DD[,aa+2])
		NUMS2 = c(NUMS2,EE[,aa+2])
		NUMS10 = c(NUMS10,EE2[,aa+2])
		NUMS3 = c(NUMS3,FF[,aa+2])
		NUMS4 = c(NUMS4,GG[,aa+2])
		NUMS5 = c(NUMS5,HH[,aa+2])	
		NUMS6 = c(NUMS6,RAT[,aa])
		NUMS7 = c(NUMS7,II[,aa+2])
		NUMS8 = c(NUMS8,RAT2[,aa])
		NUMS9 = c(NUMS9,CC[,aa+2])
	}
	
	gad = list(FIPS=JJ, County = KK, date=Dat, deaths = NUMS, ICUreq = NUMS2, dpd = NUMS3,cpd = NUMS4, totHosp = NUMS5,ICUrat = NUMS6, Suscep = NUMS7,ICUrat2 = NUMS8,cumH = NUMS9,subacute = NUMS10)
	GAtmap_sim <- merge(GA, gad, by = "FIPS")
	
	return(GAtmap_sim)
}


#To get the data summed over counties by day

ExtractSumThisDate <- function(NUMS,DATES,DATESWANT){
	NUMS_by_date = c()
	for(aa in 1:length(DATESWANT)){
		INDY = which(DATES == DATESWANT[aa])
		NUMS_by_date[aa] = sum(NUMS[INDY])
	}
	return(NUMS_by_date)
}
