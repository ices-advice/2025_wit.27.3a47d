################################## readStockOverview ###############################

readStockOverview <- function(StockOverviewFile,NumbersAtAgeLengthFile){

Wdata <- read.table(StockOverviewFile,header=TRUE,sep="\t")
names(Wdata)[7] <- "Fleet"
names(Wdata)[10] <- "CatchWt"
names(Wdata)[11] <- "CatchCat"
names(Wdata)[12] <- "ReportCat"
Wdata <- Wdata[Wdata$Discards.Imported.Or.Raised == "Imported",]

temp <- sum(Wdata$CatchWt[Wdata$CatchCat=="Logbook Registered Discard"])
print(paste("The sum of catches from Logbook Registered Discard is ", temp, " kg. They are excluded from the summary", sep=""))
temp <- sum(Wdata$CatchWt[Wdata$CatchCat=="BMS landing"])
print(paste("The sum of catches from BMS landing is ", temp, " kg. They are re-categorized as Discards", sep=""))
Wdata <- Wdata[Wdata$CatchCat!="Logbook Registered Discard",]
Wdata$CatchCat[Wdata$CatchCat=="BMS landing"] <- "Discards"
Wdata$CatchCat <- as.character(Wdata$CatchCat)

Wdata$CatchCat <- substr(Wdata$CatchCat,1,1)
Wdata <- Wdata[,-ncol(Wdata)]

Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
names(Ndata)[7] <- "CatchCat"
names(Ndata)[9] <- "Fleet"

Wdata <- merge(Wdata,Ndata[,c(3,4,5,7,9,10,11)],by=c("Area","Season","Fleet","Country","CatchCat"),all.x=TRUE)
Wdata$Sampled <- ifelse(is.na(Wdata$SampledCatch),FALSE,TRUE)

return(Wdata)
}

################################## readNumbersAtAgeLength ###############################

readNumbersAtAgeLength <- function(NumbersAtAgeLengthFile){

Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
names(Ndata)[7] <- "CatchCat"
names(Ndata)[8] <- "ReportCat"
names(Ndata)[9] <- "Fleet"
Ndata <- Ndata[,-ncol(Ndata)]

##--------check if there is any gender specific N@age------
ind_gender <- grepl("Male",names(Ndata)) | grepl("Female",names(Ndata))
print("Following strata reported gender specific N@age:")
print(Ndata[which(rowSums(Ndata[,names(Ndata)[ind_gender]])>0),])

if (sum(ind_gender)>0) {
##---merge gender specific N@age into undetermined age---
ind_male   <- grepl("Male",names(Ndata))
ind_female <- grepl("Female",names(Ndata))
ind_un     <- grepl("Undetermined",names(Ndata))
total <- sum(Ndata[,16:ncol(Ndata)])
if (sum(ind_male)>0) {
  Ndata[,ind_un] <- Ndata[,ind_un] + Ndata[,ind_male]
}
if (sum(ind_female)>0) {
  Ndata[,ind_un] <- Ndata[,ind_un] + Ndata[,ind_female]
}
Ndata <- Ndata[,!ind_gender]
print(sum(Ndata[,16:ncol(Ndata)])==total)
} 

ageNames <- names(Ndata)[16:ncol(Ndata)]
ages <- as.numeric(substr(ageNames,16,nchar(ageNames)))
allAges <- min(ages):max(ages)
missingAges <- allAges[allAges %in% ages]
colnames(Ndata)[16:ncol(Ndata)] <- ages
return(Ndata)
}

################################## plotStockOverview ###############################

plotStockOverview <- function(dat,plotType="LandPercent",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,byArea=FALSE,countryColours=NULL,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE,ymax=NULL,fcex.names=0.7){

plotTypes <- c("LandWt","LandPercent","CatchWt","DisWt","DisRatio","DiscProvided")
if (!(plotType %in% plotTypes)) stop(paste("PlotType needs to be one of the following:",paste(plotTypes)))

stock <- dat$Stock[1]

impLand <- dat[dat$CatchCat=="L",]
impDis <- dat[dat$CatchCat=="D",]

nArea <- nSeason <- nCountry <- nFleet <- 1

SeasonNames <- sort(unique(impLand$Season))
AreaNames <- sort(unique(impLand$Area))

countryLegend <- FALSE

if (byFleet) nFleet <- length(unique(c(impLand$Fleet, impDis$Fleet))) ## YV
if (byCountry) { 
  nCountry <- length(unique(c(impLand$Country, impDis$Country))) # YV
  countryLegend <- TRUE
}    
if (byArea) nArea <- length(AreaNames)
if (bySeason) nSeason <- length(SeasonNames)
if (!bySampled) markSampled <- FALSE


if (length(countryColours)==1 &&countryColours){
  #countryColours <- data.frame(
  #"Country"=c("Belgium","Denmark","France","Germany","Netherlands","Norway","Poland","Sweden","UK (England)","UK(Scotland)"),
  #"Colour"=c("green", "red", "darkblue", "black", "orange","turquoise" ,"purple","yellow","magenta","blue")
  #, stringsAsFactors=FALSE)
  countryColours <- data.frame("Country"=unique(dat$Country)[order(unique(dat$Country))],
					"Colour"=rainbow(length(unique(dat$Country)))
					, stringsAsFactors=FALSE)
}
if (length(countryColours)==1 && countryColours==FALSE){
  countryLegend <- FALSE
  #countryColours <- data.frame(
  #"Country"=c("Belgium","Denmark","France","Germany","Norway","Netherlands","Poland","Sweden","UK (England)","UK(Scotland)")
  # , stringsAsFactors=FALSE)
  countryColours <- data.frame("Country"=unique(dat$Country)[order(unique(dat$Country))],
					stringsAsFactors=FALSE)
  countryColours$Colour <- rep("grey",length(countryColours$Country))
}


LsummaryList <- list()
DsummaryList <- list()
summaryNames <- NULL
i <- 1
if (byFleet) {
  LsummaryList[[i]] <- impLand$Fleet
  DsummaryList[[i]] <- impDis$Fleet
  summaryNames <- c(summaryNames,"Fleet")
  i <- i+1
}
if(byCountry) {
  LsummaryList[[i]] <- impLand$Country
  DsummaryList[[i]] <- impDis$Country
  summaryNames <- c(summaryNames,"Country")
  i <- i+1
}
if(bySeason) {
  LsummaryList[[i]] <- impLand$Season
  DsummaryList[[i]] <- impDis$Season
  summaryNames <- c(summaryNames,"Season")
  i <- i+1
}
if (byArea) {
  LsummaryList[[i]] <- impLand$Area
  DsummaryList[[i]] <- impDis$Area
  summaryNames <- c(summaryNames,"Area")
  i <- i+1
}
if (bySampled) {
  LsummaryList[[i]] <- impLand$Sampled
  DsummaryList[[i]] <- impDis$Sampled
  summaryNames <- c(summaryNames,"Sampled")
  i <- i+1
}
byNames <- summaryNames
summaryNames <- c(summaryNames,"CatchWt")

### YV changed
if(plotType%in%c("LandWt","LandPercent")){
	Summary <- aggregate(impLand$CatchWt,LsummaryList,sum)
} else if (plotType=="DisWt"){
	Summary <- aggregate(impDis$CatchWt,DsummaryList,sum)
} else if (plotType=="DisRatio"){
	SummaryD <- aggregate(impDis$CatchWt,DsummaryList,sum)
	SummaryL <- aggregate(impLand$CatchWt,LsummaryList,sum)
	if(bySampled){
		testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD)) - 1]
	} else {
		testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]	
	}
	Summary <- merge(SummaryD, SummaryL, all=T, by=testN)
	Summary$DRatio <- Summary$x.x / (Summary$x.x+Summary$x.y)
	Summary <- Summary[!is.na(Summary$DRatio),c(testN,paste('Group.',length(testN)+1,'.x', sep=''),paste('Group.',length(testN)+1,'.y', sep=''),'DRatio')]
} else if (plotType=="DiscProvided"){
	if(bySampled){
		DsummaryList <- DsummaryList[1: (length(DsummaryList)- 1)]
		LsummaryList <- LsummaryList[1: (length(LsummaryList)- 1)]
	} else {
		DsummaryList <- DsummaryList[ length(DsummaryList)]
		LsummaryList <- LsummaryList[ length(LsummaryList)]
	}
	SummaryD <- aggregate(impDis$CatchWt,DsummaryList,sum)
	SummaryL <- aggregate(impLand$CatchWt,LsummaryList,sum)
	if(bySampled){
		testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]
	} else {
		testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]	
	}
	Summary <- merge(SummaryD, SummaryL, all=T, by=testN)
	Summary$DRatio <- Summary$x.x / (Summary$x.x+Summary$x.y)
	Summary <- Summary[,c(testN,'DRatio','x.y')]
	Summary$DRatio[!is.na(Summary$DRatio)] <- TRUE
	Summary$DRatio[is.na(Summary$DRatio)] <- FALSE
	Summary <- unique(Summary) 
	ProvidedDiscards <<- Summary
	
}
### end YV changed


#disSummary <- aggregate(impDis$CatchWt,DsummaryList,sum) YV
if (plotType!="DisRatio") {
	names(Summary) <- summaryNames #YV
} else {
	names(Summary) <- c(summaryNames[-c(grep(c('Sampled'),summaryNames), grep(c('CatchWt'),summaryNames))],
						"SampledD", "SampledL", "DRatio")
}
#names(disSummary) <- summaryNames # yv
#names(disSummary) <- c("Fleet" ,  "Country", "Area" ,   "SampledD" ,"DisWt") # yv
#names(landSummary) <- c("Fleet" ,  "Country", "Area" ,   "SampledL" ,"LandWt") # yv


#stratumSummary <- merge(landSummary,disSummary,by=byNames,all=TRUE) #YV
stratumSummary <- Summary #YV
if(plotType%in%c("LandWt","LandPercent","DiscProvided")){
names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "LandWt" # YV
} else if (plotType=="DisWt"){
names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "DisWt" # YV
} 

#names(stratumSummary)[names(stratumSummary)=="CatchWt.y"] <- "DisWt"  # YV

#if (bySampled ) {
##  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$LandWt)),
#	if(plotType%in%c("LandWt","LandPercent")){
#	  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$LandWt)),]
#	} else if (plotType=="DisWt"){
#	  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$DisWt)),]
#	}
#} else {
#	if(plotType%in%c("LandWt","LandPercent")){
#	  stratumSummary <- stratumSummary[rev(order(stratumSummary$LandWt)),]
#	} else if (plotType=="DisWt"){
#	  stratumSummary <- stratumSummary[rev(order(stratumSummary$DisWt)),]
#	}
#}
if (bySampled ) {
	if(plotType!="DisRatio"){
	stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary[,dim(stratumSummary)[2]])),]
	} else {
	stratumSummary <- stratumSummary[rev(order(stratumSummary$SampledL,stratumSummary$SampledD,stratumSummary[,dim(stratumSummary)[2]])),]	
	}
} else {
	stratumSummary <- stratumSummary[rev(order(stratumSummary[,dim(stratumSummary)[2]])),]
}

#catchData <- matrix(c(stratumSummary$LandWt,stratumSummary$DisWt),byrow=TRUE,nrow=2) YV
catchData <- stratumSummary[,dim(stratumSummary)[2]] #YV


if (set.mar) par(mar=c(8,4,1,1)+0.1) 
                                       
for (a in 1:nArea) {
#windows()
  if (bySeason & !(byCountry | byFleet)) nSeason <- 1
  for (s in 1:nSeason) {
    area <- AreaNames[a]
    season <- SeasonNames[s]

    indx <- 1:nrow(stratumSummary)
    if (bySeason & !byArea & (byCountry | byFleet)) indx <- stratumSummary$Season==season 
    if (!bySeason & byArea) indx <- stratumSummary$Area==area 
    if (bySeason & byArea & (byCountry | byFleet | bySampled)) indx <- stratumSummary$Area==area & stratumSummary$Season==season

    if (individualTotals) {
      sumLandWt <- sum(stratumSummary$LandWt[indx],na.rm=TRUE)
    } else {
      sumLandWt <- sum(stratumSummary$LandWt,na.rm=TRUE)
    }  
    if(byCountry) {
      colVec <- countryColours$Colour[match(stratumSummary$Country[indx],countryColours$Country)]
    } else {
      colVec <- "grey"
    }  
        
    if (plotType%in%c("LandWt","DiscProvided")) yvals <- stratumSummary$LandWt[indx]
    if (plotType=="LandPercent") yvals <- 100*stratumSummary$LandWt[indx]/sumLandWt    
    if (plotType=="DisWt") yvals <- stratumSummary$DisWt[indx] 
    if (plotType=="CatchWt") yvals <- catchData[,indx] 
    if (plotType=="DisRatio") yvals <- stratumSummary$DRatio

if(plotType!="DisRatio"){
 
    if (!is.null(ymax)) newYmax <- ymax
    if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
    if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
    #if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE) #YV
    #if (plotType=="CatchWt") colVec <- c("grey","black") #YV
    #if (plotType=="CatchWt") countryLegend <- FALSE #YV
    if (markSampled) newYmax <- 1.06*newYmax
    if (byFleet) namesVec=stratumSummary$Fleet[indx]
    if (!byFleet) {
      if (byArea & bySeason) namesVec=paste(stratumSummary$Area[indx],stratumSummary$Season[indx]) 
      if (!byCountry & !byArea & bySeason) namesVec=paste(stratumSummary$Season[indx]) 
      if (byCountry) namesVec=paste(stratumSummary$Country[indx]) 
      if (bySampled & !byCountry & nArea==1 & nSeason==1) namesVec=paste(stratumSummary$Season[indx]) 
    }
	

    cumulativeY <- cumsum(yvals)
    yvals[yvals>newYmax] <- newYmax

    if ((newYmax==-Inf)) {
      plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
      box()
    } else {
      b <- barplot(yvals,names=namesVec,las=2,cex.names=fcex.names,col=colVec,ylim=c(0,newYmax),yaxs="i")   

    if (bySampled & markSampled) {
		  nSampled <- sum(stratumSummary$Sampled[indx])
		if(nSampled>0){
			  arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
			  arrows(b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
			if(plotType!="DiscProvided"){
			  text((b[nSampled]+b[1])/2,newYmax*104/106,"sampled",cex=0.8)
			  text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"unsampled",cex=0.8)
			}else{
			  text((b[nSampled]+b[1])/2,newYmax*104/106,"Landings with Discards",cex=0.8)
			  text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"no Discards",cex=0.8)	
			}
		} else {
			  arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
			if(plotType!="DiscProvided"){
			  text(b[length(b)]+(b[2]-b[1])/4,newYmax*104/106,"unsampled",cex=0.8)
			}else{
			  text(b[length(b)]+(b[2]-b[1])/4,newYmax*104/106,"no Discards",cex=0.8)
			}
		}
	}
    if (countryLegend) legend("topright",inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
    box()
    if (plotType=="LandPercent") lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
    if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
    if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
    if (plotType=="LandPercent") abline(h=100)
    }
    if (!bySeason & !byArea) title.txt <- paste(stock)
    if (!bySeason & byArea) title.txt <- paste(stock,area)
    if (bySeason & !byArea & !(byCountry | byFleet | bySampled)) title.txt <- paste(stock)
    if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock,season)
    if (bySeason & byArea) title.txt <- paste(stock,area,season)
    title.txt <- paste(title.txt,plotType)
    title(title.txt)
} else {
	par(mfrow=c(2,2))
	listSample <- unique(paste(stratumSummary$SampledD,stratumSummary$SampledL))
	for(i in 1:length(listSample)){
	idx <- which(stratumSummary$SampledD[indx]==strsplit(listSample[i],' ')[[1]][1] & stratumSummary$SampledL[indx]==strsplit(listSample[i],' ')[[1]][2])
	if(length(idx)>0){
		if(byCountry) {
		  colVec <- countryColours$Colour[match(stratumSummary$Country[indx][idx],countryColours$Country)]
		} else {
		  colVec <- "grey"
		}  

		if (!is.null(ymax)) newYmax <- ymax
		if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
		if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
		#if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE) #YV
		#if (plotType=="CatchWt") colVec <- c("grey","black") #YV
		#if (plotType=="CatchWt") countryLegend <- FALSE #YV
		if (markSampled) newYmax <- 1.06*newYmax
		if (byFleet) namesVec=stratumSummary$Fleet[indx][idx]
		if (!byFleet) {
		  if (byArea & bySeason) namesVec=paste(stratumSummary$Area[idx],stratumSummary$Season[indx][idx]) 
		  if (!byCountry & !byArea & bySeason) namesVec=paste(stratumSummary$Season[indx][idx]) 
		  if (byCountry) namesVec=paste(stratumSummary$Country[indx][idx]) 
		  if (bySampled & !byCountry & nArea==1 & nSeason==1) namesVec=paste(stratumSummary$Season[indx][idx]) 
		}


			cumulativeY <- cumsum(yvals[indx][idx])
			yvals[yvals>newYmax] <- newYmax

			if ((newYmax==-Inf)) {
			  plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
			  box()
			} else {
			  b <- barplot(yvals[indx][idx],names=namesVec,las=2,cex.names=fcex.names,col=colVec,ylim=c(0,newYmax),yaxs="i")   

			#if (bySampled & markSampled) {
			#  nSampledD <- sum(stratumSummary$SampledD[idx]==T)
			#  nSampledL <- sum(stratumSummary$SampledL[idx]==T)
			#  if (nSampledD>0) arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampledD]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
			#  if (nSampledD>0) arrows(b[nSampledD]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 

			#  if (nSampledL>0) arrows(b[1]-(b[2]-b[1])/2,newYmax*92/106,b[nSampledL]+(b[2]-b[1])/2,newYmax*92/106,code=3,length=0.1) 
			#  if (nSampledL>0) arrows(b[nSampledL]+(b[2]-b[1])/2,newYmax*92/106,b[length(b)]+(b[2]-b[1])/2,newYmax*92/106,code=3,length=0.1) 

			#  } 
			if (countryLegend) legend("topright",inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
			box()
			if (plotType=="LandPercent") lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
			if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
			if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
			if (plotType=="LandPercent") abline(h=100)
			}
			if (!bySeason & !byArea) title.txt <- paste(stock)
			if (!bySeason & byArea) title.txt <- paste(stock,area)
			if (bySeason & !byArea & !(byCountry | byFleet | bySampled)) title.txt <- paste(stock)
			if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock,season)
			if (bySeason & byArea) title.txt <- paste(stock,area,season)
			title.txt <- paste(title.txt,plotType, "D/L", listSample[i])
			title(title.txt)
		}
	}
}
	}
}                                                                                                                
}

############################ plotAgeDistribution ###############################

plotAgeDistribution1 <- function(dat,plotType="perCent", fpar) {

plotTypes <- c("perCent","frequency")

if (!(plotType %in% plotTypes)) 
  stop(paste("plotType needs to be one of the following:", paste(plotTypes,collapse=", ")))

sampAge <- dat
ageCols <- 16:ncol(dat)

 

if (TRUE) {
par(mar=c(4,2,1,1)+0.1)
par(mfcol=c(10,5))

year <- sampAge$Year[1]

sampAge$AFC <- paste(sampAge$Area,sampAge$Fleet,sampAge$Country)
AFClist <- sort(unique(sampAge$AFC))
nAFC <- length(AFClist)

overallAgeDist <- list()
overallAgeDist[["L"]] <- colSums(sampAge[sampAge$CatchCat=="L" ,ageCols])/sum(sampAge[sampAge$CatchCat=="L" ,ageCols])
overallAgeDist[["D"]] <- colSums(sampAge[sampAge$CatchCat=="D" ,ageCols])/sum(sampAge[sampAge$CatchCat=="D" ,ageCols])
par(mfrow=fpar)
ymax <- max(sampAge[sampAge$Country!="UK(Scotland)",ageCols])
for (i  in 1:nAFC) {
  for (catch in c("L","D")) {
    for (season in 1:4) {
      if (sampAge[sampAge$AFC==AFClist[i],"Country"][1]=="UK(Scotland)" & catch=="D" & season==1) {
        season <- year
      }
      indx <- sampAge$AFC==AFClist[i] & sampAge$CatchCat==catch & sampAge$Season==season
      dat <- sampAge[indx,]
      if (nrow(dat)==0) {
        plot(0,0,type="n",xlab="",ylab="")
      } else {
        ageDist <- as.matrix(dat[,ageCols],nrow=nrow(dat),ncol=length(ageCols))
        if (plotType=="perCent") ageDist <- ageDist/rowSums(ageDist)
        newYmax <- max(sampAge[sampAge$AFC==AFClist[i],ageCols],na.rm=TRUE)
        if (plotType=="perCent") newYmax <- max(ageDist)
        newYmax <- 1.05*newYmax
        #newYmax <- max(ageDist,na.rm=TRUE)
        barplot(ageDist,ylim=c(0,newYmax),las=2)
        box()
      }
      title(paste(AFClist[i],catch,season),cex.main=0.7,line=0.5)
     }
  }
}
}

}







############################ TestAgeDistribution ###############################

plotAgeDistribution <- function(dat,aggregateOn="All",plotType="perCent",fleet="TBB_DEF_70-99", catch="D") {

	plotTypes <- c("perCent","frequency")
	aggregateOns <- c("All")

	if (!(plotType %in% plotTypes)) 
	  stop(paste("plotType needs to be one of the following:", paste(plotTypes,collapse=", ")))
	if (!(aggregateOn %in% aggregateOns)) 
	  stop(paste("aggregateOn needs to be one of the following:", paste(aggregateOns,collapse=", ")))


	sampAge <- dat
	ageCols <- 16:ncol(sampAge)

	if(fleet!="All") sampAge <- subset(sampAge,substr(Fleet,1,nchar(fleet))%in%fleet)
	sampAge <- subset(sampAge,CatchCat%in%catch)
	
	
	 countryColours <- data.frame(
	  "Country"=c("Belgium","Denmark","France","Germany","Netherlands","Norway","Poland","Sweden","UK (England)","UK(Scotland)"),
	  "Colour"=c("green", "red", "darkblue", "black", "orange","turquoise" ,"purple","yellow","magenta","blue")
	  , stringsAsFactors=FALSE)

	if(aggregateOn=="All") agg <- which(colnames(sampAge)%in%c('Fleet','Country','Area','Season'));varNames <- c('Fleet','Country','Area','Season')


	if (TRUE) {
		#par(mar=c(4,2,1,1)+0.1)
		#par(mfcol=c(4,2))

		year <- sampAge$Year[1]

		sampAge$AFC <- paste(sampAge$Area,sampAge$Fleet,sampAge$Country)
		AFClist <- sort(unique(sampAge$AFC))
		nAFC <- length(AFClist)


		ymax <- max(sampAge[sampAge$Country!="UK(Scotland)",ageCols])
		# to be changed to agg for (i  in 1:nAFC) {
		ageDist <- sampAge[,c(ageCols,agg)]
		newYmax <- max(ageDist[,which(substr(colnames(ageDist),1,3)=="Age")],na.rm=TRUE)
				if (plotType=="perCent") {
					ageDist[,which(substr(colnames(ageDist),1,3)=="Age")] <- ageDist[,which(substr(colnames(ageDist),1,3)=="Age")]/rowSums(ageDist[,which(substr(colnames(ageDist),1,3)=="Age")])
					newYmax <- max(ageDist[,which(substr(colnames(ageDist),1,3)=="Age")],na.rm=TRUE)
				}
				newYmax <- 1.05*newYmax
				
				tmp <- reshape(ageDist,idvar=varNames,varying=list(which(substr(colnames(ageDist),1,3)=="Age")),v.names = "Value",direction='long' )
				tmp <- merge(tmp,countryColours,all.x=T)  
				tmp$fleetType <- as.numeric(tmp$Fleet)
				
				for (i in 1:length(varNames)){ 
					if (i==1){ 
					tmp$agg <- tmp[,varNames[i]]
					}else{
					tmp$agg <- paste(tmp$agg,tmp[,varNames[i]])					
					}
				}
				
				tmp <- tmp[order(tmp$agg,tmp$time),]
				xyplot(Value~time,groups=agg,data=tmp,type='l',col=tmp$Season[tmp$tim==1],auto.key =
					list(space = "top", points = FALSE, lines = TRUE))
				xyplot(Value~time|Season,groups=agg,data=tmp,type='l',col=tmp$Country[tmp$tim==1])
		
	}
			  title(paste(AFClist[i],catch,season),cex.main=0.7,line=0.5)
}

############################ TestAgeDistribution ###############################

plotAgeDistribution <- function(dat,aggregateOn="All",plotType="perCent",fleet="OTB_DEF_70-99", catch="L") {

	plotTypes <- c("perCent","frequency")
	aggregateOns <- c("All")

	if (!(plotType %in% plotTypes)) 
	  stop(paste("plotType needs to be one of the following:", paste(plotTypes,collapse=", ")))
	if (!(aggregateOn %in% aggregateOns)) 
	  stop(paste("aggregateOn needs to be one of the following:", paste(aggregateOns,collapse=", ")))


	sampAge <- dat
	ageCols <- 16:ncol(sampAge)

	if(fleet!="All") sampAge <- subset(sampAge,substr(Fleet,1,nchar(fleet))%in%fleet)
	sampAge <- subset(sampAge,CatchCat%in%catch)
	
	
	 countryColours <- data.frame(
	  "Country"=c("Belgium","Denmark","France","Germany","Netherlands","Norway","Poland","Sweden","UK (England)","UK(Scotland)"),
	  "Colour"=c("green", "red", "darkblue", "black", "orange","turquoise" ,"purple","yellow","magenta","blue")
	  , stringsAsFactors=FALSE)

	if(aggregateOn=="All") agg <- which(colnames(sampAge)%in%c('Fleet','Country','Area','Season'));varNames <- c('Fleet','Country','Area','Season')


	if (TRUE) {
		#par(mar=c(4,2,1,1)+0.1)
		#par(mfcol=c(4,2))

		year <- sampAge$Year[1]

		sampAge$AFC <- paste(sampAge$Area,sampAge$Fleet,sampAge$Country)
		AFClist <- sort(unique(sampAge$AFC))
		nAFC <- length(AFClist)


		ymax <- max(sampAge[sampAge$Country!="UK(Scotland)",ageCols])
		# to be changed to agg for (i  in 1:nAFC) {
		ageDist <- sampAge[,c(ageCols,agg)]
		newYmax <- max(ageDist[,which(substr(colnames(ageDist),1,3)=="Age")],na.rm=TRUE)
				if (plotType=="perCent") {
					ageDist[,which(substr(colnames(ageDist),1,3)=="Age")] <- ageDist[,which(substr(colnames(ageDist),1,3)=="Age")]/rowSums(ageDist[,which(substr(colnames(ageDist),1,3)=="Age")])
					newYmax <- max(ageDist[,which(substr(colnames(ageDist),1,3)=="Age")],na.rm=TRUE)
				}
				newYmax <- 1.05*newYmax
				
				tmp <- reshape(ageDist,idvar=varNames,varying=list(which(substr(colnames(ageDist),1,3)=="Age")),v.names = "Value",direction='long' )
				tmp <- merge(tmp,countryColours,all.x=T)  
				tmp$fleetType <- as.numeric(tmp$Fleet)
				
				for (i in 1:length(varNames)){ 
					if (i==1){ 
					tmp$agg <- tmp[,varNames[i]]
					}else{
					tmp$agg <- paste(tmp$agg,tmp[,varNames[i]])					
					}
				}
				
				tmp <- tmp[order(tmp$agg,tmp$time),]
				xyplot(Value~time,groups=agg,data=tmp,type='l',col=tmp$Season[tmp$tim==1],auto.key =
					list(space = "top", points = FALSE, lines = TRUE))
				xyplot(Value~time|Season,groups=agg,data=tmp,type='l',col=tmp$Country[tmp$tim==1],auto.key =
					list(space = "top", points = FALSE, lines = TRUE))
		
	}
			  title(paste(AFClist[i],catch,season),cex.main=0.7,line=0.5)
}







############################ TestAgeDistribution ###############################

plotAgeDistribution <- function(dat,aggregateOn="All",plotType="perCent",fleet="All", catch="L") {

	plotTypes <- c("perCent","frequency")
	aggregateOns <- c("All")

	if (!(plotType %in% plotTypes)) 
	  stop(paste("plotType needs to be one of the following:", paste(plotTypes,collapse=", ")))
	if (!(aggregateOn %in% aggregateOns)) 
	  stop(paste("aggregateOn needs to be one of the following:", paste(aggregateOns,collapse=", ")))


	sampAge <- dat
	ageCols <- 16:ncol(sampAge)

	if(fleet!="All") sampAge <- subset(sampAge,substr(Fleet,1,nchar(fleet))%in%fleet)
	sampAge <- subset(sampAge,CatchCat%in%catch)
	
	
	 countryColours <- data.frame(
	  "Country"=c("Belgium","Denmark","France","Germany","Netherlands","Norway","Poland","Sweden","UK (England)","UK(Scotland)"),
	  "Colour"=c("green", "red", "darkblue", "black", "orange","turquoise" ,"purple","yellow","magenta","blue")
	  , stringsAsFactors=FALSE)

	if(aggregateOn=="All") agg <- which(colnames(sampAge)%in%c('Fleet','Country','Area','Season'));varNames <- c('Fleet','Country','Area','Season')


	if (TRUE) {
		#par(mar=c(4,2,1,1)+0.1)
		#par(mfcol=c(4,2))

		year <- sampAge$Year[1]

		sampAge$AFC <- paste(sampAge$Area,sampAge$Fleet,sampAge$Country)
		AFClist <- sort(unique(sampAge$AFC))
		nAFC <- length(AFClist)


		ymax <- max(sampAge[sampAge$Country!="UK(Scotland)",ageCols])
		# to be changed to agg for (i  in 1:nAFC) {
		ageDist <- sampAge[,c(ageCols,agg)]
		newYmax <- max(ageDist[,which(substr(colnames(ageDist),1,3)=="Age")],na.rm=TRUE)
				if (plotType=="perCent") {
					ageDist[,which(substr(colnames(ageDist),1,3)=="Age")] <- ageDist[,which(substr(colnames(ageDist),1,3)=="Age")]/rowSums(ageDist[,which(substr(colnames(ageDist),1,3)=="Age")])
					newYmax <- max(ageDist[,which(substr(colnames(ageDist),1,3)=="Age")],na.rm=TRUE)
				}
				newYmax <- 1.05*newYmax
				
				tmp <- reshape(ageDist,idvar=varNames,varying=list(which(substr(colnames(ageDist),1,3)=="Age")),v.names = "Value",direction='long' )
				tmp <- merge(tmp,countryColours,all.x=T)  
				tmp$fleetType <- as.numeric(tmp$Fleet)
				
				for (i in 1:length(varNames)){ 
					if (i==1){ 
					tmp$agg <- tmp[,varNames[i]]
					}else{
					tmp$agg <- paste(tmp$agg,tmp[,varNames[i]])					
					}
				}
				
				tmp <- tmp[order(tmp$agg,tmp$time),]
				xyplot(Value~time,groups=agg,data=tmp,type='l',col=tmp$Season[tmp$tim==1],auto.key =
					list(space = "top", points = FALSE, lines = TRUE))
				xyplot(Value~time|Season,groups=agg,data=tmp,type='l',col=tmp$Country[tmp$tim==1],auto.key =
					list(space = "top", points = FALSE, lines = TRUE))
		
	}
			  title(paste(AFClist[i],catch,season),cex.main=0.7,line=0.5)
}






################################## tableStockOverview ###############################

tableStockOverview <- function(dat,plotType="LandPercent",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,byArea=FALSE,countryColours=NULL,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE,ymax=NULL){

plotTypes <- c("LandWt","LandPercent","CatchWt","DisWt","DisRatio")
if (!(plotType %in% plotTypes)) stop(paste("PlotType needs to be one of the following:",paste(plotTypes)))

stock <- dat$Stock[1]

impLand <- dat[dat$CatchCat=="L",]
impDis <- dat[dat$CatchCat=="D",]

nArea <- nSeason <- nCountry <- nFleet <- 1

SeasonNames <- sort(unique(impLand$Season))
AreaNames <- sort(unique(impLand$Area))

countryLegend <- FALSE

if (byFleet) nFleet <- length(unique(impLand$Fleet))
if (byCountry) { 
  nCountry <- length(unique(impLand$Country))
  countryLegend <- TRUE
}    
if (byArea) nArea <- length(AreaNames)
if (bySeason) nSeason <- length(SeasonNames)
if (!bySampled) markSampled <- FALSE


LsummaryList <- list()
DsummaryList <- list()
summaryNames <- NULL
i <- 1
if (byFleet) {
  LsummaryList[[i]] <- impLand$Fleet
  DsummaryList[[i]] <- impDis$Fleet
  summaryNames <- c(summaryNames,"Fleet")
  i <- i+1
}
if(byCountry) {
  LsummaryList[[i]] <- impLand$Country
  DsummaryList[[i]] <- impDis$Country
  summaryNames <- c(summaryNames,"Country")
  i <- i+1
}
if(bySeason) {
  LsummaryList[[i]] <- impLand$Season
  DsummaryList[[i]] <- impDis$Season
  summaryNames <- c(summaryNames,"Season")
  i <- i+1
}
if (byArea) {
  LsummaryList[[i]] <- impLand$Area
  DsummaryList[[i]] <- impDis$Area
  summaryNames <- c(summaryNames,"Area")
  i <- i+1
}
if (bySampled) {
  LsummaryList[[i]] <- impLand$Sampled
  DsummaryList[[i]] <- impDis$Sampled
  summaryNames <- c(summaryNames,"Sampled")
  i <- i+1
}
byNames <- summaryNames
summaryNames <- c(summaryNames,"CatchWt")

landSummary <- aggregate(impLand$CatchWt,LsummaryList,sum)
disSummary <- aggregate(impDis$CatchWt,DsummaryList,sum)
names(landSummary) <- summaryNames
names(disSummary) <- summaryNames

stratumSummary <- merge(landSummary,disSummary,by=byNames,all=TRUE)
names(stratumSummary)[names(stratumSummary)=="CatchWt.x"] <- "LandWt"
names(stratumSummary)[names(stratumSummary)=="CatchWt.y"] <- "DisWt"

if (bySampled ) {
  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$LandWt)),]
} else {
  stratumSummary <- stratumSummary[rev(order(stratumSummary$LandWt)),]
}
catchData <- matrix(c(stratumSummary$LandWt,stratumSummary$DisWt),byrow=TRUE,nrow=2)

if (set.mar) par(mar=c(8,4,1,1)+0.1) 
                                       
for (a in 1:nArea) {
  if (bySeason & !(byCountry | byFleet)) nSeason <- 1
  for (s in 1:nSeason) {
    area <- AreaNames[a]
    season <- SeasonNames[s]

    indx <- 1:nrow(stratumSummary)
    if (bySeason & !byArea & (byCountry | byFleet)) indx <- stratumSummary$Season==season 
    if (!bySeason & byArea) indx <- stratumSummary$Area==area 
    if (bySeason & byArea & (byCountry | byFleet | bySampled)) indx <- stratumSummary$Area==area & stratumSummary$Season==season

    if (individualTotals) {
      sumLandWt <- sum(stratumSummary$LandWt[indx],na.rm=TRUE)
    } else {
      sumLandWt <- sum(stratumSummary$LandWt,na.rm=TRUE)
    }  
    if(byCountry) {
      colVec <- countryColours$Colour[match(stratumSummary$Country[indx],countryColours$Country)]
    } else {
      colVec <- "grey"
    }  
        
    if (plotType=="LandWt") yvals <- stratumSummary$LandWt[indx]
    if (plotType=="LandPercent") yvals <- 100*stratumSummary$LandWt[indx]/sumLandWt    
    if (plotType=="DisWt") yvals <- stratumSummary$DisWt[indx]
    if (plotType=="CatchWt") yvals <- catchData[,indx]
    if (plotType=="DisRatio") yvals <- 100*stratumSummary$DisWt[indx]/stratumSummary$LandWt[indx]

    if (!is.null(ymax)) newYmax <- ymax
    if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
    if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
    if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE)
    if (plotType=="CatchWt") colVec <- c("grey","black")
    if (plotType=="CatchWt") countryLegend <- FALSE
    if (markSampled) newYmax <- 1.06*newYmax
    if (byFleet) namesVec=stratumSummary$Fleet[indx]
    if (!byFleet) {
      if (byArea & bySeason) namesVec=paste(stratumSummary$Area[indx],stratumSummary$Season[indx]) 
      if (!byCountry & !byArea & bySeason) namesVec=paste(stratumSummary$Season[indx]) 
      if (byCountry) namesVec=paste(stratumSummary$Country[indx]) 
      if (bySampled & !byCountry & nArea==1 & nSeason==1) namesVec=paste(stratumSummary$Season[indx]) 
    }
    cumulativeY <- cumsum(yvals)
    yvals[yvals>newYmax] <- newYmax

    if ((newYmax==-Inf)) {
      plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
      box()
    } else {
      b <- barplot(yvals,names=namesVec,las=2,cex.names=fcex.names,col=colVec, ylim=c(0,newYmax),yaxs="i")   

    if (bySampled & markSampled) {
      nSampled <- sum(stratumSummary$Sampled[indx])
      arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
      arrows(b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
      text((b[nSampled]+b[1])/2,newYmax*104/106,"sampled",cex=0.8)
      text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"unsampled",cex=0.8)
    } 
    if (countryLegend) legend("topright",inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
    box()
    if (plotType=="LandPercent") lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
    if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
    if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
    if (plotType=="LandPercent") abline(h=100)
    }
    if (!bySeason & !byArea) title.txt <- paste(stock)
    if (!bySeason & byArea) title.txt <- paste(stock,area)
    if (bySeason & !byArea & !(byCountry | byFleet | bySampled)) title.txt <- paste(stock)
    if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock,season)
    if (bySeason & byArea) title.txt <- paste(stock,area,season)
    title.txt <- paste(title.txt,plotType)
    title(title.txt)
  }
}                                                                                                                
}




