####################################################
##-------plot the extracted data from intercatch----
####################################################

datapath <- "data/intercatch/"
path <- "output/IC/"

source("InterCatch_Output_Analysis_functions.r")

#####  save graphs
outputs <- path
dir.create(outputs, showWarnings = FALSE)

################################## READ IN INTERCATCH DATA ###############################

par(ask=TRUE)

StockOverviewFile <- paste(datapath,"StockOverview.txt",sep="")
NumbersAtAgeLengthFile <- paste(datapath,"NumbersAtAgeLength.txt",sep="")

WtData <- readStockOverview(StockOverviewFile,NumbersAtAgeLengthFile)
Ndata <- readNumbersAtAgeLength(NumbersAtAgeLengthFile)

################################## extra processing ####################################
###merge division into subarea
WtData$Area[WtData$Area %in% c("27.4.a", "27.4.b", "27.4.c")] <- "27.4"
WtData$Area[WtData$Area %in% c("27.3.a", "27.3.a.20", "27.3.a.21")] <- "27.3.a"
WtData$Area <- factor(WtData$Area)

################################## PLOT LANDINGS DATA ####################################

par(mfrow=c(1,1))

png(paste(outputs,"landingsWtCoutryFleets%01d.png",sep=''),width = 1200, height = 600, pointsize = 12)
plotStockOverview(WtData,plotType="LandWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

png(paste(outputs,"landingsPercCoutryFleets%01d.png",sep=''),width = 1200, height = 600, pointsize = 12)
plotStockOverview(WtData,plotType="LandPercent",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
byArea=TRUE,countryColours=T,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE)
dev.off()

png(paste(outputs,"DiscardProvisionCoutryFleets%01d.png",sep=''), width = 1200, height = 600, pointsize = 12)
plotStockOverview(WtData,plotType="DiscProvided",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

ProvidedDiscardsArea <- aggregate(list(providedLandings =ProvidedDiscards$x.y), by=list(DiscardProvision=ProvidedDiscards$DRatio, Area=ProvidedDiscards$Group.3), FUN=sum,na.rm=T)
Coverage <- reshape(ProvidedDiscardsArea, idvar="Area", timevar="DiscardProvision", direction="wide")
Coverage$PercCovered <- Coverage$providedLandings.1/(Coverage$providedLandings.1+Coverage$providedLandings.0)

png(paste(outputs,"landingsPercCoutry%01d.png",sep=''),width = 1200, height = 600, pointsize = 12)
plotStockOverview(WtData,plotType="LandPercent",byFleet=F,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=TRUE,countryColours=T,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE)
dev.off()

WtData1 <- WtData[WtData$Area=="27.4",]
png(paste(outputs,"landingsPercSeason%01d.png",sep=''),width = 1200, height = 600, pointsize = 12)
plotStockOverview(WtData1,plotType="LandPercent",byFleet=F,byCountry=F,bySampled=TRUE,bySeason=T,
                  byArea=F,countryColours=T,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE)
dev.off()

png(paste(outputs,"DiscardsWtCoutryFleetArea%01d.png",sep=''), width = 600, height = 600, pointsize = 12)
plotStockOverview(WtData,plotType="DisWt",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=TRUE,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

png(paste(outputs,"DiscardsRatioCoutryFleetArea%01d.png",sep=''), width = 600, height = 600, pointsize = 12)
plotStockOverview(WtData,plotType="DisRatio",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,
                  byArea=T,countryColours=TRUE,set.mar=TRUE,markSampled=TRUE,individualTotals=FALSE)
dev.off()

png(paste(outputs,"AgeDistrib%01d.png",sep=''),width = 600, height = 800, pointsize = 12)
plotAgeDistribution1(Ndata,plotType="perCent", c(4,4))
dev.off()

# subdat <- Ndata[Ndata$Fleet=="TBB_DEF_70-99_0_0_all" & Ndata$Country %in% c("Netherlands", "Germany"),]
# plotAgeDistribution1(subdat, fpar=c(4,4))

tab <- aggregate(CatchWt ~ CatchCat + Sampled, FUN = sum, data = WtData)
write.csv(tab, file = paste0(outputs, "sampled-unsampled.csv"))

