## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(stockassessment)

mkdir("data")

oldwd <- setwd("boot/data/sam_data/")

cn<-read.ices("cn.dat")
cw<-read.ices("cw.dat")
dw<-read.ices("dw.dat")
lw<-read.ices("lw.dat")
mo<-read.ices("mo.dat")
nm<-read.ices("nm.dat")
pf<-read.ices("pf.dat")
pm<-read.ices("pm.dat")
sw<-read.ices("sw.dat")
lf<-read.ices("lf.dat")
surveys<-read.ices("survey.dat")
surveys[1:2] <- lapply(surveys[1:2], function(x)x*1000)

patch <- function(x,with,ally=sort(c(1940:1949,as.integer(rownames(surveys[[3]])), as.integer(rownames(x))))){
  ret<-matrix(NA,ncol=ncol(x),nrow=length(ally))
  rownames(ret)<-ally
  colnames(ret)<-colnames(x)
  idx<-as.integer(ally)%in%as.integer(rownames(x))
  ret[idx,]<-x
  ret[!idx,]<-do.call(rbind, rep(list(with), sum(!idx)) )
  ret
}

cn <- patch(cn,colMeans(cn))
cn[as.integer(rownames(cn))%in%(1945:2008),]<-NA
cw <- patch(cw,colMeans(cw))
dw <- patch(dw,colMeans(dw))
lw <- patch(lw,colMeans(lw))
mo <- patch(mo,colMeans(mo))
nm <- patch(nm,colMeans(nm))
pf <- patch(pf,colMeans(pf))
pm <- patch(pm,colMeans(pm))
sw <- patch(sw,colMeans(sw))
lf <- patch(lf,colMeans(lf))

setwd(oldwd)

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn,
                    prop.mature=mo,
                    stock.mean.weight=sw,
                    catch.mean.weight=cw,
                    dis.mean.weight=dw,
                    land.mean.weight=lw,
                    prop.f=pf,
                    prop.m=pm,
                    natural.mortality=nm,
                    land.frac=lf)


save(dat, file="data/data.RData")

## Unzip intercatch data
unzip("boot/data/intercatch.zip", exdir = "data")

## move ref points 
file.copy(from = "boot/data/wit.27.3a47d_RefPts.csv", "data/wit.27.3a47d_RefPts.csv")
