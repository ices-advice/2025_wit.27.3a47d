## Run analysis, write model results

## Before: data.RData, model.cfg
## After: fit.RData

library(icesTAF)
library(stockassessment)

mkdir("model")

icesTAF::msg("Fitting the model")

load("data/data.RData")
cfg <- loadConf(dat, "boot/data/sam_config/model.cfg", patch = TRUE)
#cfg$corFlag <- 0
par <- defpar(dat, cfg)
par$logSdLogFsta<-0 # this one is in stockassesment.org
fit <- sam.fit(dat, cfg, par , rel.tol = 1e-8, 	upper=list(itrans_rho = 5))
if (fit$opt$convergence!=0) stop("Model did not converge.")
save(dat, fit, cfg, file = "model/model.RData")

icesTAF::msg("Retro")
RETRO<-retro(fit, year = max(fit$data$years) - c(0:4))
save(RETRO, file="model/retro.RData")

icesTAF::msg("Residuals")
idx<-which(fit$data$aux[,"fleet"]==4)
idx2<-which(fit$data$aux[,"fleet"]!=4)
r1 <- TMB::oneStepPredict(fit$obj, observation.name = "logobs", data.term.indicator = "keep", conditional=idx, discrete=FALSE, trace = FALSE)
r2 <- TMB::oneStepPredict(fit$obj, observation.name = "logobs", data.term.indicator = "keep", subset=idx, discrete=FALSE, trace = FALSE)

ret<-rbind(cbind(fit$data$aux[idx2,],r1),cbind(fit$data$aux[idx,],r2))
attr(ret, "fleetNames") <- attr(fit$data, "fleetNames")
class(ret) <- "samres"
RES <- ret

RESP <- procres(fit)
save(RES, RESP, file="model/residuals.RData")

icesTAF::msg("Forecast")
source("forecasthelpers.R")
source("forecast.R")

# icesTAF::msg("Leave-out runs")
# LO <- leaveout(fit)
# save(LO, file="model/leaveout.RData") 


