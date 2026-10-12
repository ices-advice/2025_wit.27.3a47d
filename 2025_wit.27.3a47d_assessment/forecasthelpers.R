## rounding function
rf <- if (require(icesAdvice, warn.conflicts=FALSE, quietly=TRUE)) {
  icesAdvice::icesRound
} else {
  function(x, ...) round(x, 2)
}

## Check if all equal
check_equal <- function(x) {
  if(mean(x) - x[1] != 0) warning("Not all values are the same")
  mean(x)
}

## Set assessment year
ay <- as.numeric(tail(rownames(stockassessment::fbartable(fit)), 1)) + 1
perc_change <- function(s,e) rf((e - s) / s * 100)

## Helper to make scenarios from a list of
make_scenarios <- function(s = seq(0.1, 0.6, by = 0.1)) {
  eval(parse(text =paste("list(", paste0('"F = ', s, '" = list(fval = c(Fsq,F_int,', s,',', s,'))', collapse=",\n"), ")")))
}

## Squared difference of SSB at the end of the forecast to a target SSB
op <- function(Fm, target) {
  args <- c(forecastArgs, list(args = I(list(fval = c(Fsq,F_int,Fm,Fm))), nm = paste("F =", Fm)))
  res <- do.call(run_one_scenario, args)
  (attr(res, "tab")[4,7] - target)^2
}

## Squared difference of TAC at the end of the forecast to a target TAC
op_catch <- function(Fm, target) {
  args <- c(forecastArgs, list(args = I(list(fval = c(Fsq,Fm,Fmsy,Fmsy))), nm = paste("F =", Fm)))
  res <- do.call(run_one_scenario, args)
  (attr(res, "tab")[2,10] - target)^2
}

## Run one scenario
run_one_scenario <- function(args, nm="", ave.years, rec.years, splitLD = TRUE, nosim = 1001, savesim = FALSE) {
  set.seed(12345)
  ARGS <- args
  ARGS <- c(ARGS,
            list(fit = fit, ave.years=ave.years, rec.years=rec.years, label=nm, splitLD=splitLD, nosim = nosim, savesim = savesim))
  res <- do.call(forecast_with_natage, ARGS)
  print(paste0("forecast : ", "'", nm, "'", " is complete"))
  res
}

getForecastTable <- function(fc, catchyr = 2019, ssbyr = 2021, advice, splitLD = FALSE) {
  Basis <- attr(fc, "label")
  tb <- attr(fc, "tab")
  tb <- tb[, grepl("median", colnames(tb))]

  ## Get relevant rows
  yrs <- as.numeric(rownames(tb))
  cr <- yrs == catchyr
  br <- yrs == ssbyr
  br_min1 <- yrs == (ssbyr - 1)
  br_plus1 <- yrs == (ssbyr + 1)
  ## Get relevant columns
  cc <- grepl("catch", colnames(tb))
  ftc <- grepl("fbar:", colnames(tb))
  fwc <- grepl("fbarL", colnames(tb))
  fuc <- grepl("fbarD", colnames(tb))
  bc <- grepl("ssb", colnames(tb))
  wc <- grepl("Land", colnames(tb))
  uc <- grepl("Disc", colnames(tb))

  ## Make data frame with the information needed
  res <- data.frame(Basis,
                    as.character(round(tb[cr, cc])),
                    as.character(round(tb[cr, wc])),
                    as.character(round(tb[cr, uc])),
                    as.character(rf(tb[cr, ftc])),
                    as.character(rf(tb[cr, fwc])),
                    as.character(rf(tb[cr, fuc])),
                    as.character(round(tb[br, bc])),
                    as.character(round(tb[br_plus1, bc])),
                    as.character(perc_change(tb[br_min1, bc], tb[br, bc])),
                    as.character(perc_change(advice, tb[cr, cc])),
                    stringsAsFactors = FALSE)
  names(res) <- c("Basis",
                  paste0("Total catch (", catchyr, ")"),
                  paste0("Projected landings (", catchyr, ")"),
                  paste0("Projected discards (", catchyr, ")"),
                  paste0("Ftotal ages 4-8 (", catchyr - 1, " & ", catchyr, ")"),
                  paste0("Flandings (", catchyr, ")"),
                  paste0("Fdiscards (", catchyr, ")"),
                  paste0("SSB (", ssbyr, ")"),
                  paste0("SSB (", ssbyr + 1, ")"),
                  "% SSB change",
                  "% Advice change"
  )
  if(splitLD) {
    res ##[, -c(6,7)]
  } else {
    res[, -c(3,4, 6,7)]
  }
}

makeAssumtpionTable <- function(intermyr, digits = 0, Ay, Ry) {
  ## ssblast <- round(s[nrow(s), startsWith(snm, "SSB")] / 1000, digits = digits)
  nms <- colnames(attr(FC[[1]], "tab"))
  recs <- sapply(FC, function(x) attr(x, "tab")[2:4, nms == "rec:median"])
  #rec <- check_equal(recs)
  rec <- exp(mean((attributes(FC[[5]])$fit$pl$logN[1, fit$data$years %in% Ry])))
  ssbs <- sapply(FC, function(x) attr(x, "tab")[1:4, nms == "ssb:median"])
  iy_row <- which(row.names(ssbs) == as.character(ay))
  ssblast <- check_equal(ssbs[iy_row, ])
  catches <- sapply(FC, function(x) attr(x, "tab")[1:4, nms == "catch:median"])
  catchlast <- check_equal(catches[iy_row, ]) ## tail(catchtable(fit), 1)[1]
  wcl <- check_equal(sapply(FC, function(x) attr(x, "tab")[iy_row, nms =="Land:median"]))
  ucl <- check_equal(sapply(FC, function(x) attr(x, "tab")[iy_row, nms =="Discard:median"]))
  rng <- function(x) paste0(range(x), collapse = "-")
  data.frame(
    Variable =c(paste0("F4-8(", intermyr, ")"),
                paste0("SSB(", intermyr, ")"),
                paste0("Rage1(", rng(intermyr:(intermyr + 2)), ")"),
                paste0("Total catch(", intermyr, ")"),
                paste0("Wanted catch(", intermyr, ")"),
                paste0("Unwanted catch(", intermyr, ")")
    ),
    Value =   c(rf(F_int),
                round(ssblast, digits = digits),
                round(rec, digits = digits),
                round(catchlast, digits = digits),
                round(wcl, digits = digits),
                round(ucl, digits = digits)
    ),
    Notes = c(
      paste0("Fsq = F4–8 (", intermyr - 1, ")."),
      "Tonnes; SSB at the middle of the year (i.e. spawning time) before the TAC year.",
      paste0("Resampled recruitment from the years ", rng(Ry), "(geometric mean shown); thousands."),
      "Short-term forecast. In tonnes.",
      paste0("Based on wanted catch fraction (average ", rng(Ay), "). In tonnes."),
      paste0("Based on unwanted catch fraction (average ", rng(Ay), "). In tonnes.")
    )
  )
}

# forecast keeping Numbers at age for ACOM plot
forecast_with_natage <- function (fit, fscale = NULL, catchval = NULL, catchval.exact = NULL, 
                                  fval = NULL, nextssb = NULL, landval = NULL, cwF = NULL, 
                                  nosim = 1000, year.base = max(fit$data$years), ave.years = max(fit$data$years) + 
                                    (-4:0), rec.years = max(fit$data$years) + (-9:0), label = NULL, 
                                  overwriteSelYears = NULL, deterministic = FALSE, processNoiseF = TRUE, 
                                  customWeights = NULL, customSel = NULL, lagR = FALSE, splitLD = FALSE, 
                                  addTSB = FALSE, useSWmodel = (fit$conf$stockWeightModel >= 
                                                                  1), useCWmodel = (fit$conf$catchWeightModel >= 1), useMOmodel = (fit$conf$matureModel >= 
                                                                                                                                     1), useNMmodel = (fit$conf$mortalityModel >= 1), savesim = FALSE, 
                                  cf.cv.keep.cv = NULL, cf.cv.keep.fv = NULL, cf.keep.fv.offset = NULL, 
                                  estimate = median) 
{
  forecast_args <- c(mget(ls(environment(), sorted = F)), 
                     match.call(expand.dots = F)$...)
  forecast_args <- forecast_args[names(forecast_args) != "fit"]
  attr(forecast_args, "RNG_state") <- mget(".Random.seed", 
                                           envir = .GlobalEnv, ifnotfound = list(NULL))[[1]]
  attr(forecast_args, "RNG_kind") <- "RNG_kind"
  dp1 <- function(expr, collapse = " ", width.cutoff = 500L, 
                  ...) paste(deparse(expr, width.cutoff, ...), collapse = collapse)
  estimateLabel <- dp1(substitute(estimate))
  attributes(forecast_args$estimate) <- list(estimateLabel = estimateLabel)
  idxN <- 1:nrow(fit$rep$nvar)
  idxF <- 1:nrow(fit$rep$fvar) + nrow(fit$rep$nvar)
  catchFleets <- which(fit$data$fleetType == 0)
  noCatchFleets <- length(catchFleets)
  idxFbyFleet <- lapply(catchFleets, function(i) {
    x <- fit$conf$keyLogFsta[i, , drop = FALSE]
    unique(x[x > -0.5]) + nrow(fit$rep$nvar) + 1
  })
  resample <- function(x, ...) {
    if (deterministic) {
      ret <- mean(x)
    }
    else {
      ret <- x[sample.int(length(x), ...)]
    }
    return(ret)
  }
  ns <- max(length(fscale), length(catchval), length(catchval.exact), 
            length(fval), length(nextssb), length(cwF))
  if (missing(fscale) & missing(fval) & missing(catchval) & 
      missing(catchval.exact) & missing(nextssb) & missing(cwF)) 
    stop("No scenario is specified")
  if (missing(fscale)) 
    fscale <- rep(NA, ns)
  if (missing(fval)) 
    fval <- rep(NA, ns)
  if (missing(catchval)) 
    catchval <- rep(NA, ns)
  if (missing(catchval.exact)) 
    catchval.exact <- rep(NA, ns)
  if (missing(nextssb)) 
    nextssb <- rep(NA, ns)
  if (missing(landval)) 
    landval <- rep(NA, ns)
  if (missing(cwF)) 
    cwF <- rep(NA, ns)
  if (missing(cf.cv.keep.cv)) 
    cf.cv.keep.cv = matrix(NA, ncol = 2 * sum(fit$data$fleetTypes == 
                                                0), nrow = ns)
  if (missing(cf.cv.keep.fv)) 
    cf.cv.keep.fv = matrix(NA, ncol = 2 * sum(fit$data$fleetTypes == 
                                                0), nrow = ns)
  if (missing(cf.keep.fv.offset)) 
    cf.keep.fv.offset = matrix(0, ncol = sum(fit$data$fleetTypes == 
                                               0), nrow = ns)
  if (!all(rowSums(!is.na(cbind(fscale, catchval, catchval.exact, 
                                fval, nextssb, landval, cwF))) == 1)) {
    stop("For each forecast year exactly one of fscale, catchval or fval must be specified (all others must be set to NA)")
  }
  if (!is.null(overwriteSelYears) & (noCatchFleets != 1)) 
    stop("overwriteSelYears option can only be used for single fleet models")
  if (!is.null(overwriteSelYears)) {
    fromto <- fit$conf$fbarRange - (fit$conf$minAge - 1)
    Ftab <- faytable(fit)
    fixedsel <- colMeans(Ftab[as.integer(rownames(Ftab)) %in% 
                                overwriteSelYears, , drop = FALSE])
    fixedsel <- fixedsel/mean(fixedsel[fromto[1]:fromto[2]])
  }
  if (!is.null(customSel) & (noCatchFleets != 1)) 
    stop("customSel can only be used for single fleet models")
  if (!is.null(customSel)) {
    fromto <- fit$conf$fbarRange - (fit$conf$minAge - 1)
    customSel <- customSel/mean(customSel[fromto[1]:fromto[2]])
  }
  odat <- fit$obj$env$data
  opar <- fit$obj$env$parList(par = fit$obj$env$last.par.best)
  omap <- fit$obj$env$map
  omap[names(which(lapply(opar, length) == 0))] <- NULL
  if (useSWmodel & (fit$conf$stockWeightModel == 0)) {
    stop("stockWeightModel cannot be used for forecast when it was not part of the fitted model")
  }
  if (useSWmodel) {
    rnSW <- 1:(nrow(opar$logSW) + ns) + as.integer(rownames(odat$stockMeanWeight)[1]) - 
      1
    opar$logSW <- matrix(0, nrow = nrow(opar$logSW) + ns, 
                         ncol = ncol(opar$logSW))
    oran <- unique(names(fit$obj$env$par[fit$obj$env$random]))
    objsw <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", 
                       map = omap)
    sdrep <- sdreport(objsw, par.fixed = fit$opt$par, ignore.parm.uncertainty = TRUE)
    idx <- names(sdrep$value) == "logSW"
    simLogSw <- rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx, 
                                                           idx])
  }
  if (useCWmodel & (fit$conf$catchWeightModel == 0)) {
    stop("catchWeightModel cannot be used for forecast when it was not part of the fitted model")
  }
  if (useCWmodel) {
    rnCW <- 1:(nrow(opar$logCW) + ns) + as.integer(rownames(odat$catchMeanWeight)[1]) - 
      1
    opar$logCW <- array(0, dim = dim(opar$logCW) + c(ns, 
                                                     0, 0))
    oran <- unique(names(fit$obj$env$par[fit$obj$env$random]))
    objcw <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", 
                       map = omap)
    objcw$fn(fit$opt$par)
    sdrep <- sdreport(objcw, par.fixed = fit$opt$par, ignore.parm.uncertainty = TRUE, 
    )
    idx <- names(sdrep$value) == "logCW"
    simLogCW <- rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx, 
                                                           idx])
  }
  if (useNMmodel & (fit$conf$mortalityModel == 0)) {
    stop("mortalityModel cannot be used for forecast when it was not part of the fitted model")
  }
  if (useNMmodel) {
    rnNM <- 1:(nrow(opar$logNM) + ns) + as.integer(rownames(odat$natMor)[1]) - 
      1
    opar$logNM <- matrix(0, nrow = nrow(opar$logNM) + ns, 
                         ncol = ncol(opar$logNM))
    oran <- unique(names(fit$obj$env$par[fit$obj$env$random]))
    objnm <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", 
                       map = omap)
    sdrep <- sdreport(objnm, par.fixed = fit$opt$par, ignore.parm.uncertainty = TRUE)
    idx <- names(sdrep$value) == "logNM"
    simLogNM <- rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx, 
                                                           idx])
  }
  if (useMOmodel & (fit$conf$matureModel == 0)) {
    stop("matureModel cannot be used for forecast when it was not part of the fitted model")
  }
  if (useMOmodel) {
    rnMO <- 1:(nrow(opar$logitMO) + ns) + as.integer(rownames(odat$propMat)[1]) - 
      1
    opar$logitMO <- matrix(0, nrow = nrow(opar$logitMO) + 
                             ns, ncol = ncol(opar$logitMO))
    oran <- unique(names(fit$obj$env$par[fit$obj$env$random]))
    objmo <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", 
                       map = omap)
    sdrep <- sdreport(objmo, par.fixed = fit$opt$par, ignore.parm.uncertainty = TRUE)
    idx <- names(sdrep$value) == "logitMO"
    simLogitMO <- rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx, 
                                                             idx])
  }
  getF <- function(x, fleet = which(fit$data$fleetTypes == 
                                      0), allowSelOverwrite = FALSE) {
    getfleet <- function(f) {
      idx <- fit$conf$keyLogFsta[f, ] + 2
      ret <- c(NA, exp(x[idxF]))[idx]
      ret[is.na(ret)] <- 0
      ret
    }
    FF <- lapply(fleet, getfleet)
    ret <- Reduce("+", FF)
    if (allowSelOverwrite) {
      if (!is.null(overwriteSelYears)) {
        fromto <- fit$conf$fbarRange - (fit$conf$minAge - 
                                          1)
        thisfbar <- mean(ret[fromto[1]:fromto[2]])
        ret <- fixedsel * thisfbar
      }
      if (!is.null(customSel)) {
        fromto <- fit$conf$fbarRange - (fit$conf$minAge - 
                                          1)
        thisfbar <- mean(ret[fromto[1]:fromto[2]])
        ret <- customSel * thisfbar
      }
      FF[] <- lapply(fleet, function(x) ret)
    }
    attr(ret, "byFleet") <- do.call(cbind, FF)
    ret
  }
  fbar <- function(x) {
    fromto <- fit$conf$fbarRange - (fit$conf$minAge - 1)
    mean(getF(x)[fromto[1]:fromto[2]])
  }
  fbarFrac <- function(x, lf) {
    fromto <- fit$conf$fbarRange - (fit$conf$minAge - 1)
    mean((lf * getF(x))[fromto[1]:fromto[2]])
  }
  getCWF <- function(x, w) {
    sum(getF(x) * w)
  }
  fbarbyfleet <- function(x) {
    fromto <- fit$conf$fbarRange - (fit$conf$minAge - 1)
    f <- function(fleet) mean(getF(x, fleet)[fromto[1]:fromto[2]])
    sapply(which(fit$data$fleetTypes == 0), f)
  }
  getN <- function(x) {
    idx <- fit$conf$keyLogFsta[1, ] + 1
    nsize <- length(idx)
    ret <- exp(x[1:nsize])
    ret
  }
  getState <- function(N, F) {
    fbf <- fbf <- attr(getF(est), "byFleet")
    ka <- which(fit$conf$keyLogFsta > (-1), TRUE)
    Findx <- apply(ka, 1, function(j) fit$conf$keyLogFsta[j[1], 
                                                          j[2]])
    F <- apply(ka, 1, function(j) fbf[j[2], j[1]])
    F_unique_ordered <- unname(unlist(lapply(split(F, Findx), 
                                             unique)))
    names(F_unique_ordered) <- rep("lastLogF", length(F_unique_ordered))
    x <- log(c(N, F_unique_ordered))
    x
  }
  getProcessVar <- function(fit, set.rec.var.zero = TRUE) {
    varList <- fit$rep[c("nvar", "fvar")]
    if (processNoiseF == FALSE) {
      varList$fvar <- varList$fvar * 0
    }
    cov <- bdiag(varList)
    if (set.rec.var.zero) {
      cov[1, 1] <- 0
    }
    as.matrix(cov)
  }
  step <- function(x, nm, recpool, scale, inyear = FALSE) {
    F <- getF(x, allowSelOverwrite = !inyear)
    N <- getN(x)
    if (!inyear) {
      Z <- F + nm
      n <- length(N)
      N <- c(resample(recpool, 1), N[-n] * exp(-Z[-n]) + 
               c(rep(0, n - 2), N[n] * exp(-Z[n])))
      if (fit$conf$logNMeanAssumption[2] == 1) {
        N <- N + c(0, -0.5 * exp(2 * fit$pl$logSdLogN[conf$keyVarLogN[-1]]))
      }
      else if (fit$conf$logNMeanAssumption[2] == 2) {
        N <- N + c(0, exp(2 * fit$pl$logSdLogN[conf$keyVarLogN[-1]]))
      }
    }
    xx <- rep(NA, length = length(x))
    xx[idxN] <- log(N)
    if ((noCatchFleets == 1)) {
      kk <- fit$conf$keyLogFsta[1, ] + 1
      xx[idxF] <- log(tapply(F[kk > 0], kk[kk > 0], mean))[idxF - 
                                                             min(idxF) + 1] + log(scale)
    }
    else {
      xx[idxF] <- x[idxF] + log(scale)
    }
    return(xx)
  }
  scaleF <- function(x, scale) {
    x[idxF] <- x[idxF] + log(scale)
    return(x)
  }
  scaleFbyFleet <- function(x, scale) {
    f <- function(i) {
      x[idxFbyFleet[[i]]] <<- x[idxFbyFleet[[i]]] + log(scale[i])
    }
    invisible(sapply(1:noCatchFleets, f))
    return(x)
  }
  sel <- function(x) {
    getF(x)/fbar(x)
  }
  catch <- function(x, nm, cw) {
    F <- getF(x)
    FF <- attr(F, "byFleet")
    Z <- F + nm
    N <- getN(x)
    C <- FF/Z * (1 - exp(-Z)) * N
    TCW <- sum(C * cw)
    attr(TCW, "byFleet") <- colSums(C * cw)
    return(TCW)
  }
  catchFrac <- function(x, nm, w, frac) {
    F <- getF(x)
    Z <- F + nm
    N <- getN(x)
    C <- F/Z * (1 - exp(-Z)) * N
    return(sum(frac * w * C))
  }
  catchatage <- function(x, nm) {
    F <- getF(x)
    Z <- F + nm
    N <- getN(x)
    C <- F/Z * (1 - exp(-Z)) * N
    return(C)
  }
  ssb <- function(x, nm, sw, mo, pm, pf) {
    F <- getF(x)
    FF <- attr(F, "byFleet")
    ZZ <- pm * nm + rowSums(pf * FF)
    N <- getN(x) * exp(-ZZ)
    return(sum(N * mo * sw))
  }
  tsb <- function(x, sw) {
    F <- getF(x)
    N <- getN(x)
    return(sum(N * sw))
  }
  rectab <- rectable(fit)
  recpool <- rectab[rownames(rectab) %in% rec.years, 1]
  if (year.base == max(fit$data$years)) {
    est <- fit$sdrep$estY
    cov <- fit$sdrep$covY
  }
  if (year.base == (max(fit$data$years) - 1)) {
    est <- fit$sdrep$estYm1
    cov <- fit$sdrep$covYm1
  }
  if (year.base < (max(fit$data$years) - 1)) {
    stop("State not saved, so cannot proceed from this year")
  }
  if (deterministic) 
    cov <- cov * 0
  sim <- rmvnorm(nosim, mu = est, Sigma = cov)
  if (is.null(overwriteSelYears) & is.null(customSel)) 
    if (!isTRUE(all.equal(unname(est), unname(getState(getN(est), 
                                                       getF(est)))))) 
      stop("Sorry somthing is wrong here (check code for getN, getF, and getState)")
  doAve <- function(x) {
    if (length(dim(x)) == 2) {
      ret <- colMeans(x[rownames(x) %in% ave.years, , 
                        drop = FALSE], na.rm = TRUE)
    }
    if (length(dim(x)) == 3) {
      ret <- apply(x[rownames(x) %in% ave.years, , , drop = FALSE], 
                   c(2, 3), mean, na.rm = TRUE)
    }
    ret
  }
  ave.sw <- doAve(fit$data$stockMeanWeight)
  ave.cw <- doAve(fit$data$catchMeanWeight)
  ave.mo <- doAve(fit$data$propMat)
  ave.nm <- doAve(fit$data$natMor)
  ave.lf <- doAve(fit$data$landFrac)
  ave.lw <- doAve(fit$data$landMeanWeight)
  ave.pm <- doAve(fit$data$propM)
  ave.pf <- doAve(fit$data$propF)
  getThisOrAve <- function(x, y, ave) {
    if (y %in% rownames(x)) {
      if (length(dim(x)) == 2) {
        ret <- x[which(rownames(x) == y), ]
      }
      else {
        ret <- x[which(rownames(x) == y), , ]
      }
      ret[is.na(ret)] <- ave[is.na(ret)]
    }
    else {
      ret <- ave
    }
    ret
  }
  procVar <- getProcessVar(fit)
  simlist <- list()
  rs <- NULL
  for (i in 0:(length(fscale) - 1)) {
    y <- year.base + i
    if (!is.null(rs)) {
      assign(".Random.seed", rs, envir = .GlobalEnv)
      rs <- NULL
    }
    sw <- getThisOrAve(fit$data$stockMeanWeight, y, ave.sw)
    cw <- getThisOrAve(fit$data$catchMeanWeight, y, ave.cw)
    mo <- getThisOrAve(fit$data$propMat, y, ave.mo)
    nm <- getThisOrAve(fit$data$natMor, y, ave.nm)
    lf <- getThisOrAve(fit$data$landFrac, y, ave.lf)
    lw <- getThisOrAve(fit$data$landMeanWeight, y, ave.lw)
    pm <- getThisOrAve(fit$data$propM, y, ave.pm)
    pf <- getThisOrAve(fit$data$propF, y, ave.pf)
    if (useNMmodel) {
      sim <- t(sapply(1:nrow(sim), function(s) {
        yidx <- which(rnNM == as.integer(y))
        thisnm <- matrix(exp(simLogNM[s, ]), nrow = nrow(opar$logNM))[yidx, 
        ]
        step(sim[s, ], nm = thisnm, recpool = recpool, 
             scale = 1, inyear = (i == 0))
      }))
    }
    else {
      sim <- t(apply(sim, 1, function(s) step(s, nm = nm, 
                                              recpool = recpool, scale = 1, inyear = (i == 
                                                                                        0))))
    }
    if (i != 0) {
      cof <- coef(fit)
      if (deterministic) {
        procVar <- procVar * 0
      }
      if (!is.null(overwriteSelYears)) {
        nn <- length(fit$conf$keyLogFsta[1, ])
        procVar[-c(1:nn), -c(1:nn)] <- 0
      }
      if (!is.null(customSel)) {
        nn <- length(fit$conf$keyLogFsta[1, ])
        procVar[-c(1:nn), -c(1:nn)] <- 0
      }
      if (all(fit$conf$corFlag < 3)) {
        sim <- sim + rmvnorm(nosim, mu = rep(0, nrow(procVar)), 
                             Sigma = procVar)
      }
      else if (length(fit$conf$corFlag) == 1 && fit$conf$corFlag[1] == 
               3) {
        k <- fit$conf$keyLogFsta[1, ]
        k <- unique(k[k >= 0] + 1)
        nF <- length(k)
        simTmp <- rmvnorm(nosim, mu = rep(0, nrow(procVar)), 
                          Sigma = procVar)
        rhoF = 2/(1 + exp(-2 * cof[names(cof) == "sepFlogitRho"])) - 
          1
        sepFalpha = cof[names(cof) == "sepFalpha"]
        for (jj in 1:nosim) {
          V = mean(sim[jj, (dim(sim)[2] - nF + 1):(dim(sim)[2])])
          U = sim[jj, (dim(sim)[2] - nF + 1):(dim(sim)[2] - 
                                                1)] - V
          Uepsilon = simTmp[jj, (dim(simTmp)[2] - nF + 
                                   1):(dim(simTmp)[2] - 1)]
          Vepsilon = simTmp[jj, dim(simTmp)[2]]
          Unew = rhoF[1] * U + Uepsilon + sepFalpha[1:(length(sepFalpha) - 
                                                         1)]
          Vnew = rhoF[2] * V + Vepsilon + sepFalpha[length(sepFalpha)]
          sim[jj, (dim(simTmp)[2] - nF + 1):(dim(simTmp)[2] - 
                                               1)] = Unew + Vnew
          sim[jj, dim(simTmp)[2]] = -sum(Unew) + Vnew
          sim[jj, 1:(dim(simTmp)[2] - nF)] = sim[jj, 
                                                 1:(dim(simTmp)[2] - nF)] + simTmp[jj, 1:(dim(simTmp)[2] - 
                                                                                            nF)]
        }
      }
      else {
        stop("forcast not implemented for the given corflag")
      }
    }
    if (!is.na(fscale[i + 1])) {
      sim <- t(apply(sim, 1, scaleF, scale = fscale[i + 
                                                      1]))
    }
    if (!is.na(fval[i + 1])) {
      curfbar <- estimate(apply(sim, 1, fbar))
      adj <- fval[i + 1]/curfbar
      sim <- t(apply(sim, 1, scaleF, scale = adj))
    }
    if (!is.na(cwF[i + 1])) {
      if (missing(customWeights)) 
        stop("customWeights must be supplied when using the cwF option")
      curcwF <- estimate(apply(sim, 1, getCWF, w = customWeights))
      adj <- cwF[i + 1]/curcwF
      sim <- t(apply(sim, 1, scaleF, scale = adj))
    }
    if (!is.na(catchval[i + 1])) {
      simtmp <- NA
      fun <- function(s) {
        simtmp <<- t(apply(sim, 1, scaleF, scale = s))
        if (useCWmodel | useNMmodel) {
          simcat <- sapply(1:nrow(simtmp), function(k) {
            if (useCWmodel) {
              thisy <- which(rnCW == y)
              thiscw <- matrix(exp(simLogCW[k, ]), nrow = nrow(opar$logCW))[thisy, 
              ]
            }
            else {
              thiscw <- cw
            }
            if (useNMmodel) {
              thisy <- which(rnNM == y)
              thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
              ]
            }
            else {
              thisnm <- nm
            }
            catch(simtmp[k, ], nm = thisnm, cw = thiscw)
          })
        }
        else {
          simcat <- apply(simtmp, 1, catch, nm = nm, 
                          cw = cw)
        }
        return(catchval[i + 1] - estimate(simcat))
      }
      ff <- uniroot(fun, c(0, 100))$root
      sim <- simtmp
    }
    if (!is.na(catchval.exact[i + 1])) {
      simtmp <- sim
      funk <- function(k) {
        if (useCWmodel) {
          thisy <- which(rnCW == y)
          thiscw <- matrix(exp(simLogCW[k, ]), nrow = nrow(opar$logCW))[thisy, 
          ]
        }
        else {
          thiscw = cw
        }
        if (useNMmodel) {
          thisy <- which(rnNM == y)
          thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
          ]
        }
        else {
          thisnm = nm
        }
        one <- function(s) {
          simtmp[k, ] <<- scaleF(sim[k, ], s)
          simcat <- catch(simtmp[k, ], nm = thisnm, 
                          cw = thiscw)
          return(catchval.exact[i + 1] - simcat)
        }
        ff <- uniroot(one, c(0, 100))$root
      }
      dd <- sapply(1:nrow(sim), funk)
      sim <- simtmp
    }
    if (!is.na(landval[i + 1])) {
      simtmp <- NA
      fun <- function(s) {
        simtmp <<- t(apply(sim, 1, scaleF, scale = s))
        if (useNMmodel) {
          simcat <- sapply(1:nrow(simtmp), function(k) {
            thisy <- which(rnNM == y)
            thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
            ]
            catchFrac(simtmp[k, ], nm = thisnm, w = lw, 
                      frac = lf)
          })
        }
        else {
          simcat <- apply(simtmp, 1, catchFrac, nm = nm, 
                          w = lw, frac = lf)
        }
        return(landval[i + 1] - estimate(simcat))
      }
      ff <- uniroot(fun, c(0, 100))$root
      sim <- simtmp
    }
    if (!is.na(nextssb[i + 1])) {
      if ((sum(pm) != 0) | (sum(pf) != 0)) 
        stop("nextssb option only available if SSB is calculated in the beginning of the year")
      simtmp <- NA
      rs <- .Random.seed
      fun <- function(s) {
        assign(".Random.seed", rs, envir = .GlobalEnv)
        simtmp <<- t(apply(sim, 1, scaleF, scale = s))
        if (useNMmodel) {
          simsim <- t(sapply(1:nrow(simtmp), function(s) {
            yidx <- which(rnNM == as.integer(y))
            thisnm <- matrix(exp(simLogNM[s, ]), nrow = nrow(opar$logNM))[yidx, 
            ]
            step(simtmp[s, ], nm = thisnm, recpool = recpool, 
                 scale = 1, inyear = (i == 0))
          }))
        }
        else {
          simsim <- t(apply(simtmp, 1, function(s) step(s, 
                                                        nm = nm, recpool = recpool, scale = 1, inyear = (i == 
                                                                                                           0))))
        }
        if (i != 0) {
          if (deterministic) 
            procVar <- procVar * 0
          simsim <- simsim + rmvnorm(nosim, mu = rep(0, 
                                                     nrow(procVar)), Sigma = procVar)
        }
        if (useSWmodel | useMOmodel | useNMmodel) {
          ssbswmoi <- function(k) {
            if (useSWmodel) {
              thisy <- which(rnSW == y)
              thissw <- matrix(exp(simLogSw[k, ]), nrow = nrow(opar$logSW))[thisy, 
              ]
            }
            else {
              thissw <- sw
            }
            if (useMOmodel) {
              thisy <- which(rnMO == y)
              thismo <- matrix(plogis(simLogitMO[k, 
              ]), nrow = nrow(opar$logitMO))[thisy, 
              ]
            }
            else {
              thismo <- mo
            }
            if (useNMmodel) {
              thisy <- which(rnNM == y)
              thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
              ]
            }
            else {
              thisnm <- nm
            }
            ssb(simsim[k, ], nm = thisnm, sw = thissw, 
                mo = thismo, pm = pm, pf = pf)
          }
          simnextssb <- sapply(1:nrow(simsim), ssbswmoi)
        }
        else {
          simnextssb <- apply(simsim, 1, ssb, nm = nm, 
                              sw = sw, mo = mo, pm = pm, pf = pf)
        }
        return(nextssb[i + 1] - estimate(simnextssb))
      }
      ff <- uniroot(fun, c(0, 100))$root
      sim <- simtmp
    }
    if (any(!is.na(cf.cv.keep.cv[i + 1, ]))) {
      cfcv <- cf.cv.keep.cv[i + 1, ]
      cf <- cfcv[1:noCatchFleets]
      cv <- cfcv[1:noCatchFleets + noCatchFleets]
      cfcvtcv <- c(cfcv, catchval[i + 1])
      ii <- which(apply(rbind(cv, cf), 2, function(x) any(!is.na(x))))
      theta <- rep(1, length(ii) + 1)
      if (length(theta) > noCatchFleets) 
        stop("Over-specified in cf.cv.keep.cv")
      lsfun <- function(logth) {
        th <- exp(logth)
        s <- rep(NA, noCatchFleets)
        s[ii] <- th[1:length(ii)]
        s[-ii] <- th[length(ii) + 1]
        simtmp <<- t(apply(sim, 1, scaleFbyFleet, scale = s))
        cvfun <- function(x) {
          tcv <- catch(x, nm = nm, cw = cw)
          cv <- attr(tcv, "byFleet")
          return(cv)
        }
        simcat <- apply(simtmp, 1, cvfun)
        medcv <- apply(simcat, 1, estimate)
        med <- c(medcv/sum(medcv), medcv, estimate(apply(simcat, 
                                                         2, sum)))
        return(-sum(dnorm(log(cfcvtcv), log(med), sd = 1, 
                          log = TRUE), na.rm = TRUE))
      }
      ff <- nlminb(log(theta), lsfun)
      sim <- simtmp
    }
    if (any(!is.na(cf.cv.keep.fv[i + 1, ]))) {
      cfcv <- cf.cv.keep.fv[i + 1, ]
      cf <- cfcv[1:noCatchFleets]
      cv <- cfcv[1:noCatchFleets + noCatchFleets]
      cfcvtfv <- c(cfcv, fval[i + 1])
      ii <- which(apply(rbind(cv, cf), 2, function(x) any(!is.na(x))))
      theta <- rep(1, length(ii) + 1)
      if (length(theta) > noCatchFleets) 
        stop("Over-specified in cf.cv.keep.fv")
      lsfun <- function(th) {
        s <- rep(NA, noCatchFleets)
        s[ii] <- th[1:length(ii)]
        s[-ii] <- th[length(ii) + 1]
        simtmp <<- t(apply(sim, 1, scaleFbyFleet, scale = s))
        cvfun <- function(x) {
          tcv <- catch(x, nm = nm, cw = cw)
          cv <- attr(tcv, "byFleet")
          return(cv)
        }
        simcat <- apply(simtmp, 1, cvfun)
        simfbar <- apply(simtmp, 1, fbar)
        medcv <- apply(simcat, 1, estimate)
        med <- c((medcv - cf.keep.fv.offset[i + 1, ])/sum(medcv), 
                 medcv, estimate(simfbar))
        return(sum(((cfcvtfv - med)/cfcvtfv)^2, na.rm = TRUE))
      }
      ff <- nlminb(theta, lsfun, lower = 0.001, upper = 1000)
      sim <- simtmp
    }
    fbarsim <- apply(sim, 1, fbar)
    fbarLsim <- apply(sim, 1, fbarFrac, lf = lf)
    if (useCWmodel | useNMmodel) {
      catchcwi <- function(k) {
        if (useCWmodel) {
          thisy <- which(rnCW == y)
          thiscw <- matrix(exp(simLogCW[k, ]), nrow = nrow(opar$logCW))[thisy, 
          ]
        }
        else {
          thiscw <- cw
        }
        if (useNMmodel) {
          thisy <- which(rnNM == y)
          thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
          ]
        }
        else {
          thisnm <- nm
        }
        catch(sim[k, ], nm = thisnm, cw = thiscw)
      }
      catchsim <- sapply(1:nrow(sim), catchcwi)
    }
    else {
      catchsim <- apply(sim, 1, catch, nm = nm, cw = cw)
    }
    if (useNMmodel) {
      landsim <- sapply(1:nrow(sim), function(k) {
        thisy <- which(rnNM == y)
        thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
        ]
        catchFrac(sim[k, ], nm = thisnm, w = lw, frac = lf)
      })
    }
    else {
      landsim <- apply(sim, 1, catchFrac, nm = nm, w = lw, 
                       frac = lf)
    }
    catchatagesim <- apply(sim, 1, catchatage, nm = nm)
    natagesim <- apply(sim, 1, getN)
    if (useSWmodel | useMOmodel | useNMmodel) {
      ssbswmoi <- function(k) {
        if (useSWmodel) {
          thisy <- which(rnSW == y)
          thissw <- matrix(exp(simLogSw[k, ]), nrow = nrow(opar$logSW))[thisy, 
          ]
        }
        else {
          thissw <- sw
        }
        if (useMOmodel) {
          thisy <- which(rnMO == y)
          thismo <- matrix(plogis(simLogitMO[k, ]), 
                           nrow = nrow(opar$logitMO))[thisy, ]
        }
        else {
          thismo <- mo
        }
        if (useNMmodel) {
          thisy <- which(rnNM == y)
          thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
          ]
        }
        else {
          thisnm <- nm
        }
        ssb(sim[k, ], nm = thisnm, sw = thissw, mo = thismo, 
            pm = pm, pf = pf)
      }
      ssbsim <- sapply(1:nrow(sim), ssbswmoi)
    }
    else {
      ssbsim <- apply(sim, 1, ssb, nm = nm, sw = sw, mo = mo, 
                      pm = pm, pf = pf)
    }
    tsbsim <- apply(sim, 1, tsb, sw = sw)
    if (lagR) {
      recsim <- exp(sim[, 2])
    }
    else {
      recsim <- exp(sim[, 1])
    }
    cwFsim <- rep(NA, nrow(sim))
    if (!missing(customWeights)) {
      cwFsim <- apply(sim, 1, getCWF, w = customWeights)
    }
    fbarbyfleetsim <- apply(sim, 1, fbarbyfleet)
    catchbysim <- apply(sim, 1, function(x) attr(catch(x, 
                                                       nm = nm, cw = cw), "byFleet"))
    simlist[[i + 1]] <- list(sim = sim, fbar = fbarsim, 
                             fbarbyfleet = fbarbyfleetsim, catch = catchsim, 
                             ssb = ssbsim, rec = recsim, cwF = cwFsim, catchatage = catchatagesim, 
                             natage=natagesim,
                             land = landsim, fbarL = fbarLsim, tsb = tsbsim, 
                             catchby = catchbysim, year = y)
  }
  attr(simlist, "fit") <- fit
  collect <- function(x) {
    quan <- quantile(x, c(0.5, 0.025, 0.975))
    c(Estimate = estimate(x), low = quan[2], high = quan[3])
  }
  fbar <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$fbar))), 
                3)
  fbarL <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$fbarL))), 
                 3)
  rec <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$rec))))
  ssb <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$ssb))))
  tsb <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$tsb))))
  catch <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$catch))))
  land <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$land))))
  caytable <- round(do.call(rbind, lapply(simlist, function(xx) apply(xx$catchatage, 
                                                                      1, collect))))
  naytable <- round(do.call(rbind, lapply(simlist, function(xx) apply(xx$natage, 
                                                                      1, collect))))
  tab <- cbind(fbar, rec, ssb, catch)
  if (splitLD) {
    tab <- cbind(tab, fbarL, fbar - fbarL, land, catch - 
                   land)
  }
  if (addTSB) {
    tab <- cbind(tab, tsb)
  }
  if (!missing(customWeights)) 
    tab <- cbind(tab, cwF = round(do.call(rbind, lapply(simlist, 
                                                        function(xx) collect(xx$cwF))), 3))
  rownames(tab) <- unlist(lapply(simlist, function(xx) xx$year))
  nam <- c(estimateLabel, "low", "high")
  basename <- c("fbar:", "rec:", "ssb:", "catch:")
  if (splitLD) {
    basename <- c(basename, "fbarL:", "fbarD:", "Land:", 
                  "Discard:")
  }
  if (addTSB) {
    basename <- c(basename, "tsb:")
  }
  if (!missing(customWeights)) {
    basename <- c(basename, "cwF:")
  }
  colnames(tab) <- paste0(rep(basename, each = length(nam)), 
                          nam)
  attr(simlist, "tab") <- tab
  shorttab <- t(tab[, grep(estimateLabel, colnames(tab))])
  rownames(shorttab) <- sub(paste0(":", estimateLabel), "", 
                            paste0(label, if (!is.null(label)) 
                              ":", rownames(shorttab)))
  attr(simlist, "shorttab") <- shorttab
  attr(simlist, "label") <- label
  attr(simlist, "caytable") <- caytable
  attr(simlist, "naytable") <- naytable
  if (sum(fit$data$fleetTypes == 0) == 1) {
    catchby <- catch
  }
  else {
    catchby <- round(do.call(rbind, lapply(simlist, function(xx) as.vector(apply(xx$catchby, 
                                                                                 1, collect)))))
  }
  if (sum(fit$data$fleetTypes == 0) == 1) {
    fbarby <- fbar
  }
  else {
    fbarby <- round(do.call(rbind, lapply(simlist, function(xx) as.vector(apply(xx$fbarbyfleet, 
                                                                                1, collect)))), 3)
  }
  rownames(catchby) <- rownames(tab)
  rownames(fbarby) <- rownames(tab)
  nam <- c(estimateLabel, "low", "high")
  colnames(catchby) <- paste0(rep(paste0("F", 1:sum(fit$data$fleetTypes == 
                                                      0), ":"), each = length(nam)), nam)
  colnames(fbarby) <- paste0(rep(paste0("F", 1:sum(fit$data$fleetTypes == 
                                                     0), ":"), each = length(nam)), nam)
  attr(simlist, "catchby") <- catchby
  attr(simlist, "fbarby") <- fbarby
  class(simlist) <- "samforecast"
  attr(simlist, "estimateLabel") <- estimateLabel
  attr(simlist, "forecast_arguments") <- forecast_args
  if (!savesim) {
    simlistsmall <- lapply(simlist, function(x) list(year = x$year))
    attributes(simlistsmall) <- attributes(simlist)
    return(simlistsmall)
  }
  else {
    return(simlist)
  }
}


#--------------------------------------------------------------------------------
# forecast keeping Numbers at age for ACOM plot -- older version stockassessment package
forecast_with_natage_old <- function (fit, fscale = NULL, catchval = NULL, catchval.exact = NULL, 
                                  fval = NULL, nextssb = NULL, landval = NULL, cwF = NULL, 
                                  nosim = 1000, year.base = max(fit$data$years), ave.years = max(fit$data$years) + 
                                    (-4:0), rec.years = max(fit$data$years) + (-9:0), label = NULL, 
                                  overwriteSelYears = NULL, deterministic = FALSE, processNoiseF = TRUE, 
                                  customWeights = NULL, customSel = NULL, lagR = FALSE, splitLD = FALSE, 
                                  addTSB = FALSE, useSWmodel = (fit$conf$stockWeightModel == 
                                                                  1), useCWmodel = (fit$conf$catchWeightModel == 1), useMOmodel = (fit$conf$matureModel == 
                                                                                                                                     1), useNMmodel = (fit$conf$mortalityModel == 1), savesim = FALSE) 
{
  resample <- function(x, ...) {
    if (deterministic) {
      ret <- mean(x)
    }
    else {
      ret <- x[sample.int(length(x), ...)]
    }
    return(ret)
  }
  ns <- max(length(fscale), length(catchval), length(catchval.exact), 
            length(fval), length(nextssb), length(cwF))
  if (missing(fscale) & missing(fval) & missing(catchval) & 
      missing(catchval.exact) & missing(nextssb) & missing(cwF)) 
    stop("No scenario is specified")
  if (missing(fscale)) 
    fscale <- rep(NA, ns)
  if (missing(fval)) 
    fval <- rep(NA, ns)
  if (missing(catchval)) 
    catchval <- rep(NA, ns)
  if (missing(catchval.exact)) 
    catchval.exact <- rep(NA, ns)
  if (missing(nextssb)) 
    nextssb <- rep(NA, ns)
  if (missing(landval)) 
    landval <- rep(NA, ns)
  if (missing(cwF)) 
    cwF <- rep(NA, ns)
  if (!all(rowSums(!is.na(cbind(fscale, catchval, catchval.exact, 
                                fval, nextssb, landval, cwF))) == 1)) {
    stop("For each forecast year exactly one of fscale, catchval or fval must be specified (all others must be set to NA)")
  }
  if (!is.null(overwriteSelYears)) {
    fromto <- fit$conf$fbarRange - (fit$conf$minAge - 1)
    Ftab <- faytable(fit)
    fixedsel <- colMeans(Ftab[as.integer(rownames(Ftab)) %in% 
                                overwriteSelYears, , drop = FALSE])
    fixedsel <- fixedsel/mean(fixedsel[fromto[1]:fromto[2]])
  }
  if (!is.null(customSel)) {
    fromto <- fit$conf$fbarRange - (fit$conf$minAge - 1)
    customSel <- customSel/mean(customSel[fromto[1]:fromto[2]])
  }
  odat <- fit$obj$env$data
  opar <- fit$obj$env$parList(par = fit$obj$env$last.par.best)
  omap <- fit$obj$env$map
  omap[names(which(lapply(opar, length) == 0))] <- NULL
  if (useSWmodel & (fit$conf$stockWeightModel == 0)) {
    stop("stockWeightModel cannot be used for forecast when it was not part of the fitted model")
  }
  if (useSWmodel) {
    rnSW <- 1:(nrow(opar$logSW) + ns) + as.integer(rownames(odat$stockMeanWeight)[1]) - 
      1
    opar$logSW <- matrix(0, nrow = nrow(opar$logSW) + ns, 
                         ncol = ncol(opar$logSW))
    oran <- unique(names(fit$obj$env$par[fit$obj$env$random]))
    objsw <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", 
                       map = omap)
    sdrep <- sdreport(objsw, par.fixed = fit$opt$par, ignore.parm.uncertainty = TRUE)
    idx <- names(sdrep$value) == "logSW"
    simLogSw <- rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx, 
                                                           idx])
  }
  if (useCWmodel & (fit$conf$catchWeightModel == 0)) {
    stop("catchWeightModel cannot be used for forecast when it was not part of the fitted model")
  }
  if (useCWmodel) {
    rnCW <- 1:(nrow(opar$logCW) + ns) + as.integer(rownames(odat$catchMeanWeight)[1]) - 
      1
    opar$logCW <- matrix(0, nrow = nrow(opar$logCW) + ns, 
                         ncol = ncol(opar$logCW))
    oran <- unique(names(fit$obj$env$par[fit$obj$env$random]))
    objcw <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", 
                       map = omap)
    sdrep <- sdreport(objcw, par.fixed = fit$opt$par, ignore.parm.uncertainty = TRUE)
    idx <- names(sdrep$value) == "logCW"
    simLogCW <- rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx, 
                                                           idx])
  }
  if (useNMmodel & (fit$conf$mortalityModel == 0)) {
    stop("mortalityModel cannot be used for forecast when it was not part of the fitted model")
  }
  if (useNMmodel) {
    rnNM <- 1:(nrow(opar$logNM) + ns) + as.integer(rownames(odat$natMor)[1]) - 
      1
    opar$logNM <- matrix(0, nrow = nrow(opar$logNM) + ns, 
                         ncol = ncol(opar$logNM))
    oran <- unique(names(fit$obj$env$par[fit$obj$env$random]))
    objnm <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", 
                       map = omap)
    sdrep <- sdreport(objnm, par.fixed = fit$opt$par, ignore.parm.uncertainty = TRUE)
    idx <- names(sdrep$value) == "logNM"
    simLogNM <- rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx, 
                                                           idx])
  }
  if (useMOmodel & (fit$conf$matureModel == 0)) {
    stop("matureModel cannot be used for forecast when it was not part of the fitted model")
  }
  if (useMOmodel) {
    rnMO <- 1:(nrow(opar$logitMO) + ns) + as.integer(rownames(odat$propMat)[1]) - 
      1
    opar$logitMO <- matrix(0, nrow = nrow(opar$logitMO) + 
                             ns, ncol = ncol(opar$logitMO))
    oran <- unique(names(fit$obj$env$par[fit$obj$env$random]))
    objmo <- MakeADFun(odat, opar, random = oran, DLL = "stockassessment", 
                       map = omap)
    sdrep <- sdreport(objmo, par.fixed = fit$opt$par, ignore.parm.uncertainty = TRUE)
    idx <- names(sdrep$value) == "logitMO"
    simLogitMO <- rmvnorm(nosim, sdrep$value[idx], sdrep$cov[idx, 
                                                             idx])
  }
  getF <- function(x, allowSelOverwrite = FALSE) {
    idx <- fit$conf$keyLogFsta[1, ] + 1
    nsize <- length(idx)
    ret <- exp(x[nsize + idx])
    ret[idx == 0] <- 0
    if (allowSelOverwrite) {
      if (!is.null(overwriteSelYears)) {
        fromto <- fit$conf$fbarRange - (fit$conf$minAge - 
                                          1)
        thisfbar <- mean(ret[fromto[1]:fromto[2]])
        ret <- fixedsel * thisfbar
      }
      if (!is.null(customSel)) {
        fromto <- fit$conf$fbarRange - (fit$conf$minAge - 
                                          1)
        thisfbar <- mean(ret[fromto[1]:fromto[2]])
        ret <- customSel * thisfbar
      }
    }
    ret
  }
  fbar <- function(x) {
    fromto <- fit$conf$fbarRange - (fit$conf$minAge - 1)
    mean(getF(x)[fromto[1]:fromto[2]])
  }
  fbarFrac <- function(x, lf) {
    fromto <- fit$conf$fbarRange - (fit$conf$minAge - 1)
    mean((lf * getF(x))[fromto[1]:fromto[2]])
  }
  getCWF <- function(x, w) {
    sum(getF(x) * w)
  }
  getN <- function(x) {
    idx <- fit$conf$keyLogFsta[1, ] + 1
    nsize <- length(idx)
    ret <- exp(x[1:nsize])
    ret
  }
  getState <- function(N, F) {
    k <- fit$conf$keyLogFsta[1, ]
    F <- F[k >= 0]
    k <- !duplicated(k[k >= 0])
    x <- log(c(N, F[k]))
    x
  }
  getProcessVar <- function(fit) {
    cof <- coef(fit)
    sdN <- exp(cof[names(cof) == "logSdLogN"][fit$conf$keyVarLogN + 
                                                1])
    sdN[1] <- 0
    nN <- length(sdN)
    sdF <- exp(cof[names(cof) == "logSdLogFsta"][fit$conf$keyVarF + 
                                                   1])
    if (processNoiseF == FALSE) {
      sdF <- sdF * 0
    }
    k <- fit$conf$keyLogFsta[1, ]
    sdF <- sdF[k >= 0]
    k <- unique(k[k >= 0] + 1)
    sdF <- sdF[k]
    nF <- length(sdF)
    if (fit$conf$corFlag == 0) {
      corr <- diag(nF)
    }
    if (fit$conf$corFlag == 1) {
      y <- cof[names(cof) == "itrans_rho"]
      rho <- 2/(1 + exp(-2 * y)) - 1
      corr <- matrix(rho, nrow = nF, ncol = nF)
      diag(corr) <- 1
    }
    if (fit$conf$corFlag == 2) {
      y <- cof[names(cof) == "itrans_rho"]
      rho <- 2/(1 + exp(-2 * y)) - 1
      corr <- diag(sdF)
      corr <- rho^abs(row(corr) - col(corr))
    }
    cov <- matrix(0, nrow = nN + nF, ncol = nN + nF)
    cov[1:nN, 1:nN] <- diag(sdN^2)
    if (fit$conf$corFlag < 3) {
      cov[nN + 1:nF, nN + 1:nF] <- (sdF %*% t(sdF)) * 
        corr
    }
    else {
      if (fit$conf$corFlag == 3) {
        sdU <- exp(cof[names(cof) == "sepFlogSd"][1])
        sdV <- exp(cof[names(cof) == "sepFlogSd"][2])
        diag(cov[nN + 1:(nF - 1), nN + 1:(nF - 1)]) <- sdU^2
        cov[nN + nF, nN + nF] <- sdV^2
      }
    }
    cov
  }
  step <- function(x, nm, recpool, scale, inyear = FALSE) {
    F <- getF(x, allowSelOverwrite = !inyear)
    N <- getN(x)
    if (!inyear) {
      Z <- F + nm
      n <- length(N)
      N <- c(resample(recpool, 1), N[-n] * exp(-Z[-n]) + 
               c(rep(0, n - 2), N[n] * exp(-Z[n])))
    }
    F <- F * scale
    xx <- getState(N, F)
    return(xx)
  }
  scaleF <- function(x, scale) {
    F <- getF(x) * scale
    N <- getN(x)
    xx <- getState(N, F)
    return(xx)
  }
  sel <- function(x) {
    getF(x)/fbar(x)
  }
  catch <- function(x, nm, cw) {
    F <- getF(x)
    Z <- F + nm
    N <- getN(x)
    C <- F/Z * (1 - exp(-Z)) * N
    return(sum(cw * C))
  }
  catchFrac <- function(x, nm, w, frac) {
    F <- getF(x)
    Z <- F + nm
    N <- getN(x)
    C <- F/Z * (1 - exp(-Z)) * N
    return(sum(frac * w * C))
  }
  catchatage <- function(x, nm) {
    F <- getF(x)
    Z <- F + nm
    N <- getN(x)
    C <- F/Z * (1 - exp(-Z)) * N
    return(C)
  }
  ssb <- function(x, nm, sw, mo, pm, pf) {
    F <- getF(x)
    N <- getN(x) * exp(-pm * nm - pf * F)
    return(sum(N * mo * sw))
  }
  tsb <- function(x, sw) {
    F <- getF(x)
    N <- getN(x)
    return(sum(N * sw))
  }
  rectab <- rectable(fit)
  recpool <- rectab[rownames(rectab) %in% rec.years, 1]
  if (year.base == max(fit$data$years)) {
    est <- fit$sdrep$estY
    cov <- fit$sdrep$covY
  }
  if (year.base == (max(fit$data$years) - 1)) {
    est <- fit$sdrep$estYm1
    cov <- fit$sdrep$covYm1
  }
  if (year.base < (max(fit$data$years) - 1)) {
    stop("State not saved, so cannot proceed from this year")
  }
  if (deterministic) 
    cov <- cov * 0
  sim <- rmvnorm(nosim, mu = est, Sigma = cov)
  if (is.null(overwriteSelYears) & is.null(customSel)) {
    if (!isTRUE(all.equal(est, getState(getN(est), getF(est))))) 
      stop("Sorry somthing is wrong here (check code for getN, getF, and getState)")
  }
  doAve <- function(x, y) colMeans(x[rownames(x) %in% ave.years, 
                                     , drop = FALSE], na.rm = TRUE)
  ave.sw <- doAve(fit$data$stockMeanWeight)
  ave.cw <- doAve(fit$data$catchMeanWeight)
  ave.mo <- doAve(fit$data$propMat)
  ave.nm <- doAve(fit$data$natMor)
  ave.lf <- doAve(fit$data$landFrac)
  ave.lw <- doAve(fit$data$landMeanWeight)
  ave.pm <- doAve(fit$data$propM)
  ave.pf <- doAve(fit$data$propF)
  getThisOrAve <- function(x, y, ave) {
    if ((y %in% rownames(x)) & (!all(is.na(x[which(rownames(x) == 
                                                   y), ])))) {
      ret <- x[which(rownames(x) == y), ]
    }
    else {
      ret <- ave
    }
    ret
  }
  procVar <- getProcessVar(fit)
  simlist <- list()
  rs <- NULL
  for (i in 0:(length(fscale) - 1)) {
    y <- year.base + i
    if (!is.null(rs)) {
      assign(".Random.seed", rs, envir = .GlobalEnv)
      rs <- NULL
    }
    sw <- getThisOrAve(fit$data$stockMeanWeight, y, ave.sw)
    cw <- getThisOrAve(fit$data$catchMeanWeight, y, ave.cw)
    mo <- getThisOrAve(fit$data$propMat, y, ave.mo)
    nm <- getThisOrAve(fit$data$natMor, y, ave.nm)
    lf <- getThisOrAve(fit$data$landFrac, y, ave.lf)
    lw <- getThisOrAve(fit$data$landMeanWeight, y, ave.lw)
    pm <- getThisOrAve(fit$data$propM, y, ave.pm)
    pf <- getThisOrAve(fit$data$propF, y, ave.pf)
    if (useNMmodel) {
      sim <- t(sapply(1:nrow(sim), function(s) {
        yidx <- which(rnNM == as.integer(y))
        thisnm <- matrix(exp(simLogNM[s, ]), nrow = nrow(opar$logNM))[yidx, 
        ]
        step(sim[s, ], nm = thisnm, recpool = recpool, 
             scale = 1, inyear = (i == 0))
      }))
    }
    else {
      sim <- t(apply(sim, 1, function(s) step(s, nm = nm, 
                                              recpool = recpool, scale = 1, inyear = (i == 
                                                                                        0))))
    }
    if (i != 0) {
      cof <- coef(fit)
      if (deterministic) {
        procVar <- procVar * 0
      }
      if (!is.null(overwriteSelYears)) {
        nn <- length(fit$conf$keyLogFsta[1, ])
        procVar[-c(1:nn), -c(1:nn)] <- 0
      }
      if (!is.null(customSel)) {
        nn <- length(fit$conf$keyLogFsta[1, ])
        procVar[-c(1:nn), -c(1:nn)] <- 0
      }
      if (fit$conf$corFlag < 3) {
        sim <- sim + rmvnorm(nosim, mu = rep(0, nrow(procVar)), 
                             Sigma = procVar)
      }
      else if (fit$conf$corFlag == 3) {
        k <- fit$conf$keyLogFsta[1, ]
        k <- unique(k[k >= 0] + 1)
        nF <- length(k)
        simTmp <- rmvnorm(nosim, mu = rep(0, nrow(procVar)), 
                          Sigma = procVar)
        rhoF = 2/(1 + exp(-2 * cof[names(cof) == "sepFlogitRho"])) - 
          1
        sepFalpha = cof[names(cof) == "sepFalpha"]
        for (jj in 1:nosim) {
          V = mean(sim[jj, (dim(sim)[2] - nF + 1):(dim(sim)[2])])
          U = sim[jj, (dim(sim)[2] - nF + 1):(dim(sim)[2] - 
                                                1)] - V
          Uepsilon = simTmp[jj, (dim(simTmp)[2] - nF + 
                                   1):(dim(simTmp)[2] - 1)]
          Vepsilon = simTmp[jj, dim(simTmp)[2]]
          Unew = rhoF[1] * U + Uepsilon + sepFalpha[1:(length(sepFalpha) - 
                                                         1)]
          Vnew = rhoF[2] * V + Vepsilon + sepFalpha[length(sepFalpha)]
          sim[jj, (dim(simTmp)[2] - nF + 1):(dim(simTmp)[2] - 
                                               1)] = Unew + Vnew
          sim[jj, dim(simTmp)[2]] = -sum(Unew) + Vnew
          sim[jj, 1:(dim(simTmp)[2] - nF)] = sim[jj, 
                                                 1:(dim(simTmp)[2] - nF)] + simTmp[jj, 1:(dim(simTmp)[2] - 
                                                                                            nF)]
        }
      }
      else {
        stop("forcast not implemented for the given corflag")
      }
    }
    if (!is.na(fscale[i + 1])) {
      sim <- t(apply(sim, 1, scaleF, scale = fscale[i + 
                                                      1]))
    }
    if (!is.na(fval[i + 1])) {
      curfbar <- median(apply(sim, 1, fbar))
      adj <- fval[i + 1]/curfbar
      sim <- t(apply(sim, 1, scaleF, scale = adj))
    }
    if (!is.na(cwF[i + 1])) {
      if (missing(customWeights)) 
        stop("customWeights must be supplied when using the cwF option")
      curcwF <- median(apply(sim, 1, getCWF, w = customWeights))
      adj <- cwF[i + 1]/curcwF
      sim <- t(apply(sim, 1, scaleF, scale = adj))
    }
    if (!is.na(catchval[i + 1])) {
      simtmp <- NA
      fun <- function(s) {
        simtmp <<- t(apply(sim, 1, scaleF, scale = s))
        if (useCWmodel | useNMmodel) {
          simcat <- sapply(1:nrow(simtmp), function(k) {
            if (useCWmodel) {
              thisy <- which(rnCW == y)
              thiscw <- matrix(exp(simLogCW[k, ]), nrow = nrow(opar$logCW))[thisy, 
              ]
            }
            else {
              thiscw <- cw
            }
            if (useNMmodel) {
              thisy <- which(rnNM == y)
              thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
              ]
            }
            else {
              thisnm <- nm
            }
            catch(simtmp[k, ], nm = thisnm, cw = thiscw)
          })
        }
        else {
          simcat <- apply(simtmp, 1, catch, nm = nm, 
                          cw = cw)
        }
        return(catchval[i + 1] - median(simcat))
      }
      ff <- uniroot(fun, c(0, 100))$root
      sim <- simtmp
    }
    if (!is.na(catchval.exact[i + 1])) {
      simtmp <- sim
      funk <- function(k) {
        if (useCWmodel) {
          thisy <- which(rnCW == y)
          thiscw <- matrix(exp(simLogCW[k, ]), nrow = nrow(opar$logCW))[thisy, 
          ]
        }
        else {
          thiscw = cw
        }
        if (useNMmodel) {
          thisy <- which(rnNM == y)
          thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
          ]
        }
        else {
          thisnm = nm
        }
        one <- function(s) {
          simtmp[k, ] <<- scaleF(sim[k, ], s)
          simcat <- catch(simtmp[k, ], nm = thisnm, 
                          cw = thiscw)
          return(catchval.exact[i + 1] - simcat)
        }
        ff <- uniroot(one, c(0, 100))$root
      }
      dd <- sapply(1:nrow(sim), funk)
      sim <- simtmp
    }
    if (!is.na(landval[i + 1])) {
      simtmp <- NA
      fun <- function(s) {
        simtmp <<- t(apply(sim, 1, scaleF, scale = s))
        if (useNMmodel) {
          simcat <- sapply(1:nrow(simtmp), function(k) {
            thisy <- which(rnNM == y)
            thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
            ]
            catchFrac(simtmp[k, ], nm = thisnm, w = lw, 
                      frac = lf)
          })
        }
        else {
          simcat <- apply(simtmp, 1, catchFrac, nm = nm, 
                          w = lw, frac = lf)
        }
        return(landval[i + 1] - median(simcat))
      }
      ff <- uniroot(fun, c(0, 100))$root
      sim <- simtmp
    }
    if (!is.na(nextssb[i + 1])) {
      if ((sum(pm) != 0) | (sum(pf) != 0)) 
        stop("nextssb option only available if SSB is calculated in the beginning of the year")
      simtmp <- NA
      rs <- .Random.seed
      fun <- function(s) {
        assign(".Random.seed", rs, envir = .GlobalEnv)
        simtmp <<- t(apply(sim, 1, scaleF, scale = s))
        if (useNMmodel) {
          simsim <- t(sapply(1:nrow(simtmp), function(s) {
            yidx <- which(rnNM == as.integer(y))
            thisnm <- matrix(exp(simLogNM[s, ]), nrow = nrow(opar$logNM))[yidx, 
            ]
            step(simtmp[s, ], nm = thisnm, recpool = recpool, 
                 scale = 1, inyear = (i == 0))
          }))
        }
        else {
          simsim <- t(apply(simtmp, 1, function(s) step(s, 
                                                        nm = nm, recpool = recpool, scale = 1, inyear = (i == 
                                                                                                           0))))
        }
        if (i != 0) {
          if (deterministic) 
            procVar <- procVar * 0
          simsim <- simsim + rmvnorm(nosim, mu = rep(0, 
                                                     nrow(procVar)), Sigma = procVar)
        }
        if (useSWmodel | useMOmodel | useNMmodel) {
          ssbswmoi <- function(k) {
            if (useSWmodel) {
              thisy <- which(rnSW == y)
              thissw <- matrix(exp(simLogSw[k, ]), nrow = nrow(opar$logSW))[thisy, 
              ]
            }
            else {
              thissw <- sw
            }
            if (useMOmodel) {
              thisy <- which(rnMO == y)
              thismo <- matrix(plogis(simLogitMO[k, 
              ]), nrow = nrow(opar$logitMO))[thisy, 
              ]
            }
            else {
              thismo <- mo
            }
            if (useNMmodel) {
              thisy <- which(rnNM == y)
              thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
              ]
            }
            else {
              thisnm <- nm
            }
            ssb(simsim[k, ], nm = thisnm, sw = thissw, 
                mo = thismo, pm = pm, pf = pf)
          }
          simnextssb <- sapply(1:nrow(simsim), ssbswmoi)
        }
        else {
          simnextssb <- apply(simsim, 1, ssb, nm = nm, 
                              sw = sw, mo = mo, pm = pm, pf = pf)
        }
        return(nextssb[i + 1] - median(simnextssb))
      }
      ff <- uniroot(fun, c(0, 100))$root
      sim <- simtmp
    }
    fbarsim <- apply(sim, 1, fbar)
    fbarLsim <- apply(sim, 1, fbarFrac, lf = lf)
    if (useCWmodel | useNMmodel) {
      catchcwi <- function(k) {
        if (useCWmodel) {
          thisy <- which(rnCW == y)
          thiscw <- matrix(exp(simLogCW[k, ]), nrow = nrow(opar$logCW))[thisy, 
          ]
        }
        else {
          thiscw <- cw
        }
        if (useNMmodel) {
          thisy <- which(rnNM == y)
          thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
          ]
        }
        else {
          thisnm <- nm
        }
        catch(sim[k, ], nm = thisnm, cw = thiscw)
      }
      catchsim <- sapply(1:nrow(sim), catchcwi)
    }
    else {
      catchsim <- apply(sim, 1, catch, nm = nm, cw = cw)
    }
    if (useNMmodel) {
      landsim <- sapply(1:nrow(sim), function(k) {
        thisy <- which(rnNM == y)
        thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
        ]
        catchFrac(sim[k, ], nm = thisnm, w = lw, frac = lf)
      })
    }
    else {
      landsim <- apply(sim, 1, catchFrac, nm = nm, w = lw, 
                       frac = lf)
    }
    catchatagesim <- apply(sim, 1, catchatage, nm = nm)
    natagesim <- apply(sim, 1, getN)
    if (useSWmodel | useMOmodel | useNMmodel) {
      ssbswmoi <- function(k) {
        if (useSWmodel) {
          thisy <- which(rnSW == y)
          thissw <- matrix(exp(simLogSw[k, ]), nrow = nrow(opar$logSW))[thisy, 
          ]
        }
        else {
          thissw <- sw
        }
        if (useMOmodel) {
          thisy <- which(rnMO == y)
          thismo <- matrix(plogis(simLogitMO[k, ]), 
                           nrow = nrow(opar$logitMO))[thisy, ]
        }
        else {
          thismo <- mo
        }
        if (useNMmodel) {
          thisy <- which(rnNM == y)
          thisnm <- matrix(exp(simLogNM[k, ]), nrow = nrow(opar$logNM))[thisy, 
          ]
        }
        else {
          thisnm <- nm
        }
        ssb(sim[k, ], nm = thisnm, sw = thissw, mo = thismo, 
            pm = pm, pf = pf)
      }
      ssbsim <- sapply(1:nrow(sim), ssbswmoi)
    }
    else {
      ssbsim <- apply(sim, 1, ssb, nm = nm, sw = sw, mo = mo, 
                      pm = pm, pf = pf)
    }
    tsbsim <- apply(sim, 1, tsb, sw = sw)
    if (lagR) {
      recsim <- exp(sim[, 2])
    }
    else {
      recsim <- exp(sim[, 1])
    }
    cwFsim <- rep(NA, nrow(sim))
    if (!missing(customWeights)) {
      cwFsim <- apply(sim, 1, getCWF, w = customWeights)
    }
    simlist[[i + 1]] <- list(sim = sim, fbar = fbarsim, 
                             catch = catchsim, ssb = ssbsim, rec = recsim, cwF = cwFsim, 
                             natage=natagesim,catchatage = catchatagesim, land = landsim, fbarL = fbarLsim, 
                             tsb = tsbsim, year = y)
  }
  attr(simlist, "fit") <- fit
  collect <- function(x) {
    quan <- quantile(x, c(0.5, 0.025, 0.975))
    c(median = quan[1], low = quan[2], high = quan[3])
  }
  fbar <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$fbar))), 
                3)
  fbarL <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$fbarL))), 
                 3)
  rec <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$rec))))
  ssb <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$ssb))))
  tsb <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$tsb))))
  catch <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$catch))))
  land <- round(do.call(rbind, lapply(simlist, function(xx) collect(xx$land))))
  caytable <- round(do.call(rbind, lapply(simlist, function(xx) apply(xx$catchatage, 
                                                                      1, collect))))
  naytable <- round(do.call(rbind, lapply(simlist, function(xx) apply(xx$natage, 
                                                                      1, collect))))
  tab <- cbind(fbar, rec, ssb, catch)
  if (splitLD) {
    tab <- cbind(tab, fbarL, fbar - fbarL, land, catch - 
                   land)
  }
  if (addTSB) {
    tab <- cbind(tab, tsb)
  }
  if (!missing(customWeights)) 
    tab <- cbind(tab, cwF = round(do.call(rbind, lapply(simlist, 
                                                        function(xx) collect(xx$cwF))), 3))
  rownames(tab) <- unlist(lapply(simlist, function(xx) xx$year))
  nam <- c("median", "low", "high")
  basename <- c("fbar:", "rec:", "ssb:", "catch:")
  if (splitLD) {
    basename <- c(basename, "fbarL:", "fbarD:", "Land:", 
                  "Discard:")
  }
  if (addTSB) {
    basename <- c(basename, "tsb:")
  }
  if (!missing(customWeights)) {
    basename <- c(basename, "cwF:")
  }
  colnames(tab) <- paste0(rep(basename, each = length(nam)), 
                          nam)
  attr(simlist, "tab") <- tab
  shorttab <- t(tab[, grep("median", colnames(tab))])
  rownames(shorttab) <- sub(":median", "", paste0(label, if (!is.null(label)) 
    ":", rownames(shorttab)))
  attr(simlist, "shorttab") <- shorttab
  attr(simlist, "label") <- label
  attr(simlist, "caytable") <- caytable
  attr(simlist, "naytable") <- naytable
  class(simlist) <- "samforecast"
  if (!savesim) {
    simlistsmall <- lapply(simlist, function(x) list(year = x$year))
    attributes(simlistsmall) <- attributes(simlist)
    return(simlistsmall)
  }
  else {
    return(simlist)
  }
}