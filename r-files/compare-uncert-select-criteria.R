## load packages
library(doBy, quietly = TRUE)

## path for output
chr.bacteria.twg.17.dir <- "M:/Presentations/2016-02-09 Bacteria TWG 17"

## main path for uncert re-reun
chr.uncert.rerun.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert/uncert-rerun"

## get names of runs selected
load(paste0(chr.uncert.rerun.dir, "/fdc-est.RData"))

## get bounds of USGS eq
num.lower <- df.fdcs[df.fdcs$par == "lower", "y"]
num.upper <- df.fdcs[df.fdcs$par == "upper", "y"]

## get uncert runs
chr.uncert <- unique(df.fdcs[df.fdcs$src == "uncert", "run"])

## start data.frame for summary
df.kept.runs <- data.frame(crit = "none", run = "none", 
                           stringsAsFactors = FALSE)


cur.crit <- "5-5"
for(ii in 1:length(chr.uncert)) {
  tmp.quant <- df.fdcs[df.fdcs$run == chr.uncert[ii], "y"]
  ## check if current fdc flows are within the USGS error bars 
  if(sum(num.lower <= tmp.quant & 
         tmp.quant <= num.upper) >= (length(tmp.quant) - 0)) {
    ## if yes keep file name append to list of files
    df.kept.runs <- rbind(df.kept.runs,
                          data.frame(crit = cur.crit, run = chr.uncert[ii], 
                               stringsAsFactors = FALSE))
  }
  
}

cur.crit <- "4-5"
for(ii in 1:length(chr.uncert)) {
  tmp.quant <- df.fdcs[df.fdcs$run == chr.uncert[ii], "y"]
  ## check if current fdc flows are within the USGS error bars 
  if(sum(num.lower <= tmp.quant & 
         tmp.quant <= num.upper) >= (length(tmp.quant) - 1)) {
    ## if yes keep file name append to list of files
    df.kept.runs <- rbind(df.kept.runs,
                          data.frame(crit = cur.crit, run = chr.uncert[ii], 
                                     stringsAsFactors = FALSE))
  }
  
}

cur.crit <- "3-5"
for(ii in 1:length(chr.uncert)) {
  tmp.quant <- df.fdcs[df.fdcs$run == chr.uncert[ii], "y"]
  ## check if current fdc flows are within the USGS error bars 
  if(sum(num.lower <= tmp.quant & 
         tmp.quant <= num.upper) >= (length(tmp.quant) - 2)) {
    ## if yes keep file name append to list of files
    df.kept.runs <- rbind(df.kept.runs,
                          data.frame(crit = cur.crit, run = chr.uncert[ii], 
                                     stringsAsFactors = FALSE))
  }
  
}

cur.crit <- "2-5"
for(ii in 1:length(chr.uncert)) {
  tmp.quant <- df.fdcs[df.fdcs$run == chr.uncert[ii], "y"]
  ## check if current fdc flows are within the USGS error bars 
  if(sum(num.lower <= tmp.quant & 
         tmp.quant <= num.upper) >= (length(tmp.quant) - 3)) {
    ## if yes keep file name append to list of files
    df.kept.runs <- rbind(df.kept.runs,
                          data.frame(crit = cur.crit, run = chr.uncert[ii], 
                                     stringsAsFactors = FALSE))
  }
  
}

cur.crit <- "1-5"
for(ii in 1:length(chr.uncert)) {
  tmp.quant <- df.fdcs[df.fdcs$run == chr.uncert[ii], "y"]
  ## check if current fdc flows are within the USGS error bars 
  if(sum(num.lower <= tmp.quant & 
         tmp.quant <= num.upper) >= (length(tmp.quant) - 4)) {
    ## if yes keep file name append to list of files
    df.kept.runs <- rbind(df.kept.runs,
                          data.frame(crit = cur.crit, run = chr.uncert[ii], 
                                     stringsAsFactors = FALSE))
  }
  
}

df.kept.runs <- df.kept.runs[-1,]

df.kept.runs$crit <- factor(df.kept.runs$crit, 
                            levels = c("5-5", "4-5", "3-5", "2-5", "1-5"))

summaryBy(run ~ crit, data = df.kept.runs, FUN = length)

num.min.fdc <- 0
for(ii in 1:length(chr.uncert)) {
  tmp.quant <- df.fdcs[df.fdcs$run == chr.uncert[ii], "y"]
  ## check if current min fdc flows are within the min USGS error bars 
  if(num.lower[5] <= tmp.quant[5] & tmp.quant[5] <= num.upper[5]) {
    ## if yes keep file name append to list of files
    num.min.fdc <- num.min.fdc + 1
  }
  
}

num.min.fdc.below <- 0
for(ii in 1:length(chr.uncert)) {
  tmp.quant <- df.fdcs[df.fdcs$run == chr.uncert[ii], "y"]
  ## check if current min fdc flows are within the min USGS error bars 
  if(num.lower[5] <= tmp.quant[5]) {
    ## if yes keep file name append to list of files
    num.min.fdc.below <- num.min.fdc.below + 1
  }
  
}

num.min.fdc.above <- 0
for(ii in 1:length(chr.uncert)) {
  tmp.quant <- df.fdcs[df.fdcs$run == chr.uncert[ii], "y"]
  ## check if current min fdc flows are within the min USGS error bars 
  if(num.upper[5] >= tmp.quant[5]) {
    ## if yes keep file name append to list of files
    num.min.fdc.above <- num.min.fdc.below + 1
  }
  
}
