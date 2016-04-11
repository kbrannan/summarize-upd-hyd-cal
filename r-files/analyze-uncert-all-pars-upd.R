

## main path for uncert
chr.uncert.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert"

## main path for uncert-rerun
chr.uncert.rerun.dir <- paste0(chr.uncert.dir, "/uncert-rerun")

## get all the par files used in uncert-rerun
chr.par.files <- list.files(path = chr.uncert.rerun.dir, pattern = "\\.par$",
                            recursive = TRUE, full.names = TRUE)

## get calibrated par values
tmp.pars <- scan(file = paste0(chr.uncert.dir, "/calib.par"), 
                 what = "character", sep = "\n", quiet = TRUE, skip = 1)
tmp.par <- do.call(rbind, 
       strsplit(gsub(" {1,}", ",", gsub("(^( ){1,})|(( ){1,}$)", "", tmp.pars)),
                split = ","))[ , c(-3,-4)]
## create par data.frame
df.pars <- data.frame(par = tmp.par[, 1], run = "cal", val = tmp.par[, 2], 
                      stringsAsFactors = FALSE)

## clean up
rm(list=ls(pattern = "^tmp\\."))

## loop through res files and estimate FDC for each run
for(ii in 1:length(chr.par.files)) {
  ## get calibrated par values
  tmp.pars <- scan(file =chr.par.files[ii], 
                   what = "character", sep = "\n", quiet = TRUE, skip = 1)
  tmp.par <- do.call(rbind, 
                     strsplit(gsub(" {1,}", ",", gsub("(^( ){1,})|(( ){1,}$)", "", tmp.pars)),
                              split = ","))[ , c(-3,-4)]
  ## create par data.frame
  df.pars <- rbind(df.pars, 
                   data.frame(
                     par = tmp.par[, 1], 
                     run = gsub("(^.*/)|(\\.par$)","", chr.par.files[ii]), 
                     val = tmp.par[, 2], stringsAsFactors = FALSE))
  
  ## clean up
  rm(list=ls(pattern = "^tmp\\."))
}

gsub("(^.*/)|(\\.par$)","", chr.par.files[1])
