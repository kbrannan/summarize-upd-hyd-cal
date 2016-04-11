## main path for uncert re-reun
chr.uncert.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert"

## main path for uncert re-reun
chr.uncert.rerun.dir <- paste0(chr.uncert.dir, "/uncert-rerun")

## get USGS eq fdc
load(paste0(chr.uncert.rerun.dir, "/fdc-usgs-eq.RData"))

## quantiles used
num.x <- 1 - df.fdc.ss.est$FDPercent

## start results data.frame
df.fdcs <- data.frame(x = num.x, y = df.fdc.ss.est$FDEst, par = "est", 
                      run = "eq", stringsAsFactors = FALSE)
df.fdcs <- rbind(df.fdcs, data.frame(x = num.x, y = df.fdc.ss.est$lower, par = "lower", 
                      run = "eq", stringsAsFactors = FALSE))
df.fdcs <- rbind(df.fdcs, data.frame(x = num.x, y = df.fdc.ss.est$upper, par = "upper", 
                            run = "eq", stringsAsFactors = FALSE))
## clean up
rm(df.fdc.ss.est)

## get list of res files
chr.res.files <- list.files(path = chr.uncert.rerun.dir, full.names = TRUE,
                            pattern = "\\.res$", recursive = TRUE)

## add the calibration res file to this
chr.res.files <- c(paste0(chr.uncert.dir,"/calib.res"),
                   chr.res.files)


## loop through res files and estimate FDC for each run
for(ii in 1:length(chr.res.files)) {
  ## current run name
  tmp.name <- gsub("(^.*/)|(\\.res)","",chr.res.files[ii])

  ## read residuals file
  tmp.chr.res <- scan(file = chr.res.files[ii], what = "character", 
                      sep = "\n", quiet = TRUE)
  ## replace * with x in field names
  tmp.chr.res[1] <- gsub("\\*","x", tmp.chr.res[1])
  
  ## replace spaces among columns with comma
  tmp.chr.res <- gsub("( ){1,}",",",tmp.chr.res)
  
  ## convert chracter vector to data.frame
  tmp.df.res <- data.frame(do.call(rbind,strsplit(tmp.chr.res,split = ",")), 
                       stringsAsFactors = FALSE)
  rm(tmp.chr.res) ## clean up
  
  ## remove first column becuase it is empty
  tmp.df.res <- tmp.df.res[ ,-1]
  
  ## first row is names for columns
  names(tmp.df.res) <- tmp.df.res[1, ]
  
  ## discard first row
  tmp.df.res <- tmp.df.res[-1, ]
  
  ## get mflow
  tmp.mflow <- tmp.df.res[grep("mflow", as.character(tmp.df.res$Group)), ]
  rm(tmp.df.res) ## clean up
  
  ## estimate fdc for current flow
  tmp.quant <- quantile(as.numeric(tmp.mflow$Modelled), num.x, names = FALSE)

  ## add to df.fdcs
  df.fdcs <- rbind(df.fdcs, 
                   data.frame(x = num.x, y = tmp.quant, par = "est", 
                        run = tmp.name, stringsAsFactors = FALSE))

  ## clean up
  rm(list=ls(pattern="^tmp\\."))
  
}

## estimate fdc for observed
tmp.name <- "obs"

## read residuals file
tmp.chr.res <- scan(file = chr.res.files[1], what = "character", 
                    sep = "\n", quiet = TRUE)
## replace * with x in field names
tmp.chr.res[1] <- gsub("\\*","x", tmp.chr.res[1])

## replace spaces among columns with comma
tmp.chr.res <- gsub("( ){1,}",",",tmp.chr.res)

## convert chracter vector to data.frame
tmp.df.res <- data.frame(do.call(rbind,strsplit(tmp.chr.res,split = ",")), 
                         stringsAsFactors = FALSE)
rm(tmp.chr.res) ## clean up

## remove first column becuase it is empty
tmp.df.res <- tmp.df.res[ ,-1]

## first row is names for columns
names(tmp.df.res) <- tmp.df.res[1, ]

## discard first row
tmp.df.res <- tmp.df.res[-1, ]

## get mflow
tmp.mflow <- tmp.df.res[grep("mflow", as.character(tmp.df.res$Group)), ]
rm(tmp.df.res) ## clean up

## estimate fdc for current flow
tmp.quant <- quantile(as.numeric(tmp.mflow$Measured), num.x, names = FALSE)

## add to df.fdcs
df.fdcs <- rbind(df.fdcs, 
                 data.frame(x = num.x, y = tmp.quant, par = "est", 
                            run = tmp.name, stringsAsFactors = FALSE))

## create factors
df.fdcs$run <- factor(df.fdcs$run, 
                      levels = c("obs", "cal" , 
                                 unique((df.fdcs$run[grep("^uncert",df.fdcs$run)])))) 

df.fdcs$par <- factor(df.fdcs$par, levels = c("est", "lower", "upper"))


## clean up
rm(list=ls(pattern="^tmp\\."))

## save fdc estimates
save(df.fdcs, file = paste0(chr.uncert.rerun.dir,"/fdc-est.RData"))
