## analyze pest-hspf results

## load packages
library(ggplot2, quietly = TRUE)
library(reshape, quietly = TRUE)
library(DVstats, quietly = TRUE)
library(gridExtra, quietly = TRUE)
library(doBy, quietly = TRUE)
library(gtable, quietly = TRUE)

## working path 
chr.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"

## load functions
source("M:/Models/Bacteria/LDC/Calculations/Rscripts/LDC Functions.R")
source("M:/Models/Bacteria/HSPF/HydroCal201506/R_projs/Select_Storm_HydCal/devel/functions.R")
source("M:/Models/Bacteria/HSPF/bigelkHydroCal201601/r-files/functions.R")


## orginal WDM period
## read residuals file
chr.res.org <- scan(file = paste0(chr.dir,"/org-calib/Final_Deliverables_EPA_July2012/PEST_end/control.res"),
                what = "character", sep = "\n", quiet = TRUE)
## replace * with x in field names
chr.res.org[1] <- gsub("\\*","x",chr.res.org[1])

## replace spaces among columns with comma
chr.res.org <- gsub("( ){1,}",",",chr.res.org)

## convert chracter vector to data.frame
df.res.org <- data.frame(do.call(rbind,strsplit(chr.res.org,split = ",")), 
                     stringsAsFactors = FALSE)

## remove first column becuase it is empty
df.res.org <- df.res.org[ ,-1]

## first row is names for columns
names(df.res.org) <- df.res.org[1, ]

## discard first row
df.res.org <- df.res.org[-1, ]

## get simulation dates from UCI
chr.sim.dates <- gsub("([aA-zZ ])|(00\\:00)|(24\\:00)","",
     grep("START", scan(file = paste0(chr.dir, "/org-calib/Final_Deliverables_EPA_July2012/PEST_end/bigelk.uci"), 
                   sep = "\n", what = "character", quiet = TRUE), value = TRUE))
dte.str <- as.Date(substr(chr.sim.dates, start =  1, stop = 10), fmt = "%Y/%m/%d")
dte.end <- as.Date(substr(chr.sim.dates, start = 11, stop = 20), fmt = "%Y/%m/%d")

## create date sequence
dte.flows <- seq(from = as.Date(dte.str), to = as.Date(dte.end), by = "day")

## get mlog time-series
df.mlog <- data.frame(dates = dte.flows, 
                      df.res.org[grep("mlog", as.character(df.res.org$Group)), ])
df.mlog[, 4:12] <- sapply(df.mlog[ , 4:12], as.numeric)
## add year as factor
df.mlog <- cbind(df.mlog, year = factor(format(df.mlog$dates, "%Y")))
### add month
df.mlog <- cbind(df.mlog, 
                 month = 
                   factor(
                     format(df.mlog$dates, "%b"),
                     levels = c("Oct", "Nov", "Dec", "Jan","Feb",
                                "Mar", "Apr", "May","Jun", "Jul", 
                                "Aug", "Sep")))
## add season
chr_season <- function(chr.month) {
  if(chr.month %in% c("Dec", "Jan", "Feb")) season  <- "winter"
  if(chr.month %in% c("Mar", "Apr", "May")) season   <- "spring"
  if(chr.month %in% c("Jul", "Jun", "Aug")) season   <- "summer"
  if(chr.month %in% c("Sep", "Oct", "Nov")) season <- "fall"
  return(season)
}
df.mlog <- cbind(df.mlog, 
                 season = sapply(as.character(df.mlog$month), chr_season))

## assign flow zones used in LCD
## add ecdf to mlog
mlog.ecdf <- ecdf(df.mlog$Modelled)
df.mlog <- cbind(df.mlog, exceed = 100 * ( 1 - round(mlog.ecdf(df.mlog$Modelled),3)) )
## function taken from the LDC functions
get.flow.zone <- function(flow.exceed) {
  flow.zone <- NA
  if(is.na(flow.exceed) == TRUE) {return(flow.zone)}
  if(flow.exceed >= 0 & flow.exceed <= 10) {
    flow.zone <- "high"
  } else if(flow.exceed > 10 & flow.exceed <= 40) {
    flow.zone <- "transitional"
  } else if(flow.exceed > 40 & flow.exceed <= 60) {
    flow.zone <- "typical"
  } else if(flow.exceed > 60 & flow.exceed <= 90) {
    flow.zone <- "dry"
  } else if(flow.exceed > 90 & flow.exceed <= 100) {
    flow.zone <- "low"
  }
  return(flow.zone)
}
df.mlog <- cbind(df.mlog, 
                 flw.zn = factor(
                   sapply(df.mlog$exceed,get.flow.zone), 
                   levels = c("high", "transitional", "typical", 
                              "dry", "low")))
## boxplot of weight x residuals
p.mlog.bar.wt.rs.all <- 
  ggplot(data = df.mlog,
         aes(x=factor(0), y = WeightxResidual,
             title = "weight x residuals for log10 of daily flow (mlog)")) +
  xlab("") + geom_boxplot()
##plot(p.mlog.bar.wt.rs.all)

## summary table of weight x residuals
df.sum.mlog.res.org <- data.frame(
  mean = mean(df.mlog$WeightxResidual),
  sd = sd(df.mlog$WeightxResidual),
  median = median(df.mlog$WeightxResidual),
  min = min(df.mlog$WeightxResidual),
  max = max(df.mlog$WeightxResidual),
  n = length(df.mlog$WeightxResidual))

# commands to write table to pdf
#grid.table(df.sum.mlog.res.org, show.rownames = FALSE)
#dev.off()

## boxplot of weight x residuals by year
p.mlog.bar.wt.rs.yr <- 
  ggplot(data = df.mlog, 
         aes(x=year, y = WeightxResidual, 
             title = "weight x residuals by year for log10 of daily flow (mlog)")) + 
  geom_boxplot()
##plot(p.mlog.bar.wt.rs.yr)

## summary table of weight x residuals
df.sum.by.year.mlog.res.org <- 
  summaryBy(WeightxResidual ~ year, data = df.mlog,
            FUN = c(mean, sd, median, min, max, length))
names(df.sum.by.year.mlog.res.org) <- 
  c("year", "mean", "sd", "median", "min", "max", "n")

## boxplot of weight x residuals by season
p.mlog.bar.wt.rs.sn <- 
  ggplot(data = df.mlog,
        aes(x=season, y = WeightxResidual, 
            title = "weight x residuals by season for log10 of daily flow (mlog)")) + 
  geom_boxplot()
##plot(p.mlog.bar.wt.rs.sn)

## summary table of weight x residuals by season
df.sum.by.year.mlog.res.org.sn <- 
  summaryBy(WeightxResidual ~ season, data = df.mlog,
            FUN = c(mean, sd, median, min, max, length))
names(df.sum.by.year.mlog.res.org.sn) <- 
  c("season", "mean", "sd", "median", "min", "max", "n")


## boxplot of weight x residuals by ldc flow zone
p.mlog.bar.wt.rs.fz <- 
  ggplot(data = df.mlog, 
         aes(x=flw.zn, y = WeightxResidual, title = "weight x residuals by ldc flow-zone for log10 of daily flow (mlog)")) + 
  xlab("Flow-Zone") + geom_boxplot()
##plot(p.mlog.bar.wt.rs.fz)

## summary table of weight x residuals by ldc flow zone
df.sum.by.year.mlog.res.org.fz <- 
  summaryBy(WeightxResidual ~ flw.zn, data = df.mlog,
            FUN = c(mean, sd, median, min, max, length))
names(df.sum.by.year.mlog.res.org.fz) <- 
  c("flw.zn", "mean", "sd", "median", "min", "max", "n")


## get mflow time-series
df.mflow <- data.frame(dates = dte.flows, 
                      df.res.org[grep("mflow", as.character(df.res.org$Group)), ])
df.mflow[, 4:12] <- sapply(df.mflow[ , 4:12], as.numeric)
## add year as factor
df.mflow <- cbind(df.mflow, year = factor(format(df.mflow$dates, "%Y")))
### add month
df.mflow <- cbind(df.mflow, 
                 month = 
                   factor(
                     format(df.mflow$dates, "%b"),
                     levels = c("Oct", "Nov", "Dec", "Jan","Feb",
                                "Mar", "Apr", "May","Jun", "Jul", 
                                "Aug", "Sep")))
## add season
df.mflow <- cbind(df.mflow, 
                 season = sapply(df.mflow$month, chr_season))

## assign flow zones used in LCD
## add ecdf to mlog
mflow.ecdf <- ecdf(df.mflow$Modelled)
df.mflow <- cbind(df.mflow, exceed = 100 * ( 1 - round(mflow.ecdf(df.mflow$Modelled),3)) )
df.mflow <- cbind(df.mflow, 
                 flw.zn = factor(
                   sapply(df.mflow$exceed,get.flow.zone), 
                   levels = c("high", "transitional", "typical", 
                              "dry", "low")))
## boxplot of weight x residuals
p.mflow.bar.wt.rs.all <- 
  ggplot(data = df.mflow,
         aes(x=factor(0), y = WeightxResidual,
             title = "weight x residuals for daily flow (mflow)")) + 
  xlab("") + geom_boxplot()
##plot(p.mflow.bar.wt.rs.all)

## summary table of weight x residuals
df.sum.mflow.res.org <- data.frame(
  mean = mean(df.mflow$WeightxResidual),
  sd = sd(df.mflow$WeightxResidual),
  median = median(df.mflow$WeightxResidual),
  min = min(df.mflow$WeightxResidual),
  max = max(df.mflow$WeightxResidual),
  n = length(df.mflow$WeightxResidual))

## boxplot of weight x residuals by year
p.mflow.bar.wt.rs.yr <- 
  ggplot(data = df.mflow,
         aes(x=year, y = WeightxResidual,
             title = "weight x residuals by year for daily flow (mflow)")) + 
  geom_boxplot()
##plot(p.mflow.bar.wt.rs.yr)

## summary table of weight x residuals by year
df.sum.by.year.mflow.res.org.yr <- 
  summaryBy(WeightxResidual ~ year, data = df.mflow,
            FUN = c(mean, sd, median, min, max, length))
names(df.sum.by.year.mflow.res.org.yr) <- 
  c("year", "mean", "sd", "median", "min", "max", "n")

## boxplot of weight x residuals by season
p.mflow.bar.wt.rs.sn <- 
  ggplot(data = df.mflow, 
         aes(x=season, y = WeightxResidual, 
             title = "weight x residuals by season for daily flow (mflow)")) + 
  geom_boxplot()
##plot(p.mflow.bar.wt.rs.sn)

## summary table of weight x residuals by season
df.sum.by.year.mflow.res.org.sn <- 
  summaryBy(WeightxResidual ~ season, data = df.mflow,
            FUN = c(mean, sd, median, min, max, length))
names(df.sum.by.year.mflow.res.org.sn) <- 
  c("year", "mean", "sd", "median", "min", "max", "n")

## boxplot of weight x residuals by ldc flow zone
p.mflow.bar.wt.rs.fz <- 
  ggplot(data = df.mflow,
         aes(x=flw.zn, y = WeightxResidual,
             title = "weight x residuals by ldc flow-zone for daily flow (mflow)")) + 
  xlab("Flow-Zone") + geom_boxplot()
##plot(p.mflow.bar.wt.rs.fz)

## summary table of weight x residuals by ldc flow zone
df.sum.by.year.mflow.res.org.fz <- 
  summaryBy(WeightxResidual ~ flw.zn, data = df.mflow,
            FUN = c(mean, sd, median, min, max, length))
names(df.sum.by.year.mflow.res.org.fz) <- 
  c("year", "mean", "sd", "median", "min", "max", "n")

## get mtime
df.mtime <- data.frame(
  probs = eval(
    parse(text = 
            gsub("(tmp.per)|(<-)|(( ){1, })", "", 
                 grep("tmp.per <-", 
                      scan(
                        file = paste0(chr.dir, "/r-files/hspf-output-proc.R"), 
                        sep = "\n", 
                        what = "character", 
                        quiet = TRUE), 
                      value = TRUE)))), 
  df.res.org[grep("mtime", as.character(df.res.org$Group)), ])
df.mtime[ , 4:12] <- sapply(df.mtime[, 4:12], as.numeric)
## create a long format table for mtime with factor indicating if from obs or model
df.mtime.lg <- data.frame(rbind(cbind(src = "obs", value = df.mtime[ , 5], df.mtime[ ,c(1,6:12)]),
                          cbind(src = "model", value = df.mtime[ , 4], df.mtime[ ,c(1,6:12)])),
                          stringsAsFactors = FALSE)
df.mtime.lg$src <- as.factor(df.mtime.lg$src)
levels(df.mtime.lg$src) <- c("Obs", "Model")
df.mtime.lg[ , -1] <- sapply(df.mtime.lg[ , -1], as.numeric)

## estimate fdc
## stations at outlet is LASAR 34453
tmp.one.station <- 34453
tmp.ss.est.fn <- paste0("st",tmp.one.station,".xml")
tmp.fdc.ss.est <- fdc.ss.estimate(ss.fn=tmp.ss.est.fn, ss.path=get.path("StreamStatsBacteria"))
tmp.fdc.ss.name <- paste0("fdc.ss.st",tmp.one.station)
eval(parse(text=paste0(tmp.fdc.ss.name," <- tmp.fdc.ss.est")))
rm(list=ls(pattern="^tmp\\.*")) ## clean up

## create three difference data.frames for plots
df.mtime.obs <- df.mtime.lg[df.mtime.lg$src == "Obs", c("probs", "value")]
df.mtime.mod <- df.mtime.lg[df.mtime.lg$src == "Model", c("probs", "value", "Residual","WeightxResidual")]
df.mtime.eq <- fdc.ss.st34453

df.mtime.obs$probs <- 100 * (1 - df.mtime.obs$probs)
df.mtime.mod$probs <- 100 * (1 - df.mtime.mod$probs)
df.mtime.eq$FDPercent <- 100 * df.mtime.eq$FDPercent

names(df.mtime.obs) <- c("x", "y")
names(df.mtime.mod) <- c("x", "y", "res", "wxres")
names(df.mtime.eq)  <- c("x", "y", "ymin", "ymax")

df.mtime.obs <- df.mtime.obs[order(df.mtime.obs$x), ]
df.mtime.mod <- df.mtime.mod[order(df.mtime.mod$x), ]

df.mtime.all <- melt(list(obs = df.mtime.obs, 
                          mod = df.mtime.mod, 
                          eq = df.mtime.eq),
                     id.vars = "x")

df.mtime.all$L1 <- factor(df.mtime.all$L1, levels = c("obs", "mod", "eq"))

## plot mtime and related data
p.mtime00 <- ggplot(data = df.mtime.all,
                    title = "flow duration curves") + 
  scale_y_log10() + 
  scale_colour_manual(name = "", breaks = c("obs", "mod", "eq"), 
                        labels = c("Obs", "Model", "USGS Eq"), 
                      values = c("blue", "green", "black")) +
  scale_shape_manual(name = "", breaks = c("obs", "mod", "eq"), 
                     labels = c("Obs", "Model", "USGS Eq"),
                     values = c(21, 17, 15)) +
    xlab("Percent Time Greater") + ylab("Mean Daily Flow (cfs)")

## add obs and mod flow data along with reg eq
p.mtime00 <- p.mtime00 + 
  geom_line(data = df.mtime.all[as.character(df.mtime.all$variable) == "y", ],
            aes(x = x, y = value, colour = L1))

p.mtime00 <- p.mtime00 + 
  geom_point(data = df.mtime.all[as.character(df.mtime.all$variable) == "y", ],
             aes(x = x, y = value, colour = L1, shape = L1), size = 4)

## add error bars from reg eq
p.mtime00 <- p.mtime00 + 
  geom_errorbar(data = df.mtime.all[df.mtime.all$L1 == "eq", ], 
                aes(x = df.mtime.all[df.mtime.all$L1 == "eq" & 
                                       as.character(df.mtime.all$variable) == "ymin", 'x'],
    ymin = df.mtime.all[df.mtime.all$L1 == "eq" & 
                          as.character(df.mtime.all$variable) == "ymin", 'value'],
    ymax = df.mtime.all[df.mtime.all$L1 == "eq" & 
                          as.character(df.mtime.all$variable) == "ymax", "value"]
    ))
##plot(p.mtime00)

## create summery table of mtime data
df.mtime.all.y <- df.mtime.all[df.mtime.all$variable == "y", ][, -2]
df.mtime.all.y$x <- round(df.mtime.all.y$x, 2)
df.sum.mtime <- cast(df.mtime.all.y, x~L1, value = "value")


## get mvol_ann
chr.yrs <- unique(format(dte.flows, "%Y"))
df.mvol_ann <- data.frame(year = factor(chr.yrs), 
                       df.res.org[grep("mvol_ann", as.character(df.res.org$Group)), ])
df.mvol_ann[, 4:12] <- sapply(df.mvol_ann[ , 4:12], as.numeric)

## boxplot of weight x residuals
p.mvol_ann.bar.wt.rs.all <- 
  ggplot(data = df.mvol_ann,
         aes(x=factor(0), y = WeightxResidual, 
             title = "weight x residuals for annual flow volume (mvol_ann)")) +
  xlab("") + geom_boxplot()
##plot(p.mvol_ann.bar.wt.rs.all)

## scatter plot of weight x residuals by year
# p.mvol_ann.pnt.wt.rs.yr <- 
#   ggplot(data = df.mvol_ann, 
#          aes(x=as.numeric(as.character(df.mvol_ann$year)),
#              y = WeightxResidual,
#              title = "weight x residuals by year for annual flow volume (mvol_ann)")) +
#   geom_point(shape = 1, size = 4)
# plot(p.mvol_ann.pnt.wt.rs.yr)

## bar plot of weight x residuals by year
p.mvol_ann.bar.wt.rs.yr <- 
  ggplot(data = df.mvol_ann, 
         aes(x=as.numeric(as.character(df.mvol_ann$year)),
             y = WeightxResidual,
             title = "weight x residuals by year for annual flow volume (mvol_ann)")) + 
  xlab("year") + geom_bar(stat = "identity", 
                         fill = "blue", position=position_dodge())
##plot(p.mvol_ann.bar.wt.rs.yr)

## get mvol_smr
df.mvol_smr <- data.frame(
  year = factor(
    unique(
      format(df.mlog$dates[
        grep("summer", as.character(df.mlog$season))], "%Y"))), 
  df.res.org[grep("mvol_smr", as.character(df.res.org$Group)), ])
df.mvol_smr[, 4:12] <- sapply(df.mvol_smr[ , 4:12], as.numeric)

## boxplot of weight x residuals
p.mvol_smr.bar.wt.rs.all <- 
  ggplot(data = df.mvol_smr, 
         aes(x=factor(0), y = Residual, 
             title = "weight x residuals for summer flow volume (mvol_smr)")) + 
  xlab("") + geom_boxplot()
##plot(p.mvol_smr.bar.wt.rs.all)

## scatter plot of weight x residuals by year
# p.mvol_smr.pnt.wt.rs.yr <- 
#   ggplot(data = df.mvol_smr,
#          aes(x=as.numeric(as.character(df.mvol_smr$year)),
#            y = Residual,
#            title = "weight x residuals by year for summer flow volume (mvol_smr)")) + 
#   xlab("year") + ylab("residual (ac-ft)") + geom_point(shape = 1, size = 4)
# plot(p.mvol_smr.pnt.wt.rs.yr)

## bar plot of weight x residuals by year
p.mvol_smr.bar.wt.rs.yr <- 
  ggplot(data = df.mvol_smr,
         aes(x=as.numeric(as.character(df.mvol_smr$year)),
             y = Residual,
             title = "weight x residuals by year for summer flow volume (mvol_smr)")) + 
  xlab("year") + ylab("residual (ac-ft)") +
  geom_bar(stat = "identity", fill = "blue", position=position_dodge())
##plot(p.mvol_smr.bar.wt.rs.yr)

## get mvol_wtr
df.mvol_wtr <- data.frame(
  year = factor(
    unique(
      format(df.mlog$dates[
        grep("winter", as.character(df.mlog$season))], "%Y"))), 
  df.res.org[grep("mvol_wtr", as.character(df.res.org$Group)), ])
df.mvol_wtr[, 4:12] <- sapply(df.mvol_wtr[ , 4:12], as.numeric)

## boxplot of weight x residuals
p.mvol_wtr.bar.wt.rs.all <- 
  ggplot(data = df.mvol_wtr, 
         aes(x=factor(0), y = Residual, 
             title = "weight x residuals for winter flow volume (mvol_wtr)")) +
  xlab("year") + ylab("residual (ac-ft)") + geom_boxplot()
##plot(p.mvol_wtr.bar.wt.rs.all)

## scatter plot of weight x residuals by year
# p.mvol_wtr.pnt.wt.rs.yr <- 
#   ggplot(data = df.mvol_wtr,
#          aes(x=as.numeric(as.character(df.mvol_wtr$year)),
#              y = Residual,
#              title = "weight x residuals by year for winter flow volume (mvol_wtr)")) + 
#   xlab("year") + ylab("residual (ac-ft)") + geom_point(shape = 1, size = 4)
# plot(p.mvol_wtr.pnt.wt.rs.yr)

## bar plot of weight x residuals by year
p.mvol_wtr.bar.wt.rs.yr <- 
  ggplot(data = df.mvol_wtr,
         aes(x=as.numeric(as.character(df.mvol_wtr$year)),
             y = Residual,
             title = "weight x residuals by year for winter flow volume (mvol_wtr)")) +
  xlab("year") + ylab("residual (ac-ft)") +
  geom_bar(stat = "identity", fill = "blue", position=position_dodge())
##plot(p.mvol_wtr.bar.wt.rs.yr)

## summary table for ann, smr and wtr
df.sum.mvols <- data.frame(cast(rbind(df.mvol_ann[, c("year", "Group", "WeightxResidual")],
           df.mvol_wtr[, c("year", "Group", "WeightxResidual")],
           df.mvol_smr[, c("year", "Group", "WeightxResidual")]),
     year ~ Group, value = "WeightxResidual"))

## get storms
## storm information
## get storm dates from text file Information in this file from 
## Select_Storm_HydCal repo
## column 2 is the begin date of storm and column 8 is the end date of storm
df.strm.dates.raw <- read.delim(file = paste0(chr.dir.stm, "/dates_stm.dat"),
                                header = FALSE, sep = " ", 
                                stringsAsFactors = FALSE)[ , c(2, 8)]
## convert to POSIXct dates
df.strm.dates <- data.frame(apply(df.strm.dates.raw, MARGIN = 2, strptime, 
                                  format = "%m/%d/%Y"))
## set names
names(df.strm.dates) <- c("begin", "end")

## peaks
df.storms.peak <- df.res.org[ df.res.org$Group == "mpeak", ]
df.storms.peak[ ,3:11] <- sapply(df.storms.peak[ ,3:11],as.numeric)

## combine dates with storms
df.storms.peak <- cbind(df.strm.dates, df.storms.peak)
## add year from year storm starts
df.storms.peak <- cbind(df.storms.peak, 
                        year = factor(format(df.storms.peak$begin, "%Y")))
## add month from month storm starts
df.storms.peak <- cbind(df.storms.peak, 
                   month = 
                     factor(
                       format(df.storms.peak$begin, "%b"),
                       levels = c("Oct", "Nov", "Dec", "Jan",
                                  "Feb", "Mar","Apr", "May","Jun", 
                                  "Jul", "Aug", "Sep")))
## add season from month storm starts
df.storms.peak <- cbind(df.storms.peak, 
                   season = factor(
                     sapply(as.character(df.storms.peak$month), chr_season)))
## add flow regime for peak flow
mflow.ecdf.mod <- ecdf(df.mflow$Modelled)
mflow.ecdf.obs <- ecdf(df.mflow$Measured)
df.storms.peak <- cbind(df.storms.peak, 
                        mod.exceed = 
                          100 * ( 1 - round(
                            mflow.ecdf.mod(
                              df.storms.peak$Modelled),3)),
                        obs.exceed = 
                          100 * ( 1 - round(
                            mflow.ecdf.obs(
                              df.storms.peak$Measured),3))
                        )
df.storms.peak <- cbind(df.storms.peak, 
                        mod.flw.zn = factor(
                          sapply(df.storms.peak$mod.exceed,get.flow.zone),
                          levels = c("dry", "low", "typical", 
                                     "transitional", "high")),
                        obs.flw.zn = factor(
                          sapply(df.storms.peak$obs.exceed,get.flow.zone),
                          levels = c("dry", "low", "typical", 
                                     "transitional", "high"))
                        
                        )
## volumes
df.storms.vol <- df.res.org[ df.res.org$Group == "mvol_stm", ]
df.storms.vol[ ,3:11] <- sapply(df.storms.vol[ ,3:11],as.numeric)
## combine dates with storms
df.storms.vol <- cbind(df.strm.dates, df.storms.vol)
## add info from peaks data.frame
df.storms.vol <- cbind(df.storms.vol, df.storms.peak[ , 14:20])

## get precip, obs and model flow for each storms and make plots as in 
## the Storm_Select script

# get precip data
source(file=paste0(chr.dir.stm,"/devel/get-precip-data.R"),
       chdir = TRUE, local = TRUE)
# use max precip between the two gages (src) for daily precip
tmp.daily.precip.max.stations <- 
  summaryBy(prec.sum ~ date_org, df.daily.precip, FUN = max)

# get precip for dates in flow ts
df.daily.precip <- tmp.daily.precip.max.stations[do.call(rbind,lapply(strftime(df.mflow$dates, format = "%Y%m%d"), grep, strftime(tmp.daily.precip.max.stations$date_org, format = "%Y%m%d"))), ]
names(df.daily.precip) <- c("dates", "daily.precip")

# baseflow seperation using USGS-HySep R version
df.hysep88.8.obs <- hysep(Flow = df.mflow$Measured, 
                      Dates = as.Date(df.mflow$dates), da = 88.8)
# baseflow seperation using USGS-HySep R version
df.hysep88.8.mod <- hysep(Flow = df.mflow$Modelled, 
                          Dates = as.Date(df.mflow$dates), da = 88.8)

## plot storm info to list
p.storms <- list()
for(pp in 1:length(df.storms.peak[ , 1])) {
  p.storms[[pp]] <- storm_plot(
    lng.stm = pp,
    dte.stms  = df.storms.peak[ , c("begin", "end")],
    dte.flows = df.mflow$dates,
    obs.flow  = df.mflow$Measured,
    mod.flow = df.mflow$Modelled,
    obs.bflow = df.hysep88.8.obs$BaseQ,
    mod.bflow = df.hysep88.8.mod$BaseQ,
    storms.peak = df.storms.peak,
    storms.vol = df.storms.vol,
    precip = df.daily.precip$daily.precip)
  }

                   

##
##
## write tables and plots to pdf file
chr.dir <- "m:/models/bacteria/hspf/bigelkhydrocal201601"
pdf(file = paste0(chr.dir, "/pest-hspf-hydcal-results-", 
       strftime(Sys.time(), format = "%Y%m%d%H%M"), ".pdf"),
    height = 8.5, width = 11, onefile = TRUE)

##
## mlog
## mlog weighted residuals for entire period
tmp.table <- tableGrob(df.sum.mlog.res.org, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "mlog weighted residuals for entire period",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
plot(p.mlog.bar.wt.rs.all)
grid.newpage()
rm(list=ls(patter="^tmp\\."))

## mlog weighted residuals by year
tmp.table <- tableGrob(df.sum.by.year.mlog.res, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "mlog weighted residuals by year",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
plot(p.mlog.bar.wt.rs.yr)
grid.newpage()
rm(list=ls(patter="^tmp\\."))

## mlog weighted residuals by season
tmp.table <- tableGrob(df.sum.by.year.mlog.res.sn, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "mlog weighted residuals by season",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
plot(p.mlog.bar.wt.rs.sn)
grid.newpage()
rm(list=ls(patter="^tmp\\."))

## mlog weighted residuals by flow zone
tmp.table <- tableGrob(df.sum.by.year.mlog.res.fz, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "mlog weighted residuals by flow zone",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
plot(p.mlog.bar.wt.rs.fz)
grid.newpage()
rm(list=ls(patter="^tmp\\."))

##
## mflow
## mflow weighted residuals for entire period
tmp.table <- tableGrob(df.sum.mflow.res, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "mflow weighted residuals for entire period",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
plot(p.mflow.bar.wt.rs.all)
grid.newpage()
rm(list=ls(patter="^tmp\\."))

## mflow weighted residuals by year
tmp.table <- tableGrob(df.sum.by.year.mflow.res.yr, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "mflow weighted residuals by year",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
plot(p.mflow.bar.wt.rs.yr)
grid.newpage()
rm(list=ls(patter="^tmp\\."))

## mflow weighted residuals by season
tmp.table <- tableGrob(df.sum.by.year.mflow.res.sn, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "mflow weighted residuals by season",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
plot(p.mflow.bar.wt.rs.sn)
grid.newpage()
rm(list=ls(patter="^tmp\\."))

## mflow weighted residuals by flow zone
tmp.table <- tableGrob(df.sum.by.year.mflow.res.fz, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "mflow weighted residuals by flow zone",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
plot(p.mflow.bar.wt.rs.fz)
grid.newpage()
rm(list=ls(patter="^tmp\\."))

## mvol_ann, mvol_smr and mvol_wtr weighted residuals by year
tmp.table <- tableGrob(df.sum.mvols, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "mvols weighted residuals by year",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
plot(p.mvol_ann.bar.wt.rs.all)
plot(p.mvol_ann.bar.wt.rs.yr)
plot(p.mvol_smr.bar.wt.rs.all)
plot(p.mvol_smr.bar.wt.rs.yr)
plot(p.mvol_wtr.bar.wt.rs.all)
plot(p.mvol_wtr.bar.wt.rs.yr)
grid.newpage()
rm(list=ls(patter="^tmp\\."))

## mtime
tmp.table <- tableGrob(format(data.frame(df.sum.mtime), digits = 2, 
                              big.mark = ","), show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "mtime model, obs and USGS eq results",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.draw(tmp.gt)
plot(p.mtime00)
rm(list=ls(patter="^tmp\\."))

## storms

## storm peaks
maxrow = 20; 
npages = ceiling(nrow(df.storms.peak)/maxrow); 
for (i in 1:npages) {
  idx = seq(1+((i-1)*maxrow), i*maxrow)
  idx <- idx[idx <= nrow(df.storms.peak)]
  grid.newpage()
  
  tmp.table <- tableGrob(
    df.storms.peak[idx, c("begin", "end", "Measured", "Modelled", 
                         "WeightxResidual", "season", "obs.exceed", "mod.exceed",
                         "mod.flw.zn", "obs.flw.zn")], show.rownames = FALSE,
    gpar.coretext =gpar(fontsize=10),
    gpar.coltext=gpar(fontsize=10, fontface='bold'),
    y = unit(0.1, "npc"),
    vjust = 2
  )
  tmp.h <- grobHeight(tmp.table)
  tmp.w <- grobWidth(tmp.table)
  if(i == 1) {
    tmp.label <- "storm peaks model and obs"  
  } else {
    tmp.label <- "storm peaks model and obs (continued)"
  }
  tmp.title <- textGrob(label = tmp.label,
                        y=unit(0.5,"npc") + 0.5*tmp.h, 
                        vjust=0, gp=gpar(fontsize=20))
  tmp.gt <- gTree(children=gList(tmp.table, tmp.title)) 
  grid.draw(tmp.gt)
}
rm(list=ls(patter="^tmp\\."))


## storm vols
maxrow = 20; 
npages = ceiling(nrow(df.storms.vol)/maxrow); 
for (i in 1:npages) {
  idx = seq(1+((i-1)*maxrow), i*maxrow)
  idx <- idx[idx <= nrow(df.storms.vol)]
  grid.newpage()

  tmp.table <- tableGrob(
    df.storms.vol[idx, c("begin", "end", "Measured", "Modelled", 
                      "WeightxResidual", "season", "obs.exceed", "mod.exceed",
                      "mod.flw.zn", "obs.flw.zn")], show.rownames = FALSE,
    gpar.coretext =gpar(fontsize=10),
    gpar.coltext=gpar(fontsize=10, fontface='bold'),
    y = unit(0.1, "npc"),
    vjust = 2
  )
  tmp.h <- grobHeight(tmp.table)
  tmp.w <- grobWidth(tmp.table)
  if(i == 1) {
    tmp.label <- "storm vols model and obs"  
  } else {
    tmp.label <- "storm vols model and obs (continued)"
  }
  tmp.title <- textGrob(label = tmp.label,
                        y=unit(0.5,"npc") + 0.5*tmp.h, 
                        vjust=0, gp=gpar(fontsize=20))
  tmp.gt <- gTree(children=gList(tmp.table, tmp.title)) 
  grid.draw(tmp.gt)
  
  }
rm(list=ls(patter="^tmp\\."))

for(jj in 1:length(p.storms)) {
  storm_grid(p.storms[[jj]])
}

## calibration results HSPEXP table
df.hspexp <- data.frame(flow.stat = c("Total Volume (ac-ft)",
                                      "Summer Volume (ac-ft)",
                                      "winter Volume (ac-ft)",
                                      "Storm Volume (ac-ft)",
                                      "Mean Storm Peak (cfs)",
                                      "Baseflow Index"),
                        obs = c(sum(df.mvol_ann$Measured),
                                sum(df.mvol_smr$Measured),
                                sum(df.mvol_wtr$Measured),
                                sum(df.storms.vol$Measured),
                                mean(df.storms.peak$Measured),
                                sum(df.hysep88.8.obs$BaseQ)/sum(df.hysep88.8.obs$Flow)),
                        mod = c(sum(df.mvol_ann$Modelled),
                                sum(df.mvol_smr$Modelled),
                                sum(df.mvol_wtr$Modelled),
                                sum(df.storms.vol$Modelled),
                                mean(df.storms.peak$Modelled),
                                sum(df.hysep88.8.mod$BaseQ)/sum(df.hysep88.8.mod$Flow)))
df.hspexp <- data.frame(df.hspexp, 
                        percent.err = 
                          round(100 * with(df.hspexp, (mod - obs) / obs), 2))
tmp.table <- tableGrob(df.hspexp, show.rownames = FALSE)
tmp.h <- grobHeight(tmp.table)
tmp.w <- grobWidth(tmp.table)
tmp.title <- textGrob(label = "HSPEXP-like Statistics",
                      y=unit(0.5,"npc") + 0.5*tmp.h, 
                      vjust=0, gp=gpar(fontsize=20))
tmp.gt <- gTree(children=gList(tmp.table, tmp.title))
grid.newpage()
grid.draw(tmp.gt)

dev.off()

