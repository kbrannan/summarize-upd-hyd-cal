## make tables and charts to comapre org and upd hyd cal


## paths
chr.hydcal.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"


## get data for upd cal
df.res.upd <- read.table(file = paste0(chr.hydcal.dir,
                              "/pest-hspf-files/upd-uncert/calib.res"),
                     header = TRUE)
df.res.upd[, 10] <- as.numeric(df.res.upd[, 10]) ## convert from factor to numeric
df.res.upd[, 11] <- as.numeric(df.res.upd[, 11]) ## convert from factor to numeric
chr.tmp <- scan(file = paste0(chr.hydcal.dir,
                                       "/pest-hspf-files/upd-uncert/dates_stm.dat"),
                         what = "character", sep = "\n")
gsub("( ){1,}24:00:00$", "",gsub("(( ){1,}00:00:00( ){1,})"," ", chr.tmp))


## get residuals for org cal
df.res.org <- read.table(file = paste0(chr.hydcal.dir,
                                       "/org-calib/Final_Deliverables_EPA_July2012/PEST_end/calib.res"),
                         header = TRUE)
df.res.org[, 10] <- as.numeric(df.res.org[, 10]) ## convert from factor to numeric
df.res.org[, 11] <- as.numeric(df.res.org[, 11]) ## convert from factor to numeric

## 