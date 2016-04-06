## make tables and charts to comapre org and upd hyd cal


## paths
chr.hydcal.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM"


## get residuals for upd cal
df.res.upd <- read.table(file = paste0(chr.hydcal.dir,
                              "/pest-hspf-files/upd-uncert/calib.res"),
                     header = TRUE)

## get residuals for org cal
df.res.org <- read.table(file = paste0(chr.hydcal.dir,
                                       "/org-calib/Final_Deliverables_EPA_July2012/PEST_end/calib.res"),
                         header = TRUE)
