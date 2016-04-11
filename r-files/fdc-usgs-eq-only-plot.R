## load packages
library(ggplot2, quietly = TRUE)

## path for output
chr.bacteria.twg.17.dir <- "M:/Presentations/2016-02-09 Bacteria TWG 17"

## main path for uncert re-reun
chr.uncert.rerun.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert/uncert-rerun"

## get names of runs selected
load(paste0(chr.uncert.rerun.dir, "/uncert-run-kept-list.RData"))

## plot fdc of USGS eq
## plot mtime and related data
p.mtime00.upd <- ggplot(data = df.fdc.ss.est,
                        title = "USGS Eq flow duration curve") + 
  scale_y_log10()  + xlab("Percent Time Greater") + 
  ylab("Mean Daily Flow (cfs)")

## add obs and mod flow data along with reg eq
p.mtime00.upd <- p.mtime00.upd + 
  geom_line(aes(x = FDPercent, y = FDEst), colour = "black")

p.mtime00.upd <- p.mtime00.upd + 
  geom_point(aes(x = FDPercent, y = FDEst), 
             colour =" black", shape = 15, size = 4)

## add error bars from reg eq
p.mtime00.upd <- p.mtime00.upd + 
  geom_errorbar(aes(x = FDPercent, ymin = lower, ymax = upper))
## plot fdc
png(file = paste0(chr.bacteria.twg.17.dir, "/tables-charts-figures/fdc-usgs-eq-only.png"), 
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE)
plot(p.mtime00.upd)
dev.off()
