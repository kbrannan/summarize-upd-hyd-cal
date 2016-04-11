## load packages
library(ggplot2, quietly = TRUE)

## path for output
chr.bacteria.twg.17.dir <- "M:/Presentations/2016-02-09 Bacteria TWG 17"

## main path for uncert re-reun
chr.uncert.rerun.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert/uncert-rerun"

## get names of runs selected
load(paste0(chr.uncert.rerun.dir, "/fdc-est.RData"))



10^ceiling(log10(max(df.fdcs$y)))


## plot fdc of USGS eq
## plot mtime and related data
p.mtime00.upd <- ggplot(data = df.fdcs,
                        title = "flow duration curves") + 
  scale_y_log10(limits = c(1,10^ceiling(log10(max(df.fdcs$y)))))  + 
  xlab("Percent Time Greater") + ylab("Mean Daily Flow (cfs)") +
  scale_colour_manual(name = "", breaks = c("eq", "obs", "calib", "uncert"), 
                      labels = c("USGS Eq", "Obs", "Calibration", "Uncertainty Runs"), 
                      values = c("black", "blue", "red", "gray")) +
  scale_shape_manual(name = "", breaks = c("eq", "obs", "calib", "uncert"), 
                     labels = c("USGS Eq", "Obs", "Calibration", "Uncertainty Runs"),
                     values = c(0, 1, 2, 20)) +
  scale_size_manual(name = "", breaks = c("eq", "obs", "calib", "uncert"), 
                     labels = c("USGS Eq", "Obs", "Calibration", "Uncertainty Runs"),
                     values = c(5, 5, 5, 3)) +
  xlab("Percent Time Greater") + ylab("Mean Daily Flow (cfs)")


## add obs and mod flow data along with reg eq

p.mtime00.upd <- p.mtime00.upd + 
  geom_point(data = df.fdcs[df.fdcs$par == "est", ],
             aes(x = (1 - x) * 100, y = y, colour = src,
                 shape = src, size = src))
p.mtime00.upd <- p.mtime00.upd + 
  geom_line(data = df.fdcs[df.fdcs$par == "est" &
                             df.fdcs$src != "uncert", ],
            aes(x = (1 - x) * 100, y = y, colour = src))


## add error bars from reg eq
p.mtime00.upd <- p.mtime00.upd + 
  geom_errorbar(data = df.fdcs[df.fdcs$src == "eq", ],
                 aes(x = (1 - df.fdcs[df.fdcs$src == "eq" &
                                 df.fdcs$par == "upper", "x"]) * 100,
                    ymin = df.fdcs[df.fdcs$src == "eq" &
                                   df.fdcs$par == "lower", "y"],
                    ymax = df.fdcs[df.fdcs$src == "eq" &
                                   df.fdcs$par == "upper", "y"]))
## plot fdc
png(file = paste0(chr.bacteria.twg.17.dir, "/tables-charts-figures/fdc-all.png"), 
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE)
plot(p.mtime00.upd)
dev.off()
