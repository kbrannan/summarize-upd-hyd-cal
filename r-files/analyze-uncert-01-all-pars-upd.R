## load packages
library(ggplot2)

## main path for uncert
chr.uncert.dir <- "M:/Models/Bacteria/HSPF/Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/upd-uncert"

## main path for uncert-rerun
chr.uncert.rerun.dir <- paste0(chr.uncert.dir, "/uncert-rerun-01")

## save the pars to file
load(file = paste0(chr.uncert.rerun.dir, "/all-par-vals.RData"))

## add type factor
unique(gsub("[0-9]{1,}","",df.pars$run))


# plots

## agwrc
tmp.data <- df.pars[df.pars$par == "agwrc000" , ]
tmp.p  <- ggplot(data = tmp.data) + 
  xlab("") + ylab("Parameter Value")
tmp.p <- tmp.p + geom_boxplot(data = tmp.data[grep("uncert", tmp.data$run), ],
                                  aes(x = par, y = val))
tmp.p <- tmp.p + geom_point(data = tmp.data[grep("^cal$", tmp.data$run), ],
                                aes(x = par, y = val), shape = 1, size = 5,
                                colour = "red")
tmp.p <- tmp.p + 
  geom_errorbar(aes(x = tmp.data[grep("^cal$", tmp.data$run), "par"],
                    ymin = tmp.data[grep("^cal_lower$", tmp.data$run), "val"],
                    ymax = tmp.data[grep("^cal_upper$", tmp.data$run), "val"]),
                colour = "red")
p.agwrc <- tmp.p
plot(p.agwrc)
rm(tmp.p, tmp.data)

## irc
tmp.data <- df.pars[df.pars$par == "irc000" , ]
tmp.p  <- ggplot(data = tmp.data) + 
  xlab("") + ylab("Parameter Value")
tmp.p <- tmp.p + geom_boxplot(data = tmp.data[grep("uncert", tmp.data$run), ],
                              aes(x = par, y = val))
tmp.p <- tmp.p + geom_point(data = tmp.data[grep("^cal$", tmp.data$run), ],
                            aes(x = par, y = val), shape = 1, size = 5,
                            colour = "red")
tmp.p <- tmp.p + 
  geom_errorbar(aes(x = tmp.data[grep("^cal$", tmp.data$run), "par"],
                    ymin = tmp.data[grep("^cal_lower$", tmp.data$run), "val"],
                    ymax = tmp.data[grep("^cal_upper$", tmp.data$run), "val"]),
                colour = "red")
p.irc <- tmp.p
plot(p.irc)
rm(tmp.p, tmp.data)

## lzsn
tmp.data <- df.pars[df.pars$par == "lzsn000" , ]
tmp.p  <- ggplot(data = tmp.data) + 
  xlab("") + ylab("Parameter Value")
tmp.p <- tmp.p + geom_boxplot(data = tmp.data[grep("uncert", tmp.data$run), ],
                              aes(x = par, y = val))
tmp.p <- tmp.p + geom_point(data = tmp.data[grep("^cal$", tmp.data$run), ],
                            aes(x = par, y = val), shape = 1, size = 5,
                            colour = "red")
tmp.p <- tmp.p + 
  geom_errorbar(aes(x = tmp.data[grep("^cal$", tmp.data$run), "par"],
                    ymin = tmp.data[grep("^cal_lower$", tmp.data$run), "val"],
                    ymax = tmp.data[grep("^cal_upper$", tmp.data$run), "val"]),
                colour = "red")
p.lzsn <- tmp.p
plot(p.lzsn)
rm(tmp.p, tmp.data)

## deepfr
tmp.data <- df.pars[df.pars$par == "deepfr000" , ]
tmp.p  <- ggplot(data = tmp.data) + 
  xlab("") + ylab("Parameter Value")
tmp.p <- tmp.p + geom_boxplot(data = tmp.data[grep("uncert", tmp.data$run), ],
                              aes(x = par, y = val))
tmp.p <- tmp.p + geom_point(data = tmp.data[grep("^cal$", tmp.data$run), ],
                            aes(x = par, y = val), shape = 1, size = 5,
                            colour = "red")
tmp.p <- tmp.p + 
  geom_errorbar(aes(x = tmp.data[grep("^cal$", tmp.data$run), "par"],
                    ymin = tmp.data[grep("^cal_lower$", tmp.data$run), "val"],
                    ymax = tmp.data[grep("^cal_upper$", tmp.data$run), "val"]),
                colour = "red")
p.deepfr <- tmp.p
plot(p.deepfr)
rm(tmp.p, tmp.data)

## basetp
tmp.data <- df.pars[df.pars$par == "basetp000" , ]
tmp.p  <- ggplot(data = tmp.data) + 
  xlab("") + ylab("Parameter Value")
tmp.p <- tmp.p + geom_boxplot(data = tmp.data[grep("uncert", tmp.data$run), ],
                              aes(x = par, y = val))
tmp.p <- tmp.p + geom_point(data = tmp.data[grep("^cal$", tmp.data$run), ],
                            aes(x = par, y = val), shape = 1, size = 5,
                            colour = "red")
tmp.p <- tmp.p + 
  geom_errorbar(aes(x = tmp.data[grep("^cal$", tmp.data$run), "par"],
                    ymin = tmp.data[grep("^cal_lower$", tmp.data$run), "val"],
                    ymax = tmp.data[grep("^cal_upper$", tmp.data$run), "val"]),
                colour = "red")
p.basetp <- tmp.p
plot(p.basetp)
rm(tmp.p, tmp.data)

## agwetp
tmp.data <- df.pars[df.pars$par == "agwetp000" , ]
tmp.p  <- ggplot(data = tmp.data) + 
  xlab("") + ylab("Parameter Value")
tmp.p <- tmp.p + geom_boxplot(data = tmp.data[grep("uncert", tmp.data$run), ],
                              aes(x = par, y = val))
tmp.p <- tmp.p + geom_point(data = tmp.data[grep("^cal$", tmp.data$run), ],
                            aes(x = par, y = val), shape = 1, size = 5,
                            colour = "red")
tmp.p <- tmp.p + 
  geom_errorbar(aes(x = tmp.data[grep("^cal$", tmp.data$run), "par"],
                    ymin = tmp.data[grep("^cal_lower$", tmp.data$run), "val"],
                    ymax = tmp.data[grep("^cal_upper$", tmp.data$run), "val"]),
                colour = "red")
p.agwetp <- tmp.p
plot(p.agwetp)
rm(tmp.p, tmp.data)


## infilt
tmp.data <- df.pars[grep("^infilt", df.pars$par) , ]
tmp.p  <- ggplot(data = tmp.data) + 
  xlab("") + ylab("Parameter Value")
tmp.p <- tmp.p + geom_boxplot(data = tmp.data[grep("uncert", tmp.data$run), ],
                              aes(x = par, y = val))
tmp.p <- tmp.p + geom_point(data = tmp.data[grep("^cal$", tmp.data$run), ],
                            aes(x = par, y = val), shape = 1, size = 5,
                            colour = "red")
tmp.p <- tmp.p + 
  geom_errorbar(aes(x = tmp.data[grep("^cal$", tmp.data$run), "par"],
                    ymin = tmp.data[grep("^cal_lower$", tmp.data$run), "val"],
                    ymax = tmp.data[grep("^cal_upper$", tmp.data$run), "val"]),
                colour = "red")
p.infilt <- tmp.p
plot(p.infilt)
rm(tmp.p, tmp.data)


## uzsn
tmp.data <- df.pars[grep("^uzsn", df.pars$par) , ]
tmp.p  <- ggplot(data = tmp.data) + 
  xlab("") + ylab("Parameter Value")
tmp.p <- tmp.p + geom_boxplot(data = tmp.data[grep("uncert", tmp.data$run), ],
                              aes(x = par, y = val))
tmp.p <- tmp.p + geom_point(data = tmp.data[grep("^cal$", tmp.data$run), ],
                            aes(x = par, y = val), shape = 1, size = 5,
                            colour = "red")
tmp.p <- tmp.p + 
  geom_errorbar(aes(x = tmp.data[grep("^cal$", tmp.data$run), "par"],
                    ymin = tmp.data[grep("^cal_lower$", tmp.data$run), "val"],
                    ymax = tmp.data[grep("^cal_upper$", tmp.data$run), "val"]),
                colour = "red")
p.uzsn <- tmp.p
plot(p.uzsn)
rm(tmp.p, tmp.data)

## intfw
tmp.data <- df.pars[grep("^intfw", df.pars$par) , ]
tmp.p  <- ggplot(data = tmp.data) + 
  xlab("") + ylab("Parameter Value")
tmp.p <- tmp.p + geom_boxplot(data = tmp.data[grep("uncert", tmp.data$run), ],
                              aes(x = par, y = val))
tmp.p <- tmp.p + geom_point(data = tmp.data[grep("^cal$", tmp.data$run), ],
                            aes(x = par, y = val), shape = 1, size = 5,
                            colour = "red")
tmp.p <- tmp.p + 
  geom_errorbar(aes(x = tmp.data[grep("^cal$", tmp.data$run), "par"],
                    ymin = tmp.data[grep("^cal_lower$", tmp.data$run), "val"],
                    ymax = tmp.data[grep("^cal_upper$", tmp.data$run), "val"]),
                colour = "red")
p.intfw <- tmp.p
plot(p.intfw)
rm(tmp.p, tmp.data)


## cepsc
tmp.data <- df.pars[grep("^cepsc", df.pars$par) , ]
tmp.p  <- ggplot(data = tmp.data) + 
  xlab("") + ylab("Parameter Value")
tmp.p <- tmp.p + geom_boxplot(data = tmp.data[grep("uncert", tmp.data$run), ],
                              aes(x = par, y = val))
tmp.p <- tmp.p + geom_point(data = tmp.data[grep("^cal$", tmp.data$run), ],
                            aes(x = par, y = val), shape = 1, size = 5,
                            colour = "red")
tmp.p <- tmp.p + 
  geom_errorbar(aes(x = tmp.data[grep("^cal$", tmp.data$run), "par"],
                    ymin = tmp.data[grep("^cal_lower$", tmp.data$run), "val"],
                    ymax = tmp.data[grep("^cal_upper$", tmp.data$run), "val"]),
                colour = "red")
p.cepsc <- tmp.p
plot(p.cepsc)
rm(tmp.p, tmp.data)

## lzetp
tmp.data <- df.pars[grep("^lzetp", df.pars$par) , ]
tmp.p  <- ggplot(data = tmp.data) + 
  xlab("") + ylab("Parameter Value")
tmp.p <- tmp.p + geom_boxplot(data = tmp.data[grep("uncert", tmp.data$run), ],
                              aes(x = par, y = val))
tmp.p <- tmp.p + geom_point(data = tmp.data[grep("^cal$", tmp.data$run), ],
                            aes(x = par, y = val), shape = 1, size = 5,
                            colour = "red")
tmp.p <- tmp.p + 
  geom_errorbar(aes(x = tmp.data[grep("^cal$", tmp.data$run), "par"],
                    ymin = tmp.data[grep("^cal_lower$", tmp.data$run), "val"],
                    ymax = tmp.data[grep("^cal_upper$", tmp.data$run), "val"]),
                colour = "red")
p.lzetp <- tmp.p
plot(p.lzetp)
rm(tmp.p, tmp.data)


## sub-surface
p.sub <- ggplot(data = df.pars[df.pars$proc == "sub" , ]) + 
  xlab("") + ylab("Parameter Value")


p.sub <- p.sub + geom_boxplot(data = df.pars[df.pars$proc == "sub" &
                                                 df.pars$run != "cal", ],
                                  aes(x = par, y = val))
p.sub <- p.sub + geom_point(data = df.pars[df.pars$proc == "sub" &
                                             df.pars$run == "cal", ],
                            aes(x = par, y = val), shape = 15, size = 5,
                            colour = "red")

plot(p.sub)

# water balance
p.wtb <- ggplot(data = df.pars[df.pars$proc == "wtb" , ]) + 
  xlab("") + ylab("Parameter Value")


p.wtb <- p.wtb + geom_boxplot(data = df.pars[df.pars$proc == "wtb" &
                                               df.pars$run != "cal", ],
                              aes(x = par, y = val))
p.wtb <- p.wtb + geom_point(data = df.pars[df.pars$proc == "wtb" &
                                             df.pars$run == "cal", ],
                            aes(x = par, y = val), shape = 15, size = 5,
                            colour = "red")

plot(p.wtb)

# Surface
p.sur <- ggplot(data = df.pars[df.pars$proc == "sur" , ]) + 
  xlab("") + ylab("Parameter Value")


p.sur <- p.sur + geom_boxplot(data = df.pars[df.pars$proc == "sur" &
                                               df.pars$run != "cal", ],
                              aes(x = par, y = val))
p.sur <- p.sur + geom_point(data = df.pars[df.pars$proc == "sur" &
                                             df.pars$run == "cal", ],
                            aes(x = par, y = val), shape = 15, size = 5,
                            colour = "red")

plot(p.sur)
