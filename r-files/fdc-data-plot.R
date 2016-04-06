

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
  df.res.upd[grep("mtime", as.character(df.res.upd$Group)), ])
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