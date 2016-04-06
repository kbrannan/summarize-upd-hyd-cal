chr.bacteria.twg.17.dir <- "M:/Presentations/2016-02-09 Bacteria TWG 17"


df.tmp.org <- df.mtime.all.org[df.mtime.all.org$L1 != "obs", ]
df.tmp.org$L1 <- as.character(df.tmp.org$L1)
df.tmp.org[df.tmp.org$L1 == "mod", "L1"] <- "org"
df.mtime.all.upd
df.tmp.upd <- df.mtime.all.upd[df.mtime.all.upd$L1 == "mod", ]
df.tmp.upd$L1 <- as.character(df.tmp.upd$L1)
df.tmp.upd[df.tmp.upd$L1 == "mod", "L1"] <- "upd"

df.mtime.both <- rbind(df.tmp.org, df.tmp.upd)

df.mtime.both$L1 <- factor(df.mtime.both$L1, levels = c("org", "upd", "eq"))

## plot mtime and related data
p.mtime00.both <- ggplot(data = df.mtime.both,
                    title = "flow duration curves") + 
  scale_y_log10() + 
  scale_colour_manual(name = "", breaks = c("org", "upd", "eq"), 
                      labels = c("Org", "Upd", "USGS Eq"), 
                      values = c("blue", "green", "black")) +
  scale_shape_manual(name = "", breaks = c("org", "upd", "eq"), 
                     labels = c("Org", "Upd", "USGS Eq"),
                     values = c(21, 17, 15)) +
  xlab("Percent Time Greater") + ylab("Mean Daily Flow (cfs)")

## add obs and mod flow data along with reg eq
p.mtime00.both <- p.mtime00.both + 
  geom_line(data = df.mtime.both[as.character(df.mtime.both$variable) == "y", ],
            aes(x = x, y = value, colour = L1))

p.mtime00.both <- p.mtime00.both + 
  geom_point(data = df.mtime.both[as.character(df.mtime.both$variable) == "y", ],
             aes(x = x, y = value, colour = L1, shape = L1), size = 4)

## add error bars from reg eq
p.mtime00.both <- p.mtime00.both + 
  geom_errorbar(data = df.mtime.both[df.mtime.both$L1 == "eq", ], 
                aes(x = df.mtime.both[df.mtime.both$L1 == "eq" & 
                                       as.character(df.mtime.both$variable) == "ymin", 'x'],
                    ymin = df.mtime.all.org[df.mtime.both$L1 == "eq" & 
                                          as.character(df.mtime.both$variable) == "ymin", 'value'],
                    ymax = df.mtime.all.org[df.mtime.both$L1 == "eq" & 
                                          as.character(df.mtime.both$variable) == "ymax", "value"]
                ))
## plot fdc
png(file = paste0(chr.bacteria.twg.17.dir, "/fdc-org-upd.png"), 
    width = 480, height = 480, units = "px", pointsize = 12,
    bg = "white", res = NA, family = "", restoreConsole = TRUE)
plot(p.mtime00.both)
dev.off()


