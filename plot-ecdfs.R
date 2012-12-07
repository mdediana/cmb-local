#!/usr/bin/Rscript

library(hash)

t <- read.table("res/percentiles.csv", header = TRUE, sep = ",")
t <- t[with(t, order(consist, r_w, loc, pop, delay)), ]

for (c in levels(t$consist)) {
  for (o in levels(t$op)) {
    fname <- paste(paste("hist", c, o, sep = "_"), "png", sep = ".")
    png(fname, width = 1200, height = 1500)
    par(mfrow = c(4, 4))
 
    apply(subset(t, consist == c & op == o),
          1,
          function(row) {
            gname <- paste(row[2], row[3], row[4], row[5], row[6])

            s <- row[-c(1:6)]
            s <- as.numeric(s) / 1e3	# us -> ms

            plot.ecdf(data.matrix(s), main = gname, cex = 0.2, col = "red",
                      xlim = c(0, 1.5e3), xlab = paste(o, " (ms)"))
            #hist(t(s)[, 1], main = gname, col = "lightblue")
        })
    graphics.off()
  }
}
