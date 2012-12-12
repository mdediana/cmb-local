#!/usr/bin/Rscript

library(hash)

t <- read.table("res/percentiles.csv", header = TRUE, sep = ",")
t <- t[with(t, order(consist, r_w, loc, pop, delay)), ]

for (c in levels(t$consist)) {
  for (o in levels(t$op)) {
    fname <- paste(paste("ecdf", c, o, sep = "_"), "png", sep = ".")
    png(fname, width = 1200, height = 1500)
    par(mfrow = c(4, 4))
 
    apply(subset(t, consist == c & op == o),
          1,
          function(row) {
            gname <- paste(row[2], row[3], row[4], row[5], row[6])

            s <- as.numeric(row[-c(1:6)])

            plot.ecdf(data.matrix(s), main = gname, cex = 0.2, col = "red",
                      xlim = c(0, 1.5), xlab = paste(o, " (s)"))
            delay = as.numeric(row[5]) / 1e3	# ms -> s
            abline(v = delay, lty = "dotted")
            abline(h = row[3], lty = "dotted")
            #hist(t(s)[, 1], main = gname, col = "lightblue")
        })
    graphics.off()
  }
}
