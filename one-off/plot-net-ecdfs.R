#!/usr/bin/Rscript

library(hash)

t <- read.table("res/percentiles.csv", header = TRUE, sep = ",")
t <- t[with(t, order(delay, delay_var, loss, dupl, reorder, congest)), ]

for (d in t$delay) {
  for (o in levels(t$op)) {
    fname <- paste(paste("ecdf", d, o, sep = "_"), "png", sep = ".")
    png(fname, width = 1200, height = 1500)
    par(mfrow = c(8, 8))
 
    apply(subset(t, delay == d & op == o),
          1,
          function(row) {
            gname <- paste(row[6], row[8], row[9], row[11], row[13])

            s <- as.numeric(row[-c(1:14)])

            plot.ecdf(data.matrix(s), main = gname, cex = 0.2, col = "red",
                      #xlim = c(0, 1.5), xlab = paste(o, " (s)"))
                      xlim = c(0, 5), xlab = paste(o, " (s)"))
            delay = as.numeric(row[5]) / 1e3	# ms -> s
            abline(v = delay, lty = "dotted")
            abline(h = row[3], lty = "dotted")
        })
    graphics.off()
  }
}
