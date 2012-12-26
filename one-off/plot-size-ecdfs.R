#!/usr/bin/Rscript

library(hash)

t <- read.table("res/percentiles.csv", header = TRUE, sep = ",")
t <- t[with(t, order(servers, clients, conc, total_keys)), ]

for (o in levels(t$op)) {
  fname <- paste(paste("ecdf", o, sep = "_"), "png", sep = ".")
  png(fname, width = 1200, height = 1500)
  par(mfrow = c(4, 4))

  apply(subset(t, op == o),
        1,
        function(row) {
          gname <- paste(row[11], row[12], row[14], row[13])

          s <- as.numeric(row[-c(1:15)])

          plot.ecdf(data.matrix(s), main = gname, cex = 0.2, col = "red",
                    #xlim = c(0, 1.5), xlab = paste(o, " (s)"))
                    xlim = c(0, 5), xlab = paste(o, " (s)"))
          delay = as.numeric(row[5]) / 1e3	# ms -> s
          abline(v = delay, lty = "dotted")
          abline(h = row[3], lty = "dotted")
      })
  graphics.off()
}
