#!/usr/bin/Rscript

# mem_max is a multiple of BDP, see definition in cmb/tune-tcp
t <- read.table("res/percentiles.csv", header = TRUE, sep = ",")
t <- t[with(t, order(op, delay, reorder, mem_max)), ]

fname <- "ecdf_bdp.png"
png(fname, width = 1200, height = 1500)
par(mfrow = c(4, 4))
 
apply(t, 1,
      function(row) {
        gname <- paste(row[15], row[5], row[11], row[14])

        s <- as.numeric(row[-c(1:15)])

        plot.ecdf(data.matrix(s), main = gname, cex = 0.2, col = "red",
                  xlim = c(0, 5), xlab = "(s)")
        delay = as.numeric(row[5]) / 1e3	# ms -> s
        abline(v = delay, lty = "dotted")
        abline(h = row[3], lty = "dotted")
    })
graphics.off()
