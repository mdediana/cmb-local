#!/usr/bin/Rscript

dn <- "/home/mdediana/workspace/exp-data/warmup"
total.keys = 20000
ltys = c("solid", "dashed", "dotted", "dotdash")
cols = c("black", "blue", "red", "green")

Load <- function(fn) {
  t <- read.table(paste(dn, fn, sep = '/'), sep = ',')
  t$V1 <- t$V1 * total.keys / 1e3
  t$V2 = t$V2 / 1e3
  t
}

t1 <- Load("warmup_0.5_uni.csv")
t2 <- Load("warmup_0.9_uni.csv")
t3 <- Load("warmup_0.5_par.csv")
t4 <- Load("warmup_0.9_par.csv")

#pdf("warmup.pdf")
png("warmup.png", width = 640, height = 640)

plot(t1, main = "Aquecimento",
     xlab = "Escritas (x 1000)", ylab = "Migrações (x 1000)",
     ylim = c(0, 2.2), type = "o", cex = 0, lwd = 1.5,
     lty = ltys[1], col = cols[1])
lines(t2, lty = ltys[2], lwd = 1.5, col = cols[2])
lines(t3, lty = ltys[3], lwd = 1.5, col = cols[3])
lines(t4, lty = ltys[4], lwd = 1.5, col = cols[4])
abline(v = 60, lty = "dotted")
abline(v = 340, lty = "dotted")
legend(400, 2.1, c("50% | uniforme", "90% | uniforme", "50% | concentrada",
       "90% | concentrada"),
       lty = ltys, col = cols)
dev.off()
