#!/usr/bin/Rscript

dn <- "/home/mdediana/workspace/exp-data/warmup"
total.keys = 20000
ltys = c("solid", "dashed", "dotted", "dotdash")
cols = c("black", "blue", "red", "green")

Load <- function(fn) {
  t <- read.table(paste(dn, fn, sep = '/'), sep = ',')
  t$V2 = t$V2 / total.keys * 100
  t
}

t1 <- Load("warmup_0.5_uni.csv")
t2 <- Load("warmup_0.9_uni.csv")
t3 <- Load("warmup_0.5_par.csv")
t4 <- Load("warmup_0.9_par.csv")

#pdf("warmup.pdf")
png("warmup.png", width = 640, height = 640)

plot(t1, main = "Aquecimento",
     xlab = "Passos", ylab = "Migrações / total de objetos (em %)",
     ylim = c(0, 12), type = "o", cex = 0, lwd = 1.5,
     lty = ltys[1], col = cols[1])
lines(t2, lty = ltys[2], lwd = 1.5, col = cols[2])
lines(t3, lty = ltys[3], lwd = 1.5, col = cols[3])
lines(t4, lty = ltys[4], lwd = 1.5, col = cols[4])
legend(20, 11, c("50% | uniforme", "90% | uniforme", "50% | concentrada",
       "90% | concentrada"),
       lty = ltys, col = cols)
dev.off()
