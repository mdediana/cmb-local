#!/usr/bin/Rscript

t1 <- read.table("warmup_0.5_uni.csv", sep = ",")
t2 <- read.table("warmup_0.9_uni.csv", sep = ",")
t3 <- read.table("warmup_0.5_par.csv", sep = ",")
t4 <- read.table("warmup_0.9_par.csv", sep = ",")

ltys = c("solid", "dashed", "dotted", "dotdash")
cols = c("black", "blue", "red", "green")

#pdf("warmup.pdf")
png("warmup.png", width = 640, height = 640)
plot(t1, xlab = "run", ylab = "migs", type = "o", lty = ltys[1],
     ylim = c(0, 30000), cex = 0, col = cols[1])
lines(t2, lty = ltys[2], col = cols[2])
lines(t3, lty = ltys[3], col = cols[3])
lines(t4, lty = ltys[4], col = cols[4])
legend(15, 20000, c("0.5 uni", "0.9 uni", "0.5 par", "09 par"),
       lty = ltys, col = cols)
dev.off()
