#!/usr/bin/Rscript

t1 <- read.table("check-migs-0_5-uni.log")
t2 <- read.table("check-migs-0_9-uni.log")
t3 <- read.table("check-migs-0_5-par.log")
t4 <- read.table("check-migs-0_9-par.log")

cols = c("black", "blue", "red", "green")
ltys = c("solid", "dashed", "dotted", "dotdash")

#pdf("warmup.pdf")
png("warmup.png", width = 640, height = 640)
plot(t1, xlab = "t (em s)", ylab = "# de migrações",  type = "o", lty = ltys[1], cex = 0, col = cols[1])
lines(t2, lty = ltys[2], col = cols[2])
lines(t3, lty = ltys[3], col = cols[3])
lines(t4, lty = ltys[4], col = cols[4])
legend(2000, 400000, c("0.5 uni", "0.9 uni", "0.5 par", "09 par"), lty = ltys, col = cols)
dev.off()
