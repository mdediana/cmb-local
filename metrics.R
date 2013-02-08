#!/usr/bin/Rscript

library(hash)

t1 <- read.csv("res.1/summary.csv", header = T)
t2 <- read.csv("res.2/summary.csv", header = T)
t1 <- t1[with(t1, order(consist, loc, delay, delay_var)), ]
t2 <- t2[with(t2, order(consist, loc, delay, delay_var)), ]

mig <- apply(cbind(t1$mig, t2$mig), 1, mean)

gets <- cbind(t1$get, t2$get)
upds <- cbind(t1$upd, t2$upd)

gets.mean <- apply(gets, 1, mean)
gets.sd <- apply(gets, 1, sd)
gets.cv <- gets.sd / gets.mean
upds.mean <- apply(upds, 1, mean)
upds.sd <- apply(upds, 1, sd)
upds.cv <- upds.sd / upds.mean

cat('max(gets.cv) =', max(gets.cv), '\n')
cat('max(upds.cv) =', max(upds.cv), '\n')
cat('mean(gets.cv) =', mean(gets.cv), '\n')
cat('mean(upds.cv) =', mean(upds.cv), '\n')
