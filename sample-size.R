#!/usr/bin/Rscript

library(hash)

t <- read.csv("res.1/summary.csv", header = T)
t <- t[with(t, order(consist, loc, delay, delay_var)), ]

# http://www.rit.edu/~w-uphysi/uncertainties/Uncertaintiespart2.html
# http://www.fas.harvard.edu/~scphys/courses/ps2/2007/errprop.pdf
op.mean <- (2 * t$get + t$upd) / 3  # ratio = 2:1
op.sdev <- ((2 * t$get_sdev / 3) ^ 2 + (t$upd_sdev / 3) ^ 2) ^ 0.5

z.90 <- 1.645 # 90%
z.95 <- 1.960 # 95%
z.99 <- 2.576 # 99%
r <- 1
sample.n <- ((100 * z.99 * op.mean) / (r * op.sdev)) ^ 2
enough <- (t$get_n + t$upd_n) > sample.n

t <- cbind(t, op.mean, op.sdev, sample.n, enough)

print(subset(t, enough == FALSE))
