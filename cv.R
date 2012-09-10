#!/usr/bin/Rscript

t1 <- read.table("res.1.csv", header=TRUE, sep=",")
t2 <- read.table("res.2.csv", header=TRUE, sep=",")
t3 <- read.table("res.3.csv", header=TRUE, sep=",")

# Coefficients of variation
cv <- function(v) {
  m <- apply(v, 1, mean)
  sd <- apply(v, 1, sd)
  return(sd / m)
}

ops_s <- cbind(t1$ops_s, t2$ops_s, t3$ops_s)
get <- cbind(t1$get, t2$get, t3$get)
upd <- cbind(t1$upd, t2$upd, t3$upd)

ops_s_cv <- cv(ops_s)
get_cv <- cv(get)
upd_cv <- cv(upd)
t <- cbind(t1[1:5], ops_s_cv, get_cv, upd_cv)

options(echo=TRUE)
limits <- c(0.2, 0.3, 0.4, 0.5)
for (i in limits) { print(dim(t[t$ops_s_cv > i, ])) }
for (i in limits) { print(dim(t[t$get_cv > i, ])) }
for (i in limits) { print(dim(t[t$upd_cv > i, ])) }
