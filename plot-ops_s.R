#!/usr/bin/Rscript

t1 <- read.table("res.1.csv", header = TRUE, sep = ",")
t2 <- read.table("res.2.csv", header = TRUE, sep = ",")
t3 <- read.table("res.3.csv", header = TRUE, sep = ",")

ops_s <- apply(cbind(t1$ops_s, t2$ops_s, t3$ops_s), 1, mean)
get <- apply(cbind(t1$get, t2$get, t3$get), 1, mean)
upd <- apply(cbind(t1$upd, t2$upd, t3$upd), 1, mean)
t <- cbind(t1[1:5], ops_s, get, upd)

cs <- levels(t$consist)
rs <- levels(t$r_w)
ls <- unique(t1$loc)
ps <- levels(t$pop)

f <- function(x, a, b) { a * exp(-b * x) }

for (c in cs) {
  png(paste(c(c, "png"), collapse = "."), width = 1200, height = 1500)
  par(mfrow = c(5, 4))
  for (r in rs) {
    for (l in ls) {
      for (p in ps) {
        name <- paste(c(r, l, p), collapse = ' ')
        s <- subset(t, consist == c & r_w == r & loc == l & pop == p)
        fm <- nls(ops_s ~ f(delay, a, b), data = s, start = c(a = 30000, b = 0.005))
        plot(ops_s ~ delay, data = s, main = name)
        curve(f(x, coef(fm)[1], coef(fm)[2]), add = TRUE, col = "red")
      }
    }
  }
}
graphics.off()
