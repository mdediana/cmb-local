#!/usr/bin/Rscript

T1 <- read.table("res.1.csv", header = TRUE, sep = ",")
T2 <- read.table("res.2.csv", header = TRUE, sep = ",")
T3 <- read.table("res.3.csv", header = TRUE, sep = ",")

ops_s <- apply(cbind(T1$ops_s, T2$ops_s, T3$ops_s), 1, mean)
get <- apply(cbind(T1$get, T2$get, T3$get), 1, mean)
upd <- apply(cbind(T1$upd, T2$upd, T3$upd), 1, mean)
confl <- apply(cbind(T1$confl, T2$confl, T3$confl), 1, mean)
mig <- apply(cbind(T1$mig, T2$mig, T3$mig), 1, mean)
T <- cbind(T1[1:5], ops_s, get, upd, confl, mig)

CS <- levels(T$consist)
RS <- levels(T$r_w)
LS <- unique(T1$loc)
PS <- levels(T$pop)

EXP <- function(x, a, b) { a * exp(b * x) }

plot_fit_ops_s <- function(t) {
  fm <- nls(ops_s ~ EXP(delay, a, b), t, c(a = 30000, b = -0.005))
  curve(EXP(x, coef(fm)[1], coef(fm)[2]), add = TRUE, col = "red")
}

plot_fit_upd <- function(t) {
  fm <- lm(upd ~ delay, t)
  curve(coef(fm)[1] + coef(fm)[2] * x, add = TRUE, col = "red")
}

plot_metric <- function(t, ycolname, xcolname, fitfun = NULL) {
  for (c in CS) {
    fname <- paste(paste(ycolname, c, sep = "_"), "png", sep = ".")
    png(fname, width = 1200, height = 1500)

    par(mfrow = c(5, 4))
    for (r in RS) {
      for (l in LS) {
        for (p in PS) {
          s <- subset(t, consist == c & r_w == r & loc == l & pop == p)

          gname <- paste(r, l, p)
          plot(s[[ycolname]] ~ s[[xcolname]], data = s, main = gname, xlab = xcolname, ylab = ycolname)

          if (!is.null(fitfun)) { fitfun(s) }
        }
      }
    }
  }
  graphics.off()
}

plot_metric(T, "ops_s", "delay", plot_fit_ops_s)
plot_metric(T, "get", "delay")
plot_metric(T, "upd", "delay", plot_fit_upd)
plot_metric(T, "confl", "delay")
plot_metric(T, "mig", "delay")
