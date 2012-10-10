#!/usr/bin/Rscript

T1 <- read.table("res.1.csv", header = TRUE, sep = ",")
T2 <- read.table("res.2.csv", header = TRUE, sep = ",")
T3 <- read.table("res.3.csv", header = TRUE, sep = ",")

CS <- levels(T1$consist)
RS <- levels(T1$r_w)
LS <- unique(T1$loc)
PS <- levels(T1$pop)

EXP <- function(x, a, b) { a * exp(b * x) }

avgUsers <- function(t) {
  rw <- strsplit(RS[t$r_w], ":")
  r <- as.numeric(sapply(rw, "[[", 1))
  w <- as.numeric(sapply(rw, "[[", 2))
  t$ops_s * (r * t$get + w * t$upd) # little's law
}

plotFitOps_s <- function(t) {
  fm <- nls(ops_s ~ EXP(delay, a, b), t, c(a = 30000, b = -0.005))
  curve(EXP(x, coef(fm)[1], coef(fm)[2]), add = TRUE, col = "red")
}

plotFitUpd <- function(t) {
  fm <- lm(upd ~ delay, t)
  curve(coef(fm)[1] + coef(fm)[2] * x, add = TRUE, col = "red")
}

plotMetric <- function(t, ycolname, xcolname, fitfun = NULL) {
  for (c in CS) {
    fname <- paste(paste(ycolname, c, sep = "_"), "png", sep = ".")
    png(fname, width = 1200, height = 1500)

    par(mfrow = c(5, 4))
    for (r in RS) {
      for (l in LS) {
        for (p in PS) {
          s <- subset(t, consist == c & r_w == r & loc == l & pop == p)

          gname <- paste(r, l, p)
          plot(s[[ycolname]] ~ s[[xcolname]], data = s, main = gname,
               xlab = xcolname, ylab = ycolname)

          if (!is.null(fitfun)) { fitfun(s) }
        }
      }
    }
  }
  graphics.off()
}

ops_s <- rowMeans(cbind(T1$ops_s, T2$ops_s, T3$ops_s))
get <- rowMeans(cbind(T1$get, T2$get, T3$get))
upd <- rowMeans(cbind(T1$upd, T2$upd, T3$upd))
confl <- rowMeans(cbind(T1$confl, T2$confl, T3$confl))
mig <- rowMeans(cbind(T1$mig, T2$mig, T3$mig))

T <- cbind(T1[1:5], ops_s, get, upd, confl, mig)
T <- cbind(T, users = avgUsers(T))

plotMetric(T, "ops_s", "delay", plotFitOps_s)
plotMetric(T, "get", "delay")
plotMetric(T, "upd", "delay", plotFitUpd)
plotMetric(T, "confl", "delay")
plotMetric(T, "users", "delay")
