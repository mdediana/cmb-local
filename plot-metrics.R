#!/usr/bin/Rscript

T1 <- read.table("res.1.csv", header = TRUE, sep = ",")
T2 <- read.table("res.2.csv", header = TRUE, sep = ",")
T3 <- read.table("res.3.csv", header = TRUE, sep = ",")
TS <- list(T1, T2, T3)

CS <- levels(T1$consist)
RS <- levels(T1$r_w)
LS <- unique(T1$loc)
PS <- levels(T1$pop)

EXP <- function(x, a, b) { a * exp(b * x) }

Mean <- function(ts, colname) {
  rowMeans(sapply(ts, function(t) t[[colname]]))
}

AvgUsers <- function(t) {
  rw <- strsplit(RS[t$r_w], ":")
  r <- as.numeric(sapply(rw, "[[", 1))
  w <- as.numeric(sapply(rw, "[[", 2))
  t$ops_s * (r * t$get + w * t$upd) # little's law
}

PlotFitLin <- function(t, formula) {
  fm <- lm(formula, t)
  # errors: gcap 5.6.4
  # print(summary(fm))
  curve(coef(fm)[1] + coef(fm)[2] * x, add = TRUE, col = "red")
}

PlotFitOps_s <- function(t) {
  fm <- nls(ops_s ~ EXP(delay, a, b), t, c(a = 30000, b = -0.005))
  curve(EXP(x, coef(fm)[1], coef(fm)[2]), add = TRUE, col = "red")
}

PlotFitGet <- function(t) { PlotFitLin(t, get ~ delay) }
PlotFitUpd <- function(t) { PlotFitLin(t, upd ~ delay) }
PlotFitUsers <- function(t) { PlotFitLin(t, users ~ delay) }

PlotMetric <- function(t, ycolname, xcolname, ylim, fitfun = NULL) {
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
               xlab = xcolname, ylab = ycolname, ylim = ylim)

          if (!is.null(fitfun)) { fitfun(s) }
        }
      }
    }
  }
  graphics.off()
}

ops_s <- Mean(TS, "ops_s")
get <- Mean(TS, "get")
upd <- Mean(TS, "upd")
confl <- Mean(TS, "confl")
mig <- Mean(TS, "mig")

T <- cbind(T1[1:5], ops_s, get, upd, confl, mig)
T <- cbind(T, users = AvgUsers(T))

PlotMetric(T, "ops_s", "delay", c(0,3e4), PlotFitOps_s)
PlotMetric(T, "get", "delay", c(0, 150))
PlotMetric(T, "upd", "delay", c(0,450), PlotFitUpd)
PlotMetric(T, "confl", "delay", c(0, 35))
PlotMetric(T, "mig", "delay", c(0, 10))
PlotMetric(T, "users", "delay", c(2e5, 4e6), PlotFitUsers)
