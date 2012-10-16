#!/usr/bin/Rscript

T1 <- read.table("res.1.csv", header = TRUE, sep = ",")
T2 <- read.table("res.2.csv", header = TRUE, sep = ",")
T3 <- read.table("res.3.csv", header = TRUE, sep = ",")
TS <- list(T1, T2, T3)

CS <- levels(T1$consist)
RS <- levels(T1$r_w)
LS <- unique(T1$loc)
PS <- levels(T1$pop)
DS <- unique(T1$delay)

EXP <- function(x, a, b) { a * exp(b * x) }

Mean <- function(ts, colname) {
  rowMeans(sapply(ts, function(t) t[[colname]]))
}

CheckFit <- function(t, fm, formula) {
  if ((RS[t$r_w[1]] == "1:0" && all.vars(formula)[1] == "upd") ||
      (RS[t$r_w[1]] == "0:1" && all.vars(formula)[1] == "get")) {
    return(NULL)
  }

  # errors: gcap 5.6.4
  r2 <- summary(fm)$adj.r.squared 
  # There are no updates in 1:0
  if (r2 < 0.7) {
    scenario <- paste(CS[t$consist[1]], RS[t$r_w[1]], t$loc[1], PS[t$pop[1]])
    cat("Bad R2:", r2, "[", all.vars(formula), "] [", scenario, "]", "\n")
  }
}

AvgUsers <- function(t) {
  rw <- strsplit(RS[t$r_w], ":")
  r <- as.numeric(sapply(rw, "[[", 1))
  w <- as.numeric(sapply(rw, "[[", 2))
  t$ops_s * (r * t$get + w * t$upd) # little's law
}

PlotFitLin <- function(t, formula) {
  fm <- lm(formula, t)
  CheckFit(t, fm, formula)
  curve(coef(fm)[1] + coef(fm)[2] * x, add = TRUE, col = "red")
}

PlotFitGet <- function(t) {
  formula <- get ~ poly(delay, 3, raw = TRUE)
  fm <- lm(formula, t)
  CheckFit(t, fm, formula)
  curve(coef(fm)[1] + coef(fm)[2] * x + coef(fm)[3] * x ^ 2 +
        coef(fm)[4] * x ^ 3, add = TRUE, col = "red")
}

PlotFitOps_s <- function(t) {
  fm <- nls(ops_s ~ EXP(delay, a, b), t, c(a = 30000, b = -0.005))
  # do not check fitness because r-squared is valid for linear regression only
  curve(EXP(x, coef(fm)[1], coef(fm)[2]), add = TRUE, col = "red")
}

PlotFitUpd <- function(t) { PlotFitLin(t, upd ~ delay) }
PlotFitUsers <- function(t) { PlotFitLin(t, users ~ delay) }

PlotAll <- function(t) {
  for (d in DS) {
    fname <- paste(paste("all", d, sep = "_"), "png", sep = ".")
    png(fname, width = 1200, height = 1500)
    s <- subset(t, delay == d)
    plot(~ get + upd + ops_s + confl + mig, data = s)
  }
  graphics.off()
}

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

get <- Mean(TS, "get")
upd <- Mean(TS, "upd")
ops_s <- Mean(TS, "ops_s")
confl <- Mean(TS, "confl")
mig <- Mean(TS, "mig")

T <- cbind(T1[1:5], ops_s, get, upd, confl, mig)
T <- cbind(T, users = AvgUsers(T))

PlotAll(T)

PlotMetric(T, "get", "delay", c(0, 20), PlotFitGet)
#PlotMetric(T, "get", "delay", c(0, 200), PlotFitGet)
PlotMetric(T, "upd", "delay", c(0,450), PlotFitUpd)
PlotMetric(T, "ops_s", "delay", c(0,3e4), PlotFitOps_s)
PlotMetric(T, "confl", "delay", c(0, 35))
PlotMetric(T, "mig", "delay", c(0, 10))
#PlotMetric(T, "users", "delay", c(2e5, 4e6), PlotFitUsers)
PlotMetric(T, "users", "delay", c(2e5, 4e6))
