#!/usr/bin/Rscript

#library(Hmisc)

Load <- function(fn) {
  order.by = c("consist", "r_w", "loc", "pop", "delay")

  t <- read.table(fn, header = TRUE, sep = ",")
  t <- t[do.call(order, t[order.by]), ]
}

T1 <- Load("res.1.csv")
T2 <- Load("res.2.csv")
T3 <- Load("res.3.csv")

CS <- levels(T1$consist)
RS <- levels(T1$r_w)
LS <- unique(T1$loc)
PS <- levels(T1$pop)
DS <- unique(T1$delay)

Mean <- function(ts, colname) {
  rowMeans(sapply(ts, function(t) t[[colname]]))
}

Sd <- function(ts, colname) {
  cols <- sapply(ts, function(t) t[[colname]])
  apply(cols, 1, sd)
}

AvgUsers <- function(t) {
  rw <- strsplit(RS[t$r_w], ":")
  r <- as.numeric(sapply(rw, "[[", 1))
  w <- as.numeric(sapply(rw, "[[", 2))
  t$ops_s * (r * t$get + w * t$upd) # little's law
}

TableMean <- function(...) {
  ts = list(...)

  get <- Mean(ts, "get")
  upd <- Mean(ts, "upd")
  ops_s <- Mean(ts, "ops_s")
  confl <- Mean(ts, "confl")
  mig <- Mean(ts, "mig")

  get.sd <- Sd(ts, "get")
  upd.sd <- Sd(ts, "upd")
  ops_s.sd <- Sd(ts, "ops_s")
  confl.sd <- Sd(ts, "confl")
  mig.sd <- Sd(ts, "mig")

  t <- cbind(T1[1:5], ops_s, ops_s.sd, get, get.sd, upd, upd.sd, confl, confl.sd,
             mig, mig.sd)
  cbind(t, users = AvgUsers(t))
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
  f <- function(x, a, b) { a * exp(b * x) }
  fm <- nls(ops_s ~ f(delay, a, b), t, c(a = 30000, b = -0.005))
  # do not check fitness because r-squared is valid for linear regression only
  curve(f(x, coef(fm)[1], coef(fm)[2]), add = TRUE, col = "red")
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

PlotMetric <- function(t, means, ycolname, xcolname, ylim, fitfun = NULL) {
  for (c in CS) {
    fname <- paste(paste(ycolname, c, sep = "_"), "png", sep = ".")
    png(fname, width = 1200, height = 1500)

    par(mfrow = c(5, 4))
    for (r in RS) {
      for (l in LS) {
        for (p in PS) {
          s <- subset(means, consist == c & r_w == r & loc == l & pop == p)

          gname <- paste(r, l, p)
          plot(s[[ycolname]] ~ s[[xcolname]], data = s, main = gname,
               xlab = xcolname, ylab = ycolname, ylim = ylim)

#          sdcolname <- paste(ycolname, "sd", sep = ".")
#          sdplus <- s[[ycolname]] + s[[sdcolname]]
#          sdminus <- s[[ycolname]] - s[[sdcolname]]
#          errbar(s[[xcolname]], s[[ycolname]], sdplus, sdminus)

          s <- subset(t, consist == c & r_w == r & loc == l & pop == p)
          if (!is.null(fitfun)) { fitfun(s) }
        }
      }
    }
  }
  graphics.off()
}

t <- rbind(T1, T2, T3)
means <- TableMean(T1, T2, T3)

PlotAll(t)

PlotMetric(t, means, "get", "delay", c(0, 20), PlotFitGet)
PlotMetric(t, means, "upd", "delay", c(0,450), PlotFitUpd)
PlotMetric(t, means, "ops_s", "delay", c(0,3e4), PlotFitOps_s)
PlotMetric(t, means, "confl", "delay", c(0, 35))
PlotMetric(t, means, "mig", "delay", c(0, 10))
#PlotMetric(t, means, "users", "delay", c(2e5, 4e6), PlotFitUsers)
PlotMetric(t, means, "users", "delay", c(2e5, 4e6))
