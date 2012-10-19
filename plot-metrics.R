#!/usr/bin/Rscript

library(hash)

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

COLORS <- hash()  
COLORS[CS[1]] <- "red"
COLORS[CS[2]] <- "blue"
COLORS[CS[3]] <- "green"

AvgUsers <- function(t) {
  rw <- strsplit(RS[t$r_w], ":")
  r <- as.numeric(sapply(rw, "[[", 1))
  w <- as.numeric(sapply(rw, "[[", 2))
  t$ops_s * (r * t$get + w * t$upd) # little's law
}

TableMeans <- function(...) {
  ts = list(...)
  t <- ts[[1]][1:5]
  metrics <- c("get", "upd", "ops_s", "confl", "mig")

  for (m in metrics) {
    old <- colnames(t)
    cols <- sapply(ts, function(t) t[[m]])
    t <- cbind(t,
               rowMeans(cols),
               apply(cols, 1, sd),
               apply(cols, 1, function(x) sqrt(var(x) / length(x)))) # std err
    colnames(t) <- c(old, m,
                     paste(m, "sd", sep = "."),
                     paste(m, "se", sep = "."))
  }

  cbind(t, users = AvgUsers(t))
}

CheckFit <- function(t, fm, formula) {
  if ((RS[t$r_w[1]] == "1:0" && all.vars(formula)[1] == "upd"))
    return(NULL)

  # errors: gcap 5.6.4
  r2 <- summary(fm)$adj.r.squared 
  if (r2 < 0.7) {
    scenario <- paste(CS[t$consist[1]], RS[t$r_w[1]], t$loc[1], PS[t$pop[1]])
    cat("Bad R2:", r2, "[", all.vars(formula), "] [", scenario, "]", "\n")
  }
}

PlotFitLin <- function(t, formula, color = "red") {
  fm <- lm(formula, t)
  CheckFit(t, fm, formula)
  curve(coef(fm)[1] + coef(fm)[2] * x, add = TRUE, col = color)
}

PlotFitGet <- function(t, color = "red") {
  formula <- get ~ poly(delay, 3, raw = TRUE)
  fm <- lm(formula, t)
  CheckFit(t, fm, formula)
  curve(coef(fm)[1] + coef(fm)[2] * x + coef(fm)[3] * x ^ 2 +
        coef(fm)[4] * x ^ 3, add = TRUE, col = color)
}

PlotFitOps_s <- function(t, color = "red") {
  f <- function(x, a, b) { a * exp(b * x) }
  fm <- nls(ops_s ~ f(delay, a, b), t, c(a = 30000, b = -0.005))
  # do not check fitness because r-squared is valid for linear regression only
  curve(f(x, coef(fm)[1], coef(fm)[2]), add = TRUE, col = color)
}

PlotFitUpd <- function(t, color = "red") {
  PlotFitLin(t, upd ~ delay, color)
}

PlotFitUsers <- function(t, color = "red") {
  PlotFitLin(t, users ~ delay, color)
}

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

          if (!is.null(fitfun)) { 
            s <- subset(t, consist == c & r_w == r & loc == l & pop == p)
            fitfun(s)
          }
        }
      }
    }
  }
  graphics.off()
}

PlotConsistComp <- function(t, means, ycolname, xcolname, ylim, fitfun = NULL) {
  fname <- paste(paste("consist", ycolname, sep = "_"), "png", sep = ".")
  png(fname, width = 1200, height = 1500)
  par(mfrow = c(5, 4))

  for (r in RS) {
    for (l in LS) {
      for (p in PS) {
        s <- lapply(1:3, function(i) subset(means, consist == CS[i] & r_w == r & loc == l & pop == p))

        gname <- paste(r, l, p)
        for (i in 1:3) {
          xy = s[[i]][[ycolname]] ~ s[[i]][[xcolname]]
          color = COLORS[[CS[i]]]
          if (i == 1)
            plot(xy, data = s[[i]], main = gname, xlab = xcolname,
                 ylab = ycolname, ylim = ylim, col = color)
          else
            points(xy, data = s[[i]], col = color)

          if (!is.null(fitfun))
            fitfun(s[[i]], color)
        }
      }
    }
  }
  graphics.off()
}

t <- rbind(T1, T2, T3)
means <- TableMeans(T1, T2, T3)

PlotAll(t)

PlotMetric(t, means, "get", "delay", c(0, 20), PlotFitGet)
#PlotMetric(t, means, "upd", "delay", c(0,450), PlotFitUpd)
#PlotMetric(t, means, "ops_s", "delay", c(0,3e4), PlotFitOps_s)
#PlotMetric(t, means, "confl", "delay", c(0, 35))
#PlotMetric(t, means, "mig", "delay", c(0, 10))
#PlotMetric(t, means, "users", "delay", c(2e5, 4e6))

#PlotConsistComp(t, means, "get", "delay", c(0, 20), PlotFitGet)
#PlotConsistComp(t, means, "upd", "delay", c(0,450), PlotFitUpd)
#PlotConsistComp(t, means, "ops_s", "delay", c(0,3e4), PlotFitOps_s)
