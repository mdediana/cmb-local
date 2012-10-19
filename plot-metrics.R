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

CheckFit <- function(t, fm) {
  if ((RS[t$r_w[1]] == "1:0" && all.vars(formula(fm))[1] == "upd"))
    return(NULL)

  # errors: gcap 5.6.4
  r2 <- summary(fm)$adj.r.squared 
  if (r2 < 0.7) {
    scenario <- paste(CS[t$consist[1]], RS[t$r_w[1]], t$loc[1], PS[t$pop[1]])
    cat("Bad R2:", r2, "[", all.vars(formula(fm)), "] [", scenario, "]", "\n")
  }
}

PlotFitGet <- function(t, color = "red") {
  fm <- lm(get ~ poly(delay, 3, raw = TRUE), t)
  CheckFit(t, fm)
  curve(coef(fm)[1] + coef(fm)[2] * x + coef(fm)[3] * x ^ 2 +
        coef(fm)[4] * x ^ 3, add = TRUE, col = color)
}

PlotFitUpd <- function(t, color = "red") {
  fm <- lm(upd ~ delay, t)
  CheckFit(t, fm)
  curve(coef(fm)[1] + coef(fm)[2] * x, add = TRUE, col = color)
}

PlotFitOps_s <- function(t, color = "red") {
  f <- function(x, a, b) { a * exp(b * x) }
  fm <- nls(ops_s ~ f(delay, a, b), t, c(a = 30000, b = -0.005))
  # do not check fitness because r-squared is valid for linear regression only
  curve(f(x, coef(fm)[1], coef(fm)[2]), add = TRUE, col = color)
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

          if (!is.null(fitfun))
            fitfun(s)
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
        s <- lapply(CS, function(c) subset(means, consist == c & r_w == r &
                                                  loc == l & pop == p))

        gname <- paste(r, l, p)
        for (i in 1:length(s)) {
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

AvgUsers <- function(t) {
  rw <- strsplit(RS[t$r_w], ":")
  r <- as.numeric(sapply(rw, "[[", 1))
  w <- as.numeric(sapply(rw, "[[", 2))
  t$ops_s * (r * t$get + w * t$upd) # little's law
}

t <- rbind(T1, T2, T3)

means <- T1[1:5]
for (m in c("get", "upd", "ops_s", "confl", "mig")) {
  cols <- cbind(T1[[m]], T2[[m]], T3[[m]])
  old.names <- colnames(means)
  means <- cbind(means,
                 rowMeans(cols),
                 apply(cols, 1, sd),
                 apply(cols, 1, function(x) sqrt(var(x) / length(x)))) # std err
  colnames(means) <- c(old.names,
                       m,
                       paste(m, "sd", sep = "."),
                       paste(m, "se", sep = "."))
}
means <- cbind(means, users = AvgUsers(means))

PlotAll(t)

PlotMetric(t, "get", "delay", c(0, 20), PlotFitGet)
PlotMetric(t, "upd", "delay", c(0,450), PlotFitUpd)
PlotMetric(t, "ops_s", "delay", c(0,3e4), PlotFitOps_s)
PlotMetric(t, "confl", "delay", c(0, 35))
PlotMetric(t, "mig", "delay", c(0, 10))
PlotMetric(means, "users", "delay", c(2e5, 4e6))

PlotConsistComp(t, means, "get", "delay", c(0, 20), PlotFitGet)
PlotConsistComp(t, means, "upd", "delay", c(0,450), PlotFitUpd)
PlotConsistComp(t, means, "ops_s", "delay", c(0,3e4), PlotFitOps_s)
