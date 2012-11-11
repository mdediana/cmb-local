#!/usr/bin/Rscript

library(hash)

ts = lapply(1:4, function(i) {
                   order.by = c("consist", "r_w", "loc", "pop", "delay")
                   dn <- paste("res", i, sep = '.')
                   fn <- paste(dn, "res.csv", sep = "/")
                   t <- read.table(fn, header = TRUE, sep = ",")
                   t[do.call(order, t[order.by]), ]
                 })
t <- do.call(rbind, ts)

CS <- levels(t$consist)
RS <- levels(t$r_w)
LS <- unique(t$loc)
PS <- levels(t$pop)
DS <- unique(t$delay)

COLORS <- hash()  # one color, one consistency model
COLORS[CS[1]] <- "red"
COLORS[CS[2]] <- "blue"
COLORS[CS[3]] <- "green"
COLORS[CS[4]] <- "orange"

CheckCv <- function(m) {
  limits <- c(0.2, 0.3, 0.4, 0.5)
  for (col in c("get.cv", "upd.cv", "ops_s.cv")) {
    cvs <- sapply(limits, function(l) dim(m[!is.nan(m[[col]]) &
                                            m[[col]] > l, ])[1])
    cat(col, ":", cvs, "\n")
  }
}

CheckFit <- function(t, fm) {
  if ((RS[t$r_w[1]] == "1:0" && all.vars(formula(fm))[1] == "upd"))
    return(NULL)

  # errors: gcap 5.6.4
  r2 <- summary(fm)$adj.r.squared
  # r2 may be NaN when conflicts == 0 
  if (!is.nan(r2) && r2 < 0.7) {
    scenario <- paste(CS[t$consist[1]], RS[t$r_w[1]], t$loc[1], PS[t$pop[1]])
    cat("Bad R2:", r2, "[", all.vars(formula(fm)), "] [", scenario, "]\n")
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
  if (RS[t$r_w[1]] == "1:0") {
    fm <- lm(ops_s ~ poly(delay, 3, raw = TRUE), t)
    CheckFit(t, fm)
    curve(coef(fm)[1] + coef(fm)[2] * x + coef(fm)[3] * x ^ 2 +
          coef(fm)[4] * x ^ 3, add = TRUE, col = color)
  } else {
    f <- function(x, a, b) { a * exp(b * x) }
    fm <- nls(ops_s ~ f(delay, a, b), t, c(a = 30000, b = -0.005))
    # don't check fitness because r-squared is valid for linear regression only
    curve(f(x, coef(fm)[1], coef(fm)[2]), add = TRUE, col = color)
  }
}

PlotFitConfl <- function(t, color = "red") {
  fm <- lm(confl ~ poly(delay, 2, raw = TRUE), t)
  CheckFit(t, fm)
  curve(coef(fm)[1] + coef(fm)[2] * x + coef(fm)[3] * x ^ 2,
        add = TRUE, col = color)
}

PlotAll <- function(t) {
  for (c in CS) {
    for (d in c(0, 150, 300)) {
      fname <- paste(paste("all", c, d, sep = "_"), "png", sep = ".")
      png(fname, width = 1200, height = 1500)
      s <- subset(t, delay == d)
      plot(~ get + upd + ops_s + confl + mig, data = s)
    }
    graphics.off()
  }
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

PlotConsistComp <- function(t, m, ycolname, xcolname, ylim, fitfun = NULL) {
  fname <- paste(paste("consist", ycolname, sep = "_"), "png", sep = ".")
  png(fname, width = 1200, height = 1500)
  par(mfrow = c(5, 4))

  for (r in RS) {
    for (l in LS) {
      for (p in PS) {
        s <- lapply(CS, function(c) subset(m, consist == c & r_w == r &
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

metrics <- c("get", "upd", "ops_s", "confl", "mig")
m <- data.frame()
for (c in CS) {
  for (r in RS)
    for (l in LS)
      for (p in PS)
        for (d in DS) {
          if (c == "ev(w2)" && (d == 50 || d == 150 || d == 250))
            next

          # 1 - remove the most distant point from the mean in each scenario
          s <- subset(t, consist == c & r_w == r & loc == l &
                         pop == p & delay == d)
          get.mean <- mean(s$get)
          upd.mean <- mean(s$upd)
          get.cv <- sd(s$get) / get.mean
          upd.cv <- sd(s$upd) / upd.mean
          index <- function(v, mean) which(abs(v - mean) == max(abs(v - mean)))
          if (is.nan(upd.cv) || get.cv > upd.cv)
            s.i <- index(s$get, get.mean)
          else
            s.i <- index(s$upd, upd.mean)
          # length(index) > 1 if more than one of some repeated value
          # satify the previous condition, so we take the first one
          t.rowname <- rownames(s[s.i[1], ])
          t.i <- which(rownames(t) == t.rowname)
          t <- t[-t.i, ]

          # 2 - compute the final mean, sd, se and cv
          s <- subset(t, consist == c & r_w == r & loc == l &
                         pop == p & delay == d)
          row <- s[1, 1:5]
          for (mn in metrics) {
            metric.mean <- mean(s[[mn]])
            metric.sd <- sd(s[[mn]])
            metric.se <- sqrt(var(s[[mn]]) / length(s[[mn]]))
            metric.cv <- metric.sd / metric.mean
            row <- cbind(row, metric.mean, metric.sd, metric.se, metric.cv)
          }
          m <- rbind(m, row)
        }
}

# add colnames to the metrics df
added.colnames <- sapply(metrics, function(mn) c(mn,
                                                 paste(mn, "sd", sep = "."),
                                                 paste(mn, "se", sep = "."),
                                                 paste(mn, "cv", sep = ".")))
colnames(m) <- c(colnames(m)[1:5], c(added.colnames))

# avg users
rw <- strsplit(RS[m$r_w], ":")
r <- as.numeric(sapply(rw, "[[", 1))
w <- as.numeric(sapply(rw, "[[", 2))
avg.users <- m$ops_s * (r * m$get + w * m$upd) # little's law
m <- cbind(m, avg.users)

CheckCv(m)

PlotAll(t)

PlotMetric(t, "get", "delay", c(0, 2e-2), PlotFitGet)
PlotMetric(t, "upd", "delay", c(0, 4.5e-1), PlotFitUpd)
PlotMetric(t, "ops_s", "delay", c(0, 3e4), PlotFitOps_s)
PlotMetric(t, "confl", "delay", c(0, 1.5e-1), PlotFitConfl)
PlotMetric(t, "mig", "delay", c(0, 1e-1))
PlotMetric(m, "avg.users", "delay", c(2.5e2, 4e3))

PlotConsistComp(t, m, "get", "delay", c(0, 2e-2), PlotFitGet)
PlotConsistComp(t, m, "upd", "delay", c(0, 4.5e-1), PlotFitUpd)
PlotConsistComp(t, m, "ops_s", "delay", c(0, 3e4), PlotFitOps_s)
PlotConsistComp(t, m, "confl", "delay", c(0, 2e-1), PlotFitConfl)

for (i in 1:4)
  cat(CS[i], ":", COLORS[[CS[i]]], "\n")

warnings()
