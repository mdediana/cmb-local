#!/usr/bin/Rscript

SignMatrix <- function(t, factors) {
  k <- length(factors)
  m <- matrix(nrow = dim(t)[1], ncol = 1)

  # I
  m[, 1] <- 1

  # Main effects
  m <- cbind(m, sapply(factors,
                       function(col) {
                         u <- unique(t[[col]])
                         stopifnot(length(u) == 2)
                         ifelse(t[[col]] == u[1], -1, 1)
                       }))

  # 2- to k-factor interactions
  last.factor.col = k + 1
  for (int.order in 2:k)
    m <- cbind(m, combn(2:last.factor.col, int.order,
                        # Couldn't get to do it with Reduce
                        function(x) {
                          int <- m[, x[1]]
                          for (i in 2:int.order)
                            int <- int * m[, x[i]]
                          int
                        }))
  m
}

# test - Raj Jain - Table 17.9
#y <- c(14, 22, 10, 34, 46, 58, 50, 86) 
#fraction.ss.percent <- c(18  4 71  4  1  2  0)
Variation <- function(m, y) {
  stopifnot(dim(m)[1] == length(y))
  k <- log2(length(y))

  # coeffs
  m <- m * y
  r <- apply(m, 2, sum) / 2^k 
  stopifnot(all.equal(r[1], mean(y), 1e-4, check.attributes = F))

  # variation
  ss <- sapply(r[-1], function(x) { 2^k * x^2 })
  sst <- sum(ss)
  stopifnot(all.equal(sst, sum((y - mean(y))^2), 1e-4))
  fraction.ss <- ss / sst
  fraction.ss.percent <- 100 * round(fraction.ss, 2)
  fraction.ss.percent
}

ByBDP <- function(t) {
  bdp <- (t$delay / 2 * 1e-3) * (1024^3 / 8) 	# delay rtt (ms) / 1Gb/s
  t$factor.bdp <- round(t$mem_max / bdp)	# mem_max = n * bdp
  
  factors <- c("delay", "reorder", "factor.bdp")
  cat("Factors:", factors, "\n")
  
  t <- t[with(t, order(delay, reorder, factor.bdp)), ]
  s <- subset(t, op == "get")
  m <- SignMatrix(s, factors)
  cat("get - p20:", Variation(m, s[["p20"]]), '\n')
  cat("get - p80:", Variation(m, s[["p80"]]), '\n')
  
  s <- subset(t, op == "upd")
  m <- SignMatrix(s, factors)
  cat("upd - p20:", Variation(m, s[["p20"]]), '\n')
  cat("upd - p80:", Variation(m, s[["p80"]]), '\n')
}

ByNet <- function(t) {
  t$delay_var_pc <- (t$delay_var / t$delay)

  factors <- c("delay", "delay_var_pc", "loss", "dupl", "reorder", "congest")
  cat("Factors:", factors, "\n")

  t <- t[with(t, order(delay, delay_var_pc, loss, dupl, reorder, congest)), ]
  s <- subset(t, op == "get")
  m <- SignMatrix(s, factors)
  cat("get - p10:", Variation(m, s[["p10"]]), '\n')
  cat("get - p90:", Variation(m, s[["p90"]]), '\n')
  
  s <- subset(t, op == "upd")
  m <- SignMatrix(s, factors)
  cat("upd - p10:", Variation(m, s[["p10"]]), '\n')
  cat("upd - p90:", Variation(m, s[["p90"]]), '\n')
}

ByWorkloadPerc <- function(t) {
  factors <- c("r_w", "loc", "pop", "delay")
  cat("Factors:", factors, "\n")

  t <- t[with(t, order(consist, r_w, loc, pop, delay)), ]
  
  for (c in levels(t$consist)) {
    cat("c =", c, "\n")
    s <- subset(t, consist == c & op == "get")
    m <- SignMatrix(s, factors)
    cat("get - p10:", Variation(m, s[["p10"]]), '\n')
    cat("get - p40:", Variation(m, s[["p40"]]), '\n')
    cat("get - p90:", Variation(m, s[["p90"]]), '\n')
    s <- subset(t, consist == c & op == "upd")
    m <- SignMatrix(s, factors)
    cat("upd - p10:", Variation(m, s[["p10"]]), '\n')
    cat("upd - p40:", Variation(m, s[["p40"]]), '\n')
    cat("upd - p90:", Variation(m, s[["p90"]]), '\n')
  }
}

ByWorkloadSumm <- function(t) {
  factors <- c("r_w", "loc", "pop", "delay")
  cat("Factors:", factors, "\n")

  t <- t[with(t, order(consist, r_w, loc, pop, delay)), ]
  
  for (c in levels(t$consist)) {
    cat("c =", c, "\n")
    s <- subset(t, consist == c)
    m <- SignMatrix(s, factors)
    y <- apply(s, 1, function(x) {
                       rw <- as.numeric(unlist(strsplit(x[2], ":")))
                       get.upd = as.numeric(c(x[7], x[8]))
                       weighted.mean(get.upd, c(rw[1], rw[2]))
                     })
    cat("Mean:", Variation(m, y), '\n')
    cat("Confl:", Variation(m, s[["confl"]]), '\n')
    cat("Migs:", Variation(m, s[["mig"]]), '\n')
  }
}

dir <- commandArgs(trailingOnly = T)[2]
t.s <- read.table(paste(dir, "summary.csv", sep = '/'), T, ',')
t.p <- read.table(paste(dir, "percentiles.csv", sep = '/'), T, ',')

#ByBDP(t.p)
#ByWorkloadSumm(t.s)
#ByWorkloadPerc(t.p)
ByNet(t.p)
