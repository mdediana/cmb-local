#!/usr/bin/Rscript

# test - Raj Jain - Table 17.9
#y <- c(14, 22, 10, 34, 46, 58, 50, 86) 
#fraction.ss.percent <- c(18  4 71  4  1  2  0)

dir <- commandArgs(trailingOnly = TRUE)[2]
fn <- paste(dir, "summary.csv", sep = "/")
t <- read.table(fn, header = TRUE, sep = ",")
# Important: t must be ordered in a way that match sign table.
# It works, but it's more reliable if the sign table is built
# directly from t instead of depending on its order
t <- t[with(t, order(consist, delay, pop, loc, r_w)), ]

cat("Factors:", colnames(t)[2:5], "\n")

for (c in levels(t$consist)) {
  y <- apply(subset(t, consist == c), 1, function(x) {
    rw <- as.numeric(unlist(strsplit(x[2], ":")))
    get.upd = as.numeric(c(x[7], x[8]))
    weighted.mean(get.upd, c(rw[1], rw[2]))
  })

  #
  k <- log2(length(y))
  m <- matrix(, 2^k)
  factor.cols <- 2:(k + 1)
  cat("c =", c, "k =", k, "\n")

  # I
  m[, 1] <- 1

  # Main effects
  for (i in 0:(k - 1))
    m <- cbind(m, rep(c(rep(-1, 2^i), rep(1, 2^i)), 2^(k - 1 - i)))

  # 2- to k-factor interactions
  for (factors in 2:k)
    m <- cbind(m, combn(factor.cols, factors,
                        # Couldn't get to do it with Reduce
                        function(x) {
                          z <- m[, x[1]]
                          for (i in 2:factors)
                            z <- z * m[, x[i]]
                          z
                        }))

  # coeffs
  m <- m * y
  r <- apply(m, 2, sum) / 2^k
  stopifnot(all.equal(r[1], mean(y), 1e-4))

  # variation
  ss <- sapply(r[-1], function(x) { 2^k * x^2 })
  sst <- sum(ss)
  stopifnot(all.equal(sst, sum((y - mean(y))^2), 1e-4))
  fraction.ss <- ss / sst
  fraction.ss.percent <- 100 * round(fraction.ss, 2)
  print(fraction.ss.percent)
  #print(fraction.ss)
}
