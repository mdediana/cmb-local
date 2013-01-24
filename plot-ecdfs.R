#!/usr/bin/Rscript

library(hash)

t <- read.table("res.1/percentiles.csv", header = T, sep = ",")
t <- t[with(t, order(consist, op, loc, delay, delay_var, loss)), ]

cs <- levels(t$consist)
cols <- hash(cs, c("red", "blue", "green", "orange"))
ltys <- hash(cs, c("solid", "dashed", "dotted", "dotdash"))

for (c in cs)
  cat(c, cols[[c]], '\n')

OPS.PT <- hash()
OPS.PT["get"] <- "Leituras"
OPS.PT["upd"] <- "Escritas"

for (d in unique(t$delay)) {
  fname <- paste(paste("ecdf", d, sep = ''), "png", sep = ".")
  png(fname, width = 1200, height = 1500)
  par(mfrow = c(2, 2))

  for (o in levels(t$op))
  for (l in unique(t$loc)) {
    cat(d, o, l, '\n')

    s <- subset(t, delay_var != 0 & loss == 0 &
                   delay == d & op == o & loc == l)
    gname <- paste(OPS.PT[[o]], "/ Loc =", l)

    u <- subset(s, consist == "ev1")
    v <- as.numeric(u[-c(1:10)])
    plot.ecdf(data.matrix(v), main = gname,
              xlim = c(0, 1.5),
              xlab = "Tempo de resposta (em s)",
              cex = 0, cex.lab = 1.7, cex.axis = 1.3, cex.main = 1.7,
              col = cols[[c]])
    for (c in c("ev2", "any", "lat")) {
      u <- subset(s, consist == c)
      v <- as.numeric(u[-c(1:10)])
      plot.ecdf(data.matrix(v), col = cols[[c]], add = T)
    }

    delay = as.numeric(s$delay) / 1e3	# ms -> s
    abline(v = delay, lty = "dotted")
    abline(h = l, lty = "dotted")
  }
  graphics.off()
}
