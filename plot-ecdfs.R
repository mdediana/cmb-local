#!/usr/bin/Rscript

library(hash)

t <- read.table("res.1/percentiles.csv", header = T, sep = ",")
t <- t[with(t, order(consist, op, loc, delay, delay_var)), ]

cs <- levels(t$consist)
cols <- hash(cs, c("red", "blue", "green", "orange"))
pchs <- hash(cs, c(1, 2, 5, 6))

for (c in cs)
  cat(c, cols[[c]], '\n')

OPS.PT <- hash()
OPS.PT["get"] <- "Leituras"
OPS.PT["upd"] <- "Escritas"

# Ecdfs
for (d in unique(t$delay)) {
  fname <- paste(paste("ecdf", d, sep = ''), "png", sep = ".")
  png(fname, width = 1200, height = 1500)
  par(mfrow = c(2, 2))

  for (o in levels(t$op))
  for (l in unique(t$loc)) {
    cat(d, o, l, '\n')

    s <- subset(t, delay_var != 0 & delay == d & op == o & loc == l)
    gname <- paste(OPS.PT[[o]], "/ Loc =", 100 * l, "%")

    u <- subset(s, consist == "ev1")
    v <- as.numeric(u[-c(1:10)])
    plot.ecdf(data.matrix(v), main = gname,
              xlim = c(0, 1.5),
              xlab = "Tempo de resposta (em s)",
              cex.lab = 1.7, cex.axis = 1.3, cex.main = 1.7,
              pch = pchs[[c]], col = cols[[c]])
    #legend(1, 0.4, c(cs), col = cols)

    for (c in c("ev2", "any", "lat")) {
      u <- subset(s, consist == c)
      v <- as.numeric(u[-c(1:10)])
      plot.ecdf(data.matrix(v), pch = pchs[[c]], col = cols[[c]], add = T)
    }

    delay = as.numeric(s$delay) / 1e3	# ms -> s
    abline(v = delay, lty = "dotted")
    abline(h = l, lty = "dotted")
  }
  graphics.off()
}

# Zoom
fname <- "ecdf_zoom.png"
png(fname, width = 600, height = 750)
#par(mfrow = c(2, 2))

o <- "get"
l <- 0.5
s <- subset(t, delay_var != 0 & delay == 0 & op == o & loc == l)
gname <- paste(OPS.PT[[o]], "/ Loc =", 100 * l, "%")

u <- subset(s, consist == "ev1")
v <- as.numeric(u[-c(1:10)])
plot.ecdf(data.matrix(v), main = gname,
          xlim = c(0, 0.05),
          xlab = "Tempo de resposta (em ms)",
          cex.lab = 1.7, cex.axis = 1.3, cex.main = 1.7,
          pch = pchs[[c]], col = cols[[c]])

graphics.off()

# Relative gains
for (l in unique(t$loc)) {
  cat("loc:", l, '\n')

  s <- subset(t, delay == 200 & delay_var == 60 & op == "upd" & loc == l)
  ev2 <-subset(s, consist == "ev2")
  lat <-subset(s, consist == "lat")
  any <-subset(s, consist == "any")

  cat("p75, any, lat, ev2:", any$p75, lat$p75, ev2$p75, '\n')
  cat("p75, any / ev2:", (ev2$p75 - any$p75) / ev2$p75, '\n') 
  cat("p75, lat / ev2:", (ev2$p75 - lat$p75) / ev2$p75, '\n') 
  cat("p95, any, lat, ev2:", any$p95, lat$p95, ev2$p95, '\n')
  cat("p95, any / ev2:", (ev2$p95 - any$p95) / ev2$p95, '\n') 
  cat("p95, lat / ev2:", (ev2$p95 - lat$p95) / ev2$p95, '\n') 
}
