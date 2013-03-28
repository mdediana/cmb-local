#!/usr/bin/Rscript

library(hash)

t <- read.table("res.1/percentiles.csv", header = T, sep = ",")
t <- t[with(t, order(consist, op, loc, delay, delay_var)), ]

cs <- levels(t$consist)
cols <- hash(cs, c("red", "black", "green", "orange"))
pchs <- hash(cs, c(1, 2, 5, 6))
labels <- hash(c("any", "ev1", "ev2", "lat"),
               c("lt_qqer", "ind1", "ind2", "lt_rec"))

ops.pt <- hash(c("get", "upd"), c("Leituras", "Escritas"))

Label <- function(c, x, y) {
  text(x, y, labels = labels[[c]], col = cols[[c]], cex = 1.3)
}

for (c in cs)
  cat(c, cols[[c]], '\n')

# Ecdfs
for (d in c(0, 200)) {
  fname <- paste(paste("ecdf", d, sep = ''), "png", sep = ".")
  png(fname, width = 1200, height = 1500)
  par(mfrow = c(2, 2))

  for (o in levels(t$op))
  for (l in unique(t$loc)) {
    cat(d, o, l, '\n')

    s <- subset(t, delay_var != 0 & delay == d & op == o & loc == l)
    gname <- paste(ops.pt[[o]], "/ Loc =", 100 * l, "%")

    c <- "ev1"
    u <- subset(s, consist == c)
    v <- as.numeric(u[-c(1:10)])
    plot.ecdf(data.matrix(v), main = gname,
              xlim = c(0, 1.5),
              xlab = "Tempo de resposta (em s)",
              cex = 0.5, cex.lab = 1.7, cex.axis = 1.3, cex.main = 1.7,
              col = cols[[c]])
    Label(c, 0.1, 1.02)

    x.text = 0.15
    for (c in c("ev2", "any", "lat")) {
      u <- subset(s, consist == c)
      v <- as.numeric(u[-c(1:10)])
      plot.ecdf(data.matrix(v), cex = 0.5, col = cols[[c]], add = T)

      if (d == 0) {
        x.text <- x.text + 0.15
        Label(c, x.text, 1.02)
      } else {
        if (c == "ev2") {
          if (o == "get")
            Label(c, 0.6, 1.02)
          else
            Label(c, 0.2, 0.6)
        } else if (c == "lat") {
          if (o == "0.5")
            Label(c, 0.45, 0.97)
          else
            Label(c, 0.4, 0.97)
        } else if (c == "any") {
          if (o == "get")
            Label(c, 0.4, 1.02)
          else if (l == 0.5)
            Label(c, 0.8, 0.9)
          else
            Label(c, 0.4, 0.9)
        }
      }
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
gname <- paste(ops.pt[[o]], "/ Loc =", 100 * l, "%")

u <- subset(s, consist == "ev1")
v <- as.numeric(u[-c(1:10)])
plot.ecdf(data.matrix(v), main = gname,
          xlim = c(0, 0.05),
          xlab = "Tempo de resposta (em ms)",
          cex = 0.5, cex.lab = 1.7, cex.axis = 1.3, cex.main = 1.7,
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
  cat("p75, any / ev2:", ev2$p75 / any$p75, '\n') 
  cat("p75, lat / ev2:", ev2$p75 / lat$p75, '\n') 
  cat("p95, any, lat, ev2:", any$p95, lat$p95, ev2$p95, '\n')
  cat("p95, any / ev2:", ev2$p95 / any$p95, '\n') 
  cat("p95, lat / ev2:", ev2$p95 / lat$p95, '\n') 
}
