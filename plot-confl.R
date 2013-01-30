#!/usr/bin/Rscript

library(hash)

t <- read.table("res.1/summary.csv", header = T, sep = ",")
t <- t[with(t, order(consist, loc, delay, delay_var)), ]

cs <- levels(t$consist)
cols <- hash(cs, c("red", "blue", "green", "orange"))
ltys <- hash(cs, c("solid", "dashed", "dotted", "dotdash"))

t <- subset(t, delay_var == 0 & loc == 0.9)

fname <- "confl.png"
png(fname, width = 500, height = 500)
gname <- "Conflitos (variação de latência = 60%)"

s <- subset(t, consist == "lat")
print(dim(s$confl))
plot(confl ~ delay, data = s, main = gname,
     ylim = c(0, 0.4), col = cols[["lat"]])

for (c in c("ev1", "ev2", "any")) {
  s <- subset(t, consist == c)
  points(confl ~ delay, data = s, col = cols[[c]])
}
graphics.off()
