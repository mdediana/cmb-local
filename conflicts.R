#!/usr/bin/Rscript

library(hash)

t1 <- read.csv("res.1/summary.csv", header = T)
t2 <- read.csv("res.delay_var/summary.csv", header = T)

# there are no rows with delay == 0 in t.30
for (v in sort(unique(t2$delay_var))) {
  s <- subset(t2, consist == "any" & loc == 0.9 & delay == 300 & delay_var == v)
  confl <- s$confl / s$get_n
  cat("delay_var =", v, '\n')
  print(summary(confl))
}

s <- subset(t1, consist == "any" & loc == 0.9 & delay == 300 & delay_var == 60)
confl <- s$confl / s$get_n
cat("delay_var = 60", '\n')
print(summary(confl))
