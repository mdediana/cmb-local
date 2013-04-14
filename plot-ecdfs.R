#!/usr/bin/Rscript

library(plyr)
library(ggplot2)

t <- read.table('res/percentiles.csv', header = T, sep = ',')
t <- t[with(t, order(consist, op, loc, delay, delay_var)), ]

for (d in c(0, 200)) {
  fname <- paste(paste('ecdf', d, sep = ''), 'png', sep = '.')

  s <- subset(t, delay_var != 0 & delay == d)
  v <- do.call(rbind,
               lapply(1:nrow(s), function(r)
                    # Transform perc columns into rows.
                    # Ignore 99.9th percentile (-1 below).
                    data.frame(t(sapply(12:(ncol(s)), function(c)
                                        c(levels(s$consist[r])[s$consist[r]],
                                        s$loc[r],
                                        levels(s$op[r])[s$op[r]],
                                        s[r, c]))))))
  colnames(v) <- c('consist', 'loc', 'op', 'perc')
  v$perc <- as.numeric(as.character(v$perc))
  v$consist <- factor(v$consist,
                      levels = c('ev1', 'ev2', 'any', 'lat'),
                      labels = c('ind1', 'ind2', 'lt_qqer', 'lt_rec'))
  levels(v$loc) <- c('Loc = 50%', 'Loc = 90%')
  levels(v$op) <- c('Leituras', 'Escritas')

  p <- ggplot(v, aes(x = perc, colour = consist, linetype = consist)) +
       labs(title = paste('FDAs\n(latência = ', d, ' ms)'),
            x = 'Tempo de resposta (em s)', y = 'F(X)') +
       scale_colour_discrete('Modo') +
       scale_linetype_discrete('Modo') +
       scale_x_continuous(limits = c(0, 1.5)) +
       stat_ecdf() +
       geom_vline(xintercept = d / 1e3, linetype = 'dashed', size = 0.1) +
       facet_grid(op ~ loc)

  ggsave(plot = p, filename = fname)
}

print(p)

# Relative gains
for (l in unique(t$loc)) {
  cat('loc:', l, '\n')

  s <- subset(t, delay == 200 & delay_var == 60 & op == 'upd' & loc == l)
  ev2 <-subset(s, consist == 'ev2')
  lat <-subset(s, consist == 'lat')
  any <-subset(s, consist == 'any')

  cat('p75, any, lat, ev2:', any$p75, lat$p75, ev2$p75, '\n')
  cat('p75, any / ev2:', ev2$p75 / any$p75, '\n') 
  cat('p75, lat / ev2:', ev2$p75 / lat$p75, '\n') 
  cat('p95, any, lat, ev2:', any$p95, lat$p95, ev2$p95, '\n')
  cat('p95, any / ev2:', ev2$p95 / any$p95, '\n') 
  cat('p95, lat / ev2:', ev2$p95 / lat$p95, '\n') 
}
