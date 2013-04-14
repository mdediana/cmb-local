#!/usr/bin/Rscript

library(ggplot2)

t <- read.csv('res/percentiles.csv', header = T)
t <- t[with(t, order(consist, op, loc, delay, delay_var)), ]

d <- 200
fname <- paste(paste('boxplot', d, sep = ''), 'png', sep = '.')

s <- subset(t, delay_var != 0 & delay == d)
v <- do.call(rbind,
             lapply(1:nrow(s), function(r)
                    data.frame(t(sapply(12:ncol(s), function(c)
                                        c(levels(s$consist[r])[s$consist[r]],
                                          s$loc[r],
                                          levels(s$op[r])[s$op[r]],
                                          s[r, c]))))))
colnames(v) <- c('consist', 'loc', 'op', 'rt')
v$consist <- factor(v$consist, levels = c('ev1', 'ev2', 'any', 'lat'),
                               labels = c('ind1', 'ind2', 'lt_qqer', 'lt_rec'))
levels(v$loc) <- c('Loc = 50%', 'Loc = 90%')
levels(v$op) <- c('Leituras', 'Escritas')
v$rt <- as.numeric(as.character(v$rt))

p <- ggplot(v, aes(x = consist, y = rt)) +
     ggtitle('Tempos de resposta\n(latência = 200 ms)') +
     geom_boxplot()+ #outlier.size = 0) +
     scale_x_discrete(name = 'Modo') +
     scale_y_continuous(name = 'Tempo de resposta (em s)',
                        limits = c(0, 1.0)) +
     facet_grid(op ~ loc)
ggsave(plot = p, filename = fname)
print(p)
