library('beanplot')

op <- commandArgs(trailingOnly = T)[2]

d <- read.csv("percentiles.csv")

PlotResp <- function(operation) {
    data <- data.frame()
    for (m in c('ev1', 'ev2', 'any', 'lat')) {
        for (l in c(0.5, 0.9)) {
            s <- subset(d, consist == m & delay_var != 0 & delay == 200 & loc == l & op == operation)
            data <- rbind(data, 1000 * s[12:110])
        }
    }
    data <- data.frame(t(data))
    names(data) <- c('ev1', 'ev1', 'ev2', 'ev2', 'any', 'any', 'lat', 'lat')
    
    pdf(paste('beanplot', operation, 'pdf', sep='.'))
    #par(mar=c(5.1,4.1,4.1,2.1))
    par(mar=c(2.55,4.1,2.05,2.1))
    print(beanplot(data, ylim=c(1,2000), log="y", beanlines='mean', side='both', col = list("black", c("grey", "white")), ylab = "Response time (ms)"))
    print(legend("topleft", fill = c("black", "grey"), legend = c("Locality = 50%", "Locality = 90%")))
    dev.off()
}

PlotResp(op)
