# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages and function
libraries = c("ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
logreturnsector = read.csv("logreturnsector.csv", header = TRUE)

# plot
sdvsmedian = ggplot(logreturnsector, aes(x = Mean, y = SD, color = Sector))
sdvsmedian + geom_point() + xlab("Mean") + ylab("Volatility")
