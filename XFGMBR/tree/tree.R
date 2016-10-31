# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages and function
libraries = c("tree")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# Tree classification
SPtest <- read.csv("SPrating.csv")
library(tree)
treesp = tree(Rating ~ . - Ticker, SPtest)
plot(treesp)
text(treesp, pretty = TRUE, cex = 0.5)

# Pruned Tree Classification
set.seed(2)
cvSPtest = cv.tree(treesp, FUN = prune.misclass)
pruneSPtest = prune.misclass(treesp, best = 7)
