# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages and function
libraries = c("stats","ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# get cluster result
clusterresult = read.csv("clusterresult.csv", header = TRUE)
clusterdata = clusterresult[, c(7, 8)]
set.seed(10)  #set seed to make sure every time the result is the same
ind = 0
kmean = 0
ks = Inf
while (ind < 100) {
    kmeanresult = kmeans(clusterdata, 8, iter.max = 100, nstart = 1000)
    if (ks > kmeanresult$tot.withinss) {
        ks = kmeanresult$tot.withinss
        kmean = kmeanresult
    }
    ind = ind + 1
}
clusterresult$cluster = kmean$cluster

# create dataset in terms of cluster group and record the series
cgroup1 = matrix(NA, nrow = 2767, ncol = 5)
cgroup2 = matrix(NA, nrow = 2767, ncol = 196)
cgroup3 = matrix(NA, nrow = 2767, ncol = 5)
cgroup4 = matrix(NA, nrow = 2767, ncol = 2)
cgroup5 = matrix(NA, nrow = 2767, ncol = 2)
cgroup6 = matrix(NA, nrow = 2767, ncol = 51)
cgroup7 = matrix(NA, nrow = 2767, ncol = 32)
cgroup8 = matrix(NA, nrow = 2767, ncol = 1)
cind1 = 1
cind2 = 1
cind3 = 1
cind4 = 1
cind5 = 1
cind6 = 1
cind7 = 1
cind8 = 1
for (i in 1:294) {
    if (clusterresult$cluster[i] == 1) {
        cgroup1[, cind1] = ser[, i]
        for (j in 1:2767) {
            if (cgroup1[j, cind1] == 0) {
                cgroup1[j, cind1] = NA
            }
        }
        cind1 = cind1 + 1
    } else if (clusterresult$cluster[i] == 2) {
        cgroup2[, cind2] = ser[, i]
        for (j in 1:2767) {
            if (cgroup2[j, cind2] == 0) {
                cgroup2[j, cind2] = NA
            }
        }
        cind2 = cind2 + 1
    } else if (clusterresult$cluster[i] == 3) {
        cgroup3[, cind3] = ser[, i]
        for (j in 1:2767) {
            if (cgroup3[j, cind3] == 0) {
                cgroup3[j, cind3] = NA
            }
        }
        cind3 = cind3 + 1
    } else if (clusterresult$cluster[i] == 4) {
        cgroup4[, cind4] = ser[, i]
        for (j in 1:2767) {
            if (cgroup4[j, cind4] == 0) {
                cgroup4[j, cind4] = NA
            }
        }
        cind4 = cind4 + 1
    } else if (clusterresult$cluster[i] == 5) {
        cgroup5[, cind5] = ser[, i]
        for (j in 1:2767) {
            if (cgroup5[j, cind5] == 0) {
                cgroup5[j, cind5] = NA
            }
        }
        cind5 = cind5 + 1
    } else if (clusterresult$cluster[i] == 6) {
        cgroup6[, cind6] = ser[, i]
        for (j in 1:2767) {
            if (cgroup6[j, cind6] == 0) {
                cgroup6[j, cind6] = NA
            }
        }
        cind6 = cind6 + 1
    } else if (clusterresult$cluster[i] == 7) {
        cgroup7[, cind7] = ser[, i]
        for (j in 1:2767) {
            if (cgroup7[j, cind7] == 0) {
                cgroup7[j, cind7] = NA
            }
        }
        cind7 = cind7 + 1
    } else if (clusterresult$cluster[i] == 8) {
        cgroup8[, cind8] = ser[, i]
        for (j in 1:2767) {
            if (cgroup8[j, cind8] == 0) {
                cgroup8[j, cind8] = NA
            }
        }
        cind8 = cind8 + 1
    } else {
        print(i)
    }
}

# get median series according to ranking

medcluster1 = c()
medcluster2 = c()
medcluster3 = c()
medcluster4 = c()
medcluster5 = c()
medcluster6 = c()
medcluster7 = c()
medcluster8 = c()

for (j in 1:2767) {
    medcluster1[j] = median(cgroup2[j, ], na.rm = TRUE) * 10000
    medcluster2[j] = median(cgroup6[j, ], na.rm = TRUE) * 10000
    medcluster3[j] = median(cgroup7[j, ], na.rm = TRUE) * 10000
    medcluster4[j] = median(cgroup1[j, ], na.rm = TRUE) * 10000
    medcluster5[j] = median(cgroup3[j, ], na.rm = TRUE) * 10000
    medcluster6[j] = median(cgroup5[j, ], na.rm = TRUE) * 10000
    medcluster7[j] = median(cgroup4[j, ], na.rm = TRUE) * 10000
    medcluster8[j] = median(cgroup8[j, ], na.rm = TRUE) * 10000
}

medcluster = cbind(BBY.txt$date, medcluster1, medcluster2, medcluster3, medcluster4, medcluster5, medcluster6, medcluster7, 
    medcluster8)
medcluster = data.frame(medcluster)
colnames(medcluster) = c("Date", "Group1", "Group2", "Group3", "Group4", "Group5", "Group6", "Group7", "Group8")
medcluster$Date = as.Date(as.character(medcluster$Date), "%Y%m%d")

# plot median series according to cluster
medclustergg = ggplot(medcluster, aes(x = Date))
medclusterg2 = medclustergg + geom_line(aes(y = Group1, color = "Group 1")) + geom_line(aes(y = Group2, color = "Group 2")) + 
    geom_line(aes(y = Group3, color = "Group 3")) + geom_line(aes(y = Group4, color = "Group 4")) + geom_line(aes(y = Group5, 
    color = "Group 5")) + geom_line(aes(y = Group6, color = "Group 6")) + geom_line(aes(y = Group7, color = "Group 7")) + 
    geom_line(aes(y = Group8, color = "Group 8"))
medclusterg2 + ggtitle("Median Spread for Cluster Groups") + xlab("Time") + ylab("CDS Spread") + scale_color_manual()

# get median volatility according to ranking
sdcluster1 = matrix(NA, nrow = 2766, ncol = 196)
sdcluster2 = matrix(NA, nrow = 2766, ncol = 51)
sdcluster3 = matrix(NA, nrow = 2766, ncol = 32)
sdcluster4 = matrix(NA, nrow = 2766, ncol = 5)
sdcluster5 = matrix(NA, nrow = 2766, ncol = 5)
sdcluster6 = matrix(NA, nrow = 2766, ncol = 2)
sdcluster7 = matrix(NA, nrow = 2766, ncol = 2)
sdcluster8 = matrix(NA, nrow = 2766, ncol = 1)

for (i in 1:196) {
    for (j in 1:2766) {
        sdcluster1[j, i] = sd(cgroup2[1:(j + 1), i], na.rm = TRUE)
    }
}

for (i in 1:51) {
    for (j in 1:2766) {
        sdcluster2[j, i] = sd(cgroup6[1:(j + 1), i], na.rm = TRUE)
    }
}

for (i in 1:32) {
    for (j in 1:2766) {
        sdcluster3[j, i] = sd(cgroup7[1:(j + 1), i], na.rm = TRUE)
    }
}

for (i in 1:5) {
    for (j in 1:2766) {
        sdcluster4[j, i] = sd(cgroup1[1:(j + 1), i], na.rm = TRUE)
    }
}

for (i in 1:5) {
    for (j in 1:2766) {
        sdcluster5[j, i] = sd(cgroup3[1:(j + 1), i], na.rm = TRUE)
    }
}

for (i in 1:2) {
    for (j in 1:2766) {
        sdcluster6[j, i] = sd(cgroup5[1:(j + 1), i], na.rm = TRUE)
    }
}
for (i in 1:2) {
    for (j in 1:2766) {
        sdcluster7[j, i] = sd(cgroup4[1:(j + 1), i], na.rm = TRUE)
    }
}

for (j in 1:2766) {
    sdcluster8[j] = sd(cgroup8[1:(j + 1)], na.rm = TRUE)
}
medsdcluster1 = c()
medsdcluster2 = c()
medsdcluster3 = c()
medsdcluster4 = c()
medsdcluster5 = c()
medsdcluster6 = c()
medsdcluster7 = c()
medsdcluster8 = c()

for (j in 1:2766) {
    medsdcluster1[j] = median(sdcluster1[j, ], na.rm = TRUE)
    medsdcluster2[j] = median(sdcluster2[j, ], na.rm = TRUE)
    medsdcluster3[j] = median(sdcluster3[j, ], na.rm = TRUE)
    medsdcluster4[j] = median(sdcluster4[j, ], na.rm = TRUE)
    medsdcluster5[j] = median(sdcluster5[j, ], na.rm = TRUE)
    medsdcluster6[j] = median(sdcluster6[j, ], na.rm = TRUE)
    medsdcluster7[j] = median(sdcluster7[j, ], na.rm = TRUE)
}
medsdcluster8 = sdcluster8

# plot
datets = BBY.txt$date
datets = datets[-1]
medsdcluster = cbind(datets, medsdcluster1, medsdcluster2, medsdcluster3, medsdcluster4, medsdcluster5, medsdcluster6, 
    medsdcluster7, medsdcluster8)
medsdcluster = data.frame(medsdcluster)
colnames(medsdcluster) = c("Date", "Group1", "Group2", "Group3", "Group4", "Group5", "Group6", "Group7", "Group8")
medsdcluster$Date = as.Date(as.character(medsdcluster$Date), "%Y%m%d")
medsdclustergg = ggplot(medsdcluster, aes(x = Date))
medsdclusterg2 = medsdclustergg + geom_line(aes(y = Group1, color = "Group 1")) + geom_line(aes(y = Group2, color = "Group 2")) + 
    geom_line(aes(y = Group3, color = "Group 3")) + geom_line(aes(y = Group4, color = "Group 4")) + geom_line(aes(y = Group5, 
    color = "Group 5")) + geom_line(aes(y = Group6, color = "Group 6")) + geom_line(aes(y = Group7, color = "Group 7")) + 
    geom_line(aes(y = Group8, color = "Group 8"))
medsdclusterg2 + ggtitle("Median Volatility for Cluster Groups") + xlab("Time") + ylab("Volatility") + scale_color_manual()

