#K-means

library(stats)

#for cases.time.table
M <- c(4,5,6,8,10,15)
k.means.clusters <- matrix(0,nrow = 212,ncol = 15)

for (k in M) {
clusters <- kmeans(cases.time.table[,2:15], k)

k.means.clusters[,k] <- clusters$cluster
}

#name rows and columns and drop rows with only zeros
colnames(k.means.clusters) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(k.means.clusters) <- as.character(cases.time.table$MissionTitleE)

##### final clustering matrix for cases.time.table
k.means.cases.time.table <- k.means.clusters[, which(colSums(k.means.clusters) != 0)]

#for cases.count.table
M <- c(4,5,6,8,10,15)
k.means.clusters <- matrix(0,nrow = 212,ncol = 15)

for (k in M) {
  clusters <- kmeans(cases.count.table[,2:16], k)
  
  k.means.clusters[,k] <- clusters$cluster
}

#name rows and columns and drop rows with only zeros
colnames(k.means.clusters) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(k.means.clusters) <- as.character(cases.count.table$MissionTitleE)

##### final clustering matrix for cases.count.table
k.means.cases.count.table <- k.means.clusters[, which(colSums(k.means.clusters) != 0)]

#for programs.count.table
M <- c(4,5,6,8,10,15)
k.means.clusters <- matrix(0,nrow = 212,ncol = 15)

for (k in M) {
  clusters <- kmeans(programs.count.table[,2:15], k)
  
  k.means.clusters[,k] <- clusters$cluster
}

#name rows and columns and drop rows with only zeros
colnames(k.means.clusters) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(k.means.clusters) <- as.character(programs.count.table$MissionTitleE)

##### final clustering matrix for programs.count.table
k.means.programs.count.table <- k.means.clusters[, which(colSums(k.means.clusters) != 0)]


#for TM5
M <- c(4,5,6,8,10,15)
k.means.clusters <- matrix(0,nrow = 212,ncol = 15)

for (k in M) {
  clusters <- kmeans(TM5[,2:4], k)
  
  k.means.clusters[,k] <- clusters$cluster
}

#name rows and columns and drop rows with only zeros
colnames(k.means.clusters) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(k.means.clusters) <- as.character(TM5$MissionTitleE)

##### final clustering matrix for TM5
k.means.tm5 <- k.means.clusters[, which(colSums(k.means.clusters) != 0)]

#for TY5
M <- c(4,5,6,8,10,15)
k.means.clusters <- matrix(0,nrow = 212,ncol = 15)

for (k in M) {
  clusters <- kmeans(TY5[,2:4], k)
  
  k.means.clusters[,k] <- clusters$cluster
}

#name rows and columns and drop rows with only zeros
colnames(k.means.clusters) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(k.means.clusters) <- as.character(TY5$MissionTitleE)

##### final clustering matrix for TY5
k.means.ty5 <- k.means.clusters[, which(colSums(k.means.clusters) != 0)]


#for repres
M <- c(4,5,6,8,10,15)
k.means.clusters <- matrix(0,nrow = 212,ncol = 15)

for (k in M) {
  clusters <- kmeans(repres[,2:11], k)
  
  k.means.clusters[,k] <- clusters$cluster
}

#name rows and columns and drop rows with only zeros
colnames(k.means.clusters) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(k.means.clusters) <- as.character(repres$MissionTitleE)

##### final clustering matrix for repres
k.means.repres <- k.means.clusters[, which(colSums(k.means.clusters) != 0)]

