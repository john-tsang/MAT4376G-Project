#This file uses PAM clustering
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
M <- c(4,5,6,8,10,15)
pam_cluster_list2<-matrix(0,nrow = 212,ncol = 15)

#Clusters dataset cases.time.table for values k in M

cluster.test <- daisy(cases.time.table,metric="gower")
for(k in M) {
  pam_fit <- pam(cluster.test, diss = TRUE, k)
  pam_results <- cases.time.table %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

tsne_obj <- Rtsne(cluster.test, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

pam_results <- cases.time.table %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster)
pam_cluster_list <- subset(pam_results, select = c(MissionTitleE,cluster))
pam_cluster_list2[,k] <- pam_cluster_list$cluster
}

#name rows and columns and drop rows with only zeros
colnames(pam_cluster_list2) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(pam_cluster_list2) <- as.character(cases.time.table$MissionTitleE)

##### final clustering matrix for cases.time.table
pam.cases.time.table <- pam_cluster_list2[, which(colSums(pam_cluster_list2) != 0)]

pam_cluster_list2<-matrix(0,nrow = 212,ncol = 15)

#Clusters dataset cases.count.table for values k in M

cluster.test <- daisy(cases.count.table,metric="gower")
for(k in M) {
  pam_fit <- pam(cluster.test, diss = TRUE, k)
  pam_results <- cases.count.table %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  pam_results$the_summary
  
  tsne_obj <- Rtsne(cluster.test, is_distance = TRUE)
  
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))
  
  pam_results <- cases.count.table %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster)
  pam_cluster_list <- subset(pam_results, select = c(MissionTitleE,cluster))
  pam_cluster_list2[,k] <- pam_cluster_list$cluster
}
#name rows and columns and drop rows with only zeros
colnames(pam_cluster_list2) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(pam_cluster_list2) <- as.character(cases.count.table$MissionTitleE)


#####final clustering matrix for cases.count.table
pam.cases.count.table <- pam_cluster_list2[, which(colSums(pam_cluster_list2) != 0)]

pam_cluster_list2<-matrix(0,nrow = 212,ncol = 15)

#Clusters dataset cases.count.table for values k in M

cluster.test <- daisy(programs.count.table,metric="gower")
for(k in M) {
  pam_fit <- pam(cluster.test, diss = TRUE, k)
  pam_results <- programs.count.table %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  pam_results$the_summary
  
  tsne_obj <- Rtsne(cluster.test, is_distance = TRUE)
  
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))
  
  pam_results <- programs.count.table %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster)
  pam_cluster_list <- subset(pam_results, select = c(MissionTitleE,cluster))
  pam_cluster_list2[,k] <- pam_cluster_list$cluster
}
#name rows and columns and drop rows with only zeros
colnames(pam_cluster_list2) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(pam_cluster_list2) <- as.character(programs.count.table$MissionTitleE)


#####final clustering matrix for programs.count.table
pam.programs.count.table <- pam_cluster_list2[, which(colSums(pam_cluster_list2) != 0)]

pam_cluster_list2<-matrix(0,nrow = 212,ncol = 15)

#Clusters dataset TM5 for values k in M

cluster.test <- daisy(TM5,metric="gower")
for(k in M) {
  pam_fit <- pam(cluster.test, diss = TRUE, k)
  pam_results <- TM5 %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  pam_results$the_summary
  
  tsne_obj <- Rtsne(cluster.test, is_distance = TRUE)
  
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))
  
  pam_results <- TM5 %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster)
  pam_cluster_list <- subset(pam_results, select = c(MissionTitleE,cluster))
  pam_cluster_list2[,k] <- pam_cluster_list$cluster
}
#name rows and columns and drop rows with only zeros
colnames(pam_cluster_list2) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(pam_cluster_list2) <- as.character(TM5$MissionTitleE)

#####final clustering matrix for TM5
pam.tm5 <- pam_cluster_list2[, which(colSums(pam_cluster_list2) != 0)]

pam_cluster_list2<-matrix(0,nrow = 212,ncol = 15)

#Clusters dataset TM5 for values k in M

cluster.test <- daisy(TY5,metric="gower")
for(k in M) {
  pam_fit <- pam(cluster.test, diss = TRUE, k)
  pam_results <- TY5 %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  pam_results$the_summary
  
  tsne_obj <- Rtsne(cluster.test, is_distance = TRUE)
  
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))
  
  pam_results <- TY5 %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster)
  pam_cluster_list <- subset(pam_results, select = c(MissionTitleE,cluster))
  pam_cluster_list2[,k] <- pam_cluster_list$cluster
}
#name rows and columns and drop rows with only zeros
colnames(pam_cluster_list2) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(pam_cluster_list2) <- as.character(TY5$MissionTitleE)

#####final clustering matrix for TY5
pam.ty5 <- pam_cluster_list2[, which(colSums(pam_cluster_list2) != 0)]

pam_cluster_list2<-matrix(0,nrow = 212,ncol = 15)

#Clusters dataset repres for values k in M

cluster.test <- daisy(repres,metric="gower")
for(k in M) {
  pam_fit <- pam(cluster.test, diss = TRUE, k)
  pam_results <- repres %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  pam_results$the_summary
  
  tsne_obj <- Rtsne(cluster.test, is_distance = TRUE)
  
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_fit$clustering))
  
  pam_results <- repres %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster)
  pam_cluster_list <- subset(pam_results, select = c(MissionTitleE,cluster))
  pam_cluster_list2[,k] <- pam_cluster_list$cluster
}
#name rows and columns and drop rows with only zeros
colnames(pam_cluster_list2) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15")
rownames(pam_cluster_list2) <- as.character(repres$MissionTitleE)

#####final clustering matrix for repres
pam.repres <- pam_cluster_list2[, which(colSums(pam_cluster_list2) != 0)]


#####done
