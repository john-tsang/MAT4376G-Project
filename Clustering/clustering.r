library(tidyverse)
library(factoextra)
library(jpeg)
library(ggpubr)


library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)
library(dendextend)


cases.count.table$MissionTitleE <- trimws(cases.count.table$MissionTitleE, which=c("left"))
cases.count.table$MissionTitleE <- as.factor(cases.count.table$MissionTitleE)
cases.count.table <- cases.count.table %>% arrange(MissionTitleE) 

cases.time.table$MissionTitleE <- trimws(cases.time.table$MissionTitleE, which=c("left"))
cases.time.table$MissionTitleE <- as.factor(cases.time.table$MissionTitleE)
cases.time.table <- cases.time.table %>% arrange(MissionTitleE)

programs.count.table$MissionTitleE <- trimws(programs.count.table$MissionTitleE, which=c("left"))
programs.count.table$MissionTitleE <- as.factor(programs.count.table$MissionTitleE)
programs.count.table <- programs.count.table %>% arrange(MissionTitleE)

tm5_mod <- TM5 %>% filter(MinutesofHighestMonth != 0)
tm5_mod$MissionTitleE <- trimws(tm5_mod$MissionTitleE, which=c("left"))
tm5_mod$MissionTitleE <- as.factor(tm5_mod$MissionTitleE)
tm5_mod <- tm5_mod %>% arrange(MissionTitleE)


ty5_mod <- TY5 %>% filter(MinutesofHighestYear!= 0)
ty5_mod$MissionTitleE <- trimws(ty5_mod$MissionTitleE, which=c("left"))
ty5_mod$MissionTitleE <- as.factor(ty5_mod$MissionTitleE)
ty5_mod <- ty5_mod %>% arrange(MissionTitleE)

#Removing missions that are not present in all the 6 datasets
cases.count.table<-subset(cases.count.table, MissionTitleE!="Jackson" &
                  MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
                & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
                & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
                & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
                & MissionTitleE!="Bosphorus")
cases.time.table<-subset(cases.time.table, MissionTitleE!="Jackson" &
                            MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
                          & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
                          & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
                          & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
                          & MissionTitleE!="Bosphorus")
programs.count.table<-subset(programs.count.table, MissionTitleE!="Jackson" &
                            MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
                          & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
                          & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
                          & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
                          & MissionTitleE!="Bosphorus")

tm5_mod<-subset(tm5_mod, MissionTitleE!="Jackson" &
                  MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
                & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
                & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
                & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
                & MissionTitleE!="Bosphorus")
tm5_mod <- tm5_mod[-c(166),]
ty5_mod<-subset(ty5_mod, MissionTitleE!="Jackson" &
              MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
            & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
            & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
            & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
            & MissionTitleE!="Bosphorus")
repres<-subset(repres, MissionTitleE!="Jackson" &
                  MissionTitleE!="Masqat" & MissionTitleE!="Ammertuma"
                & MissionTitleE!="Asbi" & MissionTitleE!="Malkajgiri"
                & MissionTitleE!="Ulsan" & MissionTitleE!="Antananarivo"
                & MissionTitleE!="Asmara" & MissionTitleE!="Sitka"
                & MissionTitleE!="Bosphorus")


save(cases.count.table,cases.time.table,programs.count.table,tm5_mod,ty5_mod,file="datasets_cleaned.RData")



#Clustering Function
hier_clust <- function(seq,clust,res){
  for(val in seq){
    cut <- cutree(clust, k=val)
    col_name <- paste("hier_clust_k",val,sep="")
    res <- res %>% mutate(!!col_name := cut)
  }
  return(res)
}



#dataset 1 - cases.count,table
gower_dist <- cluster::daisy(cases.count.table,metric="gower")
gower_mat <- as.matrix(gower_dist)
clust1 <- hclust(gower_dist,method="complete")


result1 <- cases.count.table %>% select(MissionTitleE)
range <- c(4,5,6,7,8,10)
result1 <- hier_clust(range,clust1,result1)

result1_mat <- data.matrix(result1[,-1])
row.names(result1_mat) <- result1$MissionTitleE

#dataset 2 - cases.time.table
gower_dist <- cluster::daisy(cases.time.table,metric="gower")
clust2 <- hclust(gower_dist,method="complete")

result2 <- cases.time.table %>% select(MissionTitleE)
result2 <- hier_clust(range,clust2,result2)

result2_mat <- data.matrix(result2[,-1])
row.names(result2_mat) <- result2$MissionTitleE

#dataset 3 - programs.count.table
gower_dist <- cluster::daisy(programs.count.table,metric="gower")
clust3 <- hclust(gower_dist,method="complete")

result3 <- programs.count.table%>% select(MissionTitleE)
result3 <- hier_clust(range,clust3,result3)

result3_mat <- data.matrix(result3[,-1])
row.names(result3_mat) <- result3$MissionTitleE


#dataset 4 - TM5
gower_dist <- cluster::daisy(tm5_mod,metric="gower")
clust4 <- hclust(gower_dist,method="complete")

result4 <- tm5_mod %>% select(MissionTitleE)
result4 <- hier_clust(range,clust4,result4)

result4_mat <- data.matrix(result4[,-1])
row.names(result4_mat) <- result4$MissionTitleE

#dataset 5 - TY5
gower_dist <- cluster::daisy(ty5_mod,metric="gower")
clust5 <- hclust(gower_dist,method="complete")
plot(clust5,cex=0.6,hang=-1)

result5 <- ty5_mod %>% select(MissionTitleE)
result5 <- hier_clust(range,clust5,result5)

result5_mat <- data.matrix(result5[,-1])
row.names(result5_mat) <- result5$MissionTitleE

#dataset 6 - repres
gower_dist <- cluster::daisy(repres,metric="gower")
clust6 <- hclust(gower_dist,method="complete")
plot(clust6,cex=0.6,hang=-1)

result6 <- repres %>% select(MissionTitleE)
result6 <- hier_clust(range,clust6,result6)

result6_mat <- data.matrix(result6[,-1])
row.names(result6_mat) <- result6$MissionTitleE

save(result1_mat,result2_mat,result3_mat,result4_mat,result5_mat,result6_mat,file = "clustering_results.RData")

