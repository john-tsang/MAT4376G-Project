#DBscan

library(dbscan)

kNNdistplot(mat, k=5)
abline(h=0.95, col="red")

db = dbscan(mat,eps = 0.95,minPts =5)
db

Mission <- colnames(mat)
DB.Cluster <- db$cluster

frame <- data.frame(Mission,DB.Cluster)


frame[frame$DB.Cluster == 5, ] 
