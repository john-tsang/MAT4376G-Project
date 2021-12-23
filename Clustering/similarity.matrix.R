#Creation of the similarity matrix based on clustering results

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

#PAM clustering

#pam.cases.time.table
#pam.cases.count.table
#pam.programs.count.table
#pam.tm5
#pam.ty5
#pam.repres

#for pam.cases.time.table
mat.1 <- is.wholenumber(sqrt(pam.cases.time.table[,1]%*%t(pam.cases.time.table[,1])))*1
mat.2 <- is.wholenumber(sqrt(pam.cases.time.table[,2]%*%t(pam.cases.time.table[,2])))*1
mat.3 <- is.wholenumber(sqrt(pam.cases.time.table[,3]%*%t(pam.cases.time.table[,3])))*1
mat.4 <- is.wholenumber(sqrt(pam.cases.time.table[,4]%*%t(pam.cases.time.table[,4])))*1
mat.5 <- is.wholenumber(sqrt(pam.cases.time.table[,5]%*%t(pam.cases.time.table[,5])))*1
mat.6 <- is.wholenumber(sqrt(pam.cases.time.table[,6]%*%t(pam.cases.time.table[,6])))*1

mat.pam.cases.time.table <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for pam.cases.count.table

mat.1 <- is.wholenumber(sqrt(pam.cases.count.table[,1]%*%t(pam.cases.count.table[,1])))*1
mat.2 <- is.wholenumber(sqrt(pam.cases.count.table[,2]%*%t(pam.cases.count.table[,2])))*1
mat.3 <- is.wholenumber(sqrt(pam.cases.count.table[,3]%*%t(pam.cases.count.table[,3])))*1
mat.4 <- is.wholenumber(sqrt(pam.cases.count.table[,4]%*%t(pam.cases.count.table[,4])))*1
mat.5 <- is.wholenumber(sqrt(pam.cases.count.table[,5]%*%t(pam.cases.count.table[,5])))*1
mat.6 <- is.wholenumber(sqrt(pam.cases.count.table[,6]%*%t(pam.cases.count.table[,6])))*1

mat.pam.cases.count.table <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for pam.programs.count.table

mat.1 <- is.wholenumber(sqrt(pam.programs.count.table[,1]%*%t(pam.programs.count.table[,1])))*1
mat.2 <- is.wholenumber(sqrt(pam.programs.count.table[,2]%*%t(pam.programs.count.table[,2])))*1
mat.3 <- is.wholenumber(sqrt(pam.programs.count.table[,3]%*%t(pam.programs.count.table[,3])))*1
mat.4 <- is.wholenumber(sqrt(pam.programs.count.table[,4]%*%t(pam.programs.count.table[,4])))*1
mat.5 <- is.wholenumber(sqrt(pam.programs.count.table[,5]%*%t(pam.programs.count.table[,5])))*1
mat.6 <- is.wholenumber(sqrt(pam.programs.count.table[,6]%*%t(pam.programs.count.table[,6])))*1

mat.pam.programs.count.table <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#pam.tm5

mat.1 <- is.wholenumber(sqrt(pam.tm5[,1]%*%t(pam.tm5[,1])))*1
mat.2 <- is.wholenumber(sqrt(pam.tm5[,2]%*%t(pam.tm5[,2])))*1
mat.3 <- is.wholenumber(sqrt(pam.tm5[,3]%*%t(pam.tm5[,3])))*1
mat.4 <- is.wholenumber(sqrt(pam.tm5[,4]%*%t(pam.tm5[,4])))*1
mat.5 <- is.wholenumber(sqrt(pam.tm5[,5]%*%t(pam.tm5[,5])))*1
mat.6 <- is.wholenumber(sqrt(pam.tm5[,6]%*%t(pam.tm5[,6])))*1

mat.pam.tm5 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#pam.ty5

mat.1 <- is.wholenumber(sqrt(pam.ty5[,1]%*%t(pam.ty5[,1])))*1
mat.2 <- is.wholenumber(sqrt(pam.ty5[,2]%*%t(pam.ty5[,2])))*1
mat.3 <- is.wholenumber(sqrt(pam.ty5[,3]%*%t(pam.ty5[,3])))*1
mat.4 <- is.wholenumber(sqrt(pam.ty5[,4]%*%t(pam.ty5[,4])))*1
mat.5 <- is.wholenumber(sqrt(pam.ty5[,5]%*%t(pam.ty5[,5])))*1
mat.6 <- is.wholenumber(sqrt(pam.ty5[,6]%*%t(pam.ty5[,6])))*1

mat.pam.ty5 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#pam.repres

mat.1 <- is.wholenumber(sqrt(pam.repres[,1]%*%t(pam.repres[,1])))*1
mat.2 <- is.wholenumber(sqrt(pam.repres[,2]%*%t(pam.repres[,2])))*1
mat.3 <- is.wholenumber(sqrt(pam.repres[,3]%*%t(pam.repres[,3])))*1
mat.4 <- is.wholenumber(sqrt(pam.repres[,4]%*%t(pam.repres[,4])))*1
mat.5 <- is.wholenumber(sqrt(pam.repres[,5]%*%t(pam.repres[,5])))*1
mat.6 <- is.wholenumber(sqrt(pam.repres[,6]%*%t(pam.repres[,6])))*1

mat.pam.repres <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#final pam similarity matrix

mat.pam <- (mat.pam.cases.time.table + mat.pam.cases.count.table + 
              mat.pam.programs.count.table + mat.pam.tm5 + mat.pam.ty5 +
              mat.pam.repres)*1/6

#Hierarchical clustering 

#result1_mat
#result2_mat
#result3_mat
#result4_mat
#result5_mat
#result6_mat

#for result1_mat
mat.1 <- is.wholenumber(sqrt(result1_mat[,1]%*%t(result1_mat[,1])))*1
mat.2 <- is.wholenumber(sqrt(result1_mat[,2]%*%t(result1_mat[,2])))*1
mat.3 <- is.wholenumber(sqrt(result1_mat[,3]%*%t(result1_mat[,3])))*1
mat.4 <- is.wholenumber(sqrt(result1_mat[,4]%*%t(result1_mat[,4])))*1
mat.5 <- is.wholenumber(sqrt(result1_mat[,5]%*%t(result1_mat[,5])))*1
mat.6 <- is.wholenumber(sqrt(result1_mat[,6]%*%t(result1_mat[,6])))*1

mat.hier.1 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for result2_mat
mat.1 <- is.wholenumber(sqrt(result2_mat[,1]%*%t(result2_mat[,1])))*1
mat.2 <- is.wholenumber(sqrt(result2_mat[,2]%*%t(result2_mat[,2])))*1
mat.3 <- is.wholenumber(sqrt(result2_mat[,3]%*%t(result2_mat[,3])))*1
mat.4 <- is.wholenumber(sqrt(result2_mat[,4]%*%t(result2_mat[,4])))*1
mat.5 <- is.wholenumber(sqrt(result2_mat[,5]%*%t(result2_mat[,5])))*1
mat.6 <- is.wholenumber(sqrt(result2_mat[,6]%*%t(result2_mat[,6])))*1

mat.hier.2 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for result3_mat
mat.1 <- is.wholenumber(sqrt(result3_mat[,1]%*%t(result3_mat[,1])))*1
mat.2 <- is.wholenumber(sqrt(result3_mat[,2]%*%t(result3_mat[,2])))*1
mat.3 <- is.wholenumber(sqrt(result3_mat[,3]%*%t(result3_mat[,3])))*1
mat.4 <- is.wholenumber(sqrt(result3_mat[,4]%*%t(result3_mat[,4])))*1
mat.5 <- is.wholenumber(sqrt(result3_mat[,5]%*%t(result3_mat[,5])))*1
mat.6 <- is.wholenumber(sqrt(result3_mat[,6]%*%t(result3_mat[,6])))*1

mat.hier.3 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for result4_mat
mat.1 <- is.wholenumber(sqrt(result4_mat[,1]%*%t(result4_mat[,1])))*1
mat.2 <- is.wholenumber(sqrt(result4_mat[,2]%*%t(result4_mat[,2])))*1
mat.3 <- is.wholenumber(sqrt(result4_mat[,3]%*%t(result4_mat[,3])))*1
mat.4 <- is.wholenumber(sqrt(result4_mat[,4]%*%t(result4_mat[,4])))*1
mat.5 <- is.wholenumber(sqrt(result4_mat[,5]%*%t(result4_mat[,5])))*1
mat.6 <- is.wholenumber(sqrt(result4_mat[,6]%*%t(result4_mat[,6])))*1

mat.hier.4 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for result5_mat
mat.1 <- is.wholenumber(sqrt(result5_mat[,1]%*%t(result5_mat[,1])))*1
mat.2 <- is.wholenumber(sqrt(result5_mat[,2]%*%t(result5_mat[,2])))*1
mat.3 <- is.wholenumber(sqrt(result5_mat[,3]%*%t(result5_mat[,3])))*1
mat.4 <- is.wholenumber(sqrt(result5_mat[,4]%*%t(result5_mat[,4])))*1
mat.5 <- is.wholenumber(sqrt(result5_mat[,5]%*%t(result5_mat[,5])))*1
mat.6 <- is.wholenumber(sqrt(result5_mat[,6]%*%t(result5_mat[,6])))*1

mat.hier.5 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for result6_mat
mat.1 <- is.wholenumber(sqrt(result6_mat[,1]%*%t(result6_mat[,1])))*1
mat.2 <- is.wholenumber(sqrt(result6_mat[,2]%*%t(result6_mat[,2])))*1
mat.3 <- is.wholenumber(sqrt(result6_mat[,3]%*%t(result6_mat[,3])))*1
mat.4 <- is.wholenumber(sqrt(result6_mat[,4]%*%t(result6_mat[,4])))*1
mat.5 <- is.wholenumber(sqrt(result6_mat[,5]%*%t(result6_mat[,5])))*1
mat.6 <- is.wholenumber(sqrt(result6_mat[,6]%*%t(result6_mat[,6])))*1

mat.hier.6 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#Final Hierarchical clustering similarity matrix

mat.hier <-(mat.hier.1+mat.hier.2+mat.hier.3+mat.hier.4+mat.hier.5+mat.hier.6)*1/6

#K Means Clustering

#k.means.cases.time.table
#k.means.cases.count.table
#k.means.programs.count.table
#k.means.tm5
#k.means.ty5
#k.means.repres

#for k.means.cases.time.table
mat.1 <- is.wholenumber(sqrt(k.means.cases.time.table[,1]%*%t(k.means.cases.time.table[,1])))*1
mat.2 <- is.wholenumber(sqrt(k.means.cases.time.table[,2]%*%t(k.means.cases.time.table[,2])))*1
mat.3 <- is.wholenumber(sqrt(k.means.cases.time.table[,3]%*%t(k.means.cases.time.table[,3])))*1
mat.4 <- is.wholenumber(sqrt(k.means.cases.time.table[,4]%*%t(k.means.cases.time.table[,4])))*1
mat.5 <- is.wholenumber(sqrt(k.means.cases.time.table[,5]%*%t(k.means.cases.time.table[,5])))*1
mat.6 <- is.wholenumber(sqrt(k.means.cases.time.table[,6]%*%t(k.means.cases.time.table[,6])))*1

mat.k.means.1 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for k.means.cases.count.table
mat.1 <- is.wholenumber(sqrt(k.means.cases.count.table[,1]%*%t(k.means.cases.count.table[,1])))*1
mat.2 <- is.wholenumber(sqrt(k.means.cases.count.table[,2]%*%t(k.means.cases.count.table[,2])))*1
mat.3 <- is.wholenumber(sqrt(k.means.cases.count.table[,3]%*%t(k.means.cases.count.table[,3])))*1
mat.4 <- is.wholenumber(sqrt(k.means.cases.count.table[,4]%*%t(k.means.cases.count.table[,4])))*1
mat.5 <- is.wholenumber(sqrt(k.means.cases.count.table[,5]%*%t(k.means.cases.count.table[,5])))*1
mat.6 <- is.wholenumber(sqrt(k.means.cases.count.table[,6]%*%t(k.means.cases.count.table[,6])))*1

mat.k.means.2 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for k.means.programs.count.table
mat.1 <- is.wholenumber(sqrt(k.means.programs.count.table[,1]%*%t(k.means.programs.count.table[,1])))*1
mat.2 <- is.wholenumber(sqrt(k.means.programs.count.table[,2]%*%t(k.means.programs.count.table[,2])))*1
mat.3 <- is.wholenumber(sqrt(k.means.programs.count.table[,3]%*%t(k.means.programs.count.table[,3])))*1
mat.4 <- is.wholenumber(sqrt(k.means.programs.count.table[,4]%*%t(k.means.programs.count.table[,4])))*1
mat.5 <- is.wholenumber(sqrt(k.means.programs.count.table[,5]%*%t(k.means.programs.count.table[,5])))*1
mat.6 <- is.wholenumber(sqrt(k.means.programs.count.table[,6]%*%t(k.means.programs.count.table[,6])))*1

mat.k.means.3 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for k.means.tm5
mat.1 <- is.wholenumber(sqrt(k.means.tm5[,1]%*%t(k.means.tm5[,1])))*1
mat.2 <- is.wholenumber(sqrt(k.means.tm5[,2]%*%t(k.means.tm5[,2])))*1
mat.3 <- is.wholenumber(sqrt(k.means.tm5[,3]%*%t(k.means.tm5[,3])))*1
mat.4 <- is.wholenumber(sqrt(k.means.tm5[,4]%*%t(k.means.tm5[,4])))*1
mat.5 <- is.wholenumber(sqrt(k.means.tm5[,5]%*%t(k.means.tm5[,5])))*1
mat.6 <- is.wholenumber(sqrt(k.means.tm5[,6]%*%t(k.means.tm5[,6])))*1

mat.k.means.4 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for k.means.ty5
mat.1 <- is.wholenumber(sqrt(k.means.ty5[,1]%*%t(k.means.ty5[,1])))*1
mat.2 <- is.wholenumber(sqrt(k.means.ty5[,2]%*%t(k.means.ty5[,2])))*1
mat.3 <- is.wholenumber(sqrt(k.means.ty5[,3]%*%t(k.means.ty5[,3])))*1
mat.4 <- is.wholenumber(sqrt(k.means.ty5[,4]%*%t(k.means.ty5[,4])))*1
mat.5 <- is.wholenumber(sqrt(k.means.ty5[,5]%*%t(k.means.ty5[,5])))*1
mat.6 <- is.wholenumber(sqrt(k.means.ty5[,6]%*%t(k.means.ty5[,6])))*1

mat.k.means.5 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#for k.means.repres
mat.1 <- is.wholenumber(sqrt(k.means.repres[,1]%*%t(k.means.repres[,1])))*1
mat.2 <- is.wholenumber(sqrt(k.means.repres[,2]%*%t(k.means.repres[,2])))*1
mat.3 <- is.wholenumber(sqrt(k.means.repres[,3]%*%t(k.means.repres[,3])))*1
mat.4 <- is.wholenumber(sqrt(k.means.repres[,4]%*%t(k.means.repres[,4])))*1
mat.5 <- is.wholenumber(sqrt(k.means.repres[,5]%*%t(k.means.repres[,5])))*1
mat.6 <- is.wholenumber(sqrt(k.means.repres[,6]%*%t(k.means.repres[,6])))*1

mat.k.means.6 <-(mat.1+mat.2+mat.3+mat.4+mat.5+mat.6)*1/6

#Final K Means Similarity Matrix

mat.k.means <-(mat.k.means.1+mat.k.means.2+mat.k.means.3+mat.k.means.4
               +mat.k.means.5+mat.k.means.6)*1/6



#Final Similarity Matrix

mat <- (mat.pam + mat.hier + mat.k.means)*1/3



test <- mat < 0.35 | mat > 0.75
sum(test)
n = 212*212

#the percentage of time the value is between 0.35 and 0.75
(n-sum(test))/n*100

#OK
