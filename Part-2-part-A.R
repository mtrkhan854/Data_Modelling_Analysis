library("ggplot2")
library("remotes")

library("dplyr")
#library("sparcl") # for dendogram
library("cluster") #clusplot
library("fpc") # for cluster stats
library("dendextend") #fancy dendogram
library("combinat") #maximizing diagonal


# *** df_clust_2a: USED FOR PARTS 2 AND 3 ***
# *** df_clust_2a: contains additional columns formed while clustering ***

df_clust_2a = data.frame(df_pca_w_SID_Class) #df_pca_w_SID_Class: this dataset cleaned from Leaf Weight, then mean replaced and standardized.
df_clust_2a["Class"] = plant_data["Class"]
# View(df_clust_2a)
# View(df_pca_w_SID_Class)
write.csv(df_clust_2a[2:19], "Dataset_for_2_3.csv")


#------------------------------------------------------------------------------------------------------------------------
#---------------------- K-MEANS CLUSTERING ------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

km_2a= kmeans(df_clust_2a[,2:18],5, nstart = 50, iter.max=100) #applies k-means with 5 clusters and 100 iterations

df_clust_2a$KM5= km_2a$cluster #We are keeping the clustering results (i.e. the cluster with highest prob)
table_km_2a = table(df_clust_2a$Class, df_clust_2a$KM5) #note how clusters are not aligned
#table_km_2a

table_km_2a_max = maximise_diag(table_km_2a)
#table_km_2a_max

km_conf_mat_extract=table_km_2a_max[[1]]
write.table(km_conf_mat_extract, "km_conf_mat_extract.txt", sep="\t")

TP_km_2a = sum(diag(km_conf_mat_extract))/sum(km_conf_mat_extract)

col_max_km_2a = apply(km_conf_mat_extract, 2, max);
Purity_km_2a = sum(col_max_km_2a)/sum(km_conf_mat_extract)


#---------------------- PLOTTING: -----------------------

clusplot(df_clust_2a[,2:18], km_2a$cluster, main="K-Means Clustering with 5 Clusters", 
         color = TRUE, shade = TRUE,
         labels = 4, lines = 0, cex = 1, col.p=km_2a$cluster)


pairs(df_clust_2a[,2:18],col=df_clust_2a$KM5) # in appendix

#----------------------------------------------------------------------------------------------------------------------
#--------------------------- PAM CLUSTERING ---------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------

pam_2a = pam(df_clust_2a[,2:18], 5)
#names(pam_2a)

df_clust_2a$PAM5= pam_2a$clustering #We are keeping the clustering results (i.e. the cluster with highest prob)
table_pam_2a = table(df_clust_2a$Class, df_clust_2a$PAM5) #note how clusters are not aligned
#table_pam_2a

table_pam_2a_max = maximise_diag(table_pam_2a)
table_pam_2a_max
pam_conf_mat_extract=table_pam_2a_max[[1]]

write.table(pam_conf_mat_extract, "pam_conf_mat_extract.txt", sep="\t")

TP_pam_2a = sum(diag(pam_conf_mat_extract))/sum(pam_conf_mat_extract)

col_max_pam_2a = apply(pam_conf_mat_extract, 2, max);
Purity_pam_2a = sum(col_max_pam_2a)/sum(pam_conf_mat_extract)



#---------------------- PLOTTING: -----------------------
clusplot(df_clust_2a[,2:18], pam_2a$clustering, main="PAM Clustering with 5 Clusters", 
         color = TRUE, shade = TRUE,
         labels = 4, lines = 0, cex = 1, col.p=pam_2a$clustering)
pairs(df_clust_2a[,2:18],col=df_clust_2a$PAM5) # in appendix

#------------------------------------------------------------------------------------------------------------------------
# ------------------------ HIERARCHICAL CLUSTERING ----------------------------------------------------------------------  
#------------------------------------------------------------------------------------------------------------------------

hc_2a= hclust(dist(df_clust_2a[,2:18])) #applies hierarchical clustering
df_clust_2a$HC5= cutree(hc_2a,5) #stops hierarchy at level 5 
table_hc_2a = table(df_clust_2a$Class, df_clust_2a$HC5) #show clusters class label according to clusters
#table_hc_2a

table_hc_2a_max = maximise_diag(table_hc_2a)
#table_hc_2a_max

hc_conf_mat_extract=table_hc_2a_max[[1]]
write.table(hc_conf_mat_extract, "hc_conf_mat_extract.txt", sep="\t")


TP_hc_2a = sum(diag(hc_conf_mat_extract))/sum(hc_conf_mat_extract)

col_max_hc_2a = apply(hc_conf_mat_extract, 2, max);
Purity_hc_2a = sum(col_max_hc_2a)/sum(hc_conf_mat_extract)


#---------------------- PLOTTING: -----------------------
dendro_2a <- df_clust_2a[,2:18] %>%  scale %>% 
  dist %>% hclust %>% as.dendrogram %>%
  set("branches_k_color", k=5) %>% set("branches_lwd", 1.2) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>% 
  set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))

plot(dendro_2a, main = "Dendogram with 5 Clusters", xlab="Instances", ylab="Height")
dendro_2a%>% 
  rect.dendrogram(k=5, border = 8, lty = 5, lwd = 2)

pairs(df_clust_2a[,2:18],col=df_clust_2a$HC5) # in appendix
# ------------------------------------------------------------------------------------------------------------------------


# --------------------- DATAFRAME containing True Positive Rates and Purity Rates ----------------------------------------

df_2a_measures <- data.frame("Algorithms" = c("K-means", "PAM", "Hierarchical"), "TP Rate" = c(TP_km_2a, TP_pam_2a, TP_hc_2a), "Purity Rate" = c(Purity_km_2a, Purity_pam_2a,Purity_hc_2a))
write.csv(df_2a_measures, "Part-2-a-TP_Purity_measures.csv")
#df_2a_measures

# ------------------------------------------------------------------------------------------------------------------------

# ------------------------ Maximizing Diagonal ---------------------------------------------------------------------------  


maximise_diag <- function(tab){

  permute = permn(seq(1:ncol(tab)))

  maxim_accuracy = sum(diag(tab))/sum(tab)
  maxim_t = tab

  for(i in 2:length(permute)){ #for all possible combinations

    perm = unlist(permute[i])
    temp= tab[,c(perm)]
    accuracy = sum(diag(temp))/sum(tab)


    if(accuracy>maxim_accuracy){

      maxim_accuracy = accuracy

      maxim_t = temp

    }

  }
  return(list(maxim_t,maxim_accuracy))
}


##############################################################################################################################
##############################################################################################################################
#########################                                                                            #########################
#########################          B  o  x     P L O T S   o f    C L U S T    A L G O               #########################
#########################                                                                            #########################
##############################################################################################################################
##############################################################################################################################


######################
####  2 A ############
######################

# ------------------ BOX PLOT FOR KM5 -------------------------------------------------------------
boxplot(df_clust_2a[,2:18],main="Whole Data")
boxplot(df_clust_2a[df_clust_2a$KM5==1,2:18],main="Cluster 1")
boxplot(df_clust_2a[df_clust_2a$KM5==2,2:18],main="Cluster 2")
boxplot(df_clust_2a[df_clust_2a$KM5==3,2:18],main="Cluster 3")
boxplot(df_clust_2a[df_clust_2a$KM5==4,2:18],main="Cluster 4")
boxplot(df_clust_2a[df_clust_2a$KM5==5,2:18],main="Cluster 5")

# ------------------ BOX PLOT FOR PAM5 -------------------------------------------------------------
boxplot(df_clust_2a[,2:18],main="Whole Data")
boxplot(df_clust_2a[df_clust_2a$PAM5==1,2:18],main="Cluster 1")
boxplot(df_clust_2a[df_clust_2a$PAM5==2,2:18],main="Cluster 2")
boxplot(df_clust_2a[df_clust_2a$PAM5==3,2:18],main="Cluster 3")
boxplot(df_clust_2a[df_clust_2a$PAM5==4,2:18],main="Cluster 4")
boxplot(df_clust_2a[df_clust_2a$PAM5==5,2:18],main="Cluster 5")

# ------------------ BOX PLOT FOR HC5 -------------------------------------------------------------
boxplot(df_clust_2a[,2:18],main="Whole Data")
boxplot(df_clust_2a[df_clust_2a$HC5==1,2:18],main="Cluster 1")
boxplot(df_clust_2a[df_clust_2a$HC5==2,2:18],main="Cluster 2")
boxplot(df_clust_2a[df_clust_2a$HC5==3,2:18],main="Cluster 3")
boxplot(df_clust_2a[df_clust_2a$HC5==4,2:18],main="Cluster 4")
boxplot(df_clust_2a[df_clust_2a$HC5==5,2:18],main="Cluster 5")

#-------------------------------- SOME EXTRA CODES THAT I WROTE ARE BELOW THIS - Number of Clusters were varied ------------- 

#--------------------------------------------- 2 B---------------------------------------------------------------------------

# --------------------------- 2B - K MEANS ----------------------------------------------------------------------------------

# wssplot <- function(data, nc=15){
#   wss <- (nrow(data)-1)*sum(apply(data,2,var))
#   for (i in 2:nc){
#     # set.seed(seed)
#     wss[i] <- sum(kmeans(data, centers=i, iter.max=1000, nstart = 50)$withinss)}
#   plot(1:nc, wss, type="b", xlab="Number of Clusters",
#        ylab="Within groups sum of squares")}
# 
# wssplot(df_clust_2a[,2:18], nc=60)
# #------------------------------------------------------------------------------------------------------------------------
# 
# 
# wrap <- function(i, hc, x) {
#   cl <- cutree(hc, i)
#   spl <- split(x, cl)
#   wss <- sum(sapply(spl, wss))
#   wss
# }
# 
# wss <- function(d) {
#   sum(scale(d, scale = FALSE)^2)
# }
# 
# res <- sapply(seq.int(1, nrow(data)), wrap, h = cl, x = data)
# 
# plot(seq_along(res), res, type = "b", pch = 19)
# -----------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------
#---------------------------------------- Silhouette Scores and Graphs --------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

#######################
#### K- means #########
#######################

#------------------------------------ Hartigan-Wong ---------------------------------------------------------------------
#ss_km <- silhouette(km_2a$cluster, dist(df_clust_2a[,2:18]))

# silhouette_score_km_1 <- function(k, seed_km = 1234){
#   set.seed(seed_km)
#   km_1 <- kmeans(df_clust_2a[,2:18], centers = k, nstart=25, algorithm = c("Hartigan-Wong"), iter.max = 100)
#   ss_km1 <- silhouette(km_1$cluster, dist(df_clust_2a[,2:18]))
#   mean(ss_km1[, 3])
# }
# k <- 2:20
# avg_sil <- sapply(k, silhouette_score_km_1)
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', main = 'Silhouette Measures 
#      for K-means using Hartigan-Wong Algorithm', frame=FALSE)
# 
# #------------------------------------  Lloyd/Forgy  ---------------------------------------------------------------------
# 
# silhouette_score_km_2 <- function(k, seed_km = 1234){
#   set.seed(seed_km)
#   km_2 <- kmeans(df_clust_2a[,2:18], centers = k, nstart=25, algorithm = c("Lloyd"), iter.max = 100)
#   ss_km2 <- silhouette(km_2$cluster, dist(df_clust_2a[,2:18]))
#   mean(ss_km2[, 3])
# }
# k <- 2:20
# avg_sil <- sapply(k, silhouette_score_km_2)
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', main = 'Silhouette Measures 
#      for K-means using Lloyd/Forgy Algorithm', frame=FALSE)
# 
# #------------------------------------  MacQueen  ---------------------------------------------------------------------
# 
# silhouette_score_km_3 <- function(k, seed_km = 1234){
#   set.seed(seed_km)
#   km_3 <- kmeans(df_clust_2a[,2:18], centers = k, nstart=25, algorithm = c("MacQueen"), iter.max = 100)
#   ss_km3 <- silhouette(km_3$cluster, dist(df_clust_2a[,2:18]))
#   mean(ss_km3[, 3])
# }
# k <- 2:20
# avg_sil <- sapply(k, silhouette_score_km_3)
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', main = 'Silhouette Measures 
#      for K-means using MacQueen Algorithm', frame=FALSE)
# 
# 
# #---------------------------------------------------------------------------------------------------------------------
# #---------------------------------------------------------------------------------------------------------------------
# 
# #######################
# #### H-Clust ##########
# #######################
# 
# #------------------------------------  Complete  ---------------------------------------------------------------------
# 
# 
# silhouette_score_hc_1 <- function(k){
#   #set.seed(seed_km=1234)
#   hc_1= hclust(dist(df_clust_2a[,2:18]), method = "complete") #applies hierarchical clustering
#   hcc_1= cutree(hc_1,k)
#   ss_hcc_1 <- silhouette(hcc_1, dist(df_clust_2a[,2:18]))
#   mean(ss_hcc_1[, 3])
# }
# k <- 2:20
# avg_sil <- sapply(k, silhouette_score_hc_1)
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', main = 'Silhouette Measures 
#      for H-Clustering using Complete Linkage', frame=FALSE)
# 
# 
# #------------------------------------  Average  ---------------------------------------------------------------------
# 
# silhouette_score_hc_2 <- function(k){
#   hc_2= hclust(dist(df_clust_2a[,2:18]), method = "average") #applies hierarchical clustering
#   hcc_2= cutree(hc_2,k)
#   ss_hcc_2 <- silhouette(hcc_2, dist(df_clust_2a[,2:18]))
#   mean(ss_hcc_2[, 3])
# }
# k <- 2:20
# avg_sil <- sapply(k, silhouette_score_hc_2)
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', main = 'Silhouette Measures 
#      for H-Clustering using Average Linkage', frame=FALSE)
# 
# 
# 
# #------------------------------------  Single  ---------------------------------------------------------------------
# 
# silhouette_score_hc_3 <- function(k){
#   hc_3= hclust(dist(df_clust_2a[,2:18]), method = "single") #applies hierarchical clustering
#   hcc_3= cutree(hc_3,k)
#   ss_hcc_3 <- silhouette(hcc_3, dist(df_clust_2a[,2:18]))
#   mean(ss_hcc_3[, 3])
# }
# k <- 2:20
# avg_sil <- sapply(k, silhouette_score_hc_3)
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', main = 'Silhouette Measures 
#      for H-Clustering using Single Linkage', frame=FALSE)
# 
# 
# 
# #------------------------------------  Centroid  ---------------------------------------------------------------------
# 
# silhouette_score_hc_4 <- function(k){
#   hc_4= hclust(dist(df_clust_2a[,2:18]), method = "centroid") #applies hierarchical clustering
#   hcc_4= cutree(hc_4,k)
#   ss_hcc_4 <- silhouette(hcc_4, dist(df_clust_2a[,2:18]))
#   mean(ss_hcc_4[, 3])
# }
# 
# k <- 2:20
# avg_sil <- sapply(k, silhouette_score_hc_4)
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', main = 'Silhouette Measures 
#      for H-Clustering using Centroid Linkage', frame=FALSE)
# 
# #---------------------------------------------------------------------------------------------------------------------
# #---------------------------------------------------------------------------------------------------------------------
# 
# 
# #######################
# #### PAM ##############
# #######################
# 
# 
# 
# silhouette_score_pam <- function(k){
#   pam_1 = pam(df_clust_2a[,2:18], k)
#   ss_pam1 <- silhouette(pam_1$clustering, dist(df_clust_2a[,2:18]))
#   mean(ss_pam1[, 3])
# }
# 
# k <- 2:20
# avg_sil <- sapply(k, silhouette_score_pam)
# plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', main = 'Silhouette Measures 
#      for PAM', frame=FALSE)


