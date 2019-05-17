##############################################################################################################################
##############################################################################################################################
#########################                                                                            #########################
#########################          KEEPING     CLUSTER   WIDTH   CONSTANT    AT   5                  #########################
#########################                                                                            #########################
##############################################################################################################################
##############################################################################################################################



##############################################################################################################################
###################################                                                      #####################################
###################################              EXTERNAL           MEASURES             #####################################
###################################                                                      #####################################
##############################################################################################################################

library("fpc")

#--------------------------------------- K MEANS -----------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
#set.seed(1)
km_HW <- kmeans(df_clust_2a[,2:18], centers = 5, nstart=50, algorithm = c("Hartigan-Wong"), iter.max = 100)
df_clust_2a$KM_HW = km_HW$cluster

t1 = table(df_clust_2a$Class, df_clust_2a$KM_HW)
res_1 = maximise_diag(t1)
t11 = res_1[[1]]
col_max_1 = apply(t11, 2, max)

tp_1 = sum(diag(t11))/sum(t11)
pp_1 = sum(col_max_1)/sum(t11)

#------------------------------------------------------------

km_LF <- kmeans(df_clust_2a[,2:18], centers = 5, nstart=50, algorithm = c("Lloyd"), iter.max = 100)
df_clust_2a$KM_LF = km_LF$cluster

t2 = table(df_clust_2a$Class, df_clust_2a$KM_LF)
res_2 = maximise_diag(t2)
t22 = res_2[[1]]
col_max_2 = apply(t22, 2, max)

tp_2 = sum(diag(t22))/sum(t22)
pp_2 = sum(col_max_2)/sum(t22)

#------------------------------------------------------------
km_MQ <- kmeans(df_clust_2a[,2:18], centers = 5, nstart=50, algorithm = c("MacQueen"), iter.max = 100)
df_clust_2a$KM_MQ = km_MQ$cluster

t3 = table(df_clust_2a$Class, df_clust_2a$KM_MQ)
res_3 = maximise_diag(t3)
t33 = res_3[[1]]
col_max_3 = apply(t33, 2, max)

tp_3 = sum(diag(t33))/sum(t33)
pp_3 = sum(col_max_3)/sum(t33)

#-----------------------------------------------------------------------------------------------------------------------------


#-------------------------------------- PAM ----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
pam_EUC <- pam(df_clust_2a[,2:18], metric = c("euclidean"), 5)
df_clust_2a$PAM_EUC = pam_EUC$clustering

t4 = table(df_clust_2a$Class, df_clust_2a$PAM_EUC)
res_4 = maximise_diag(t4)
t44 = res_4[[1]]
col_max_4 = apply(t44, 2, max)

tp_4 = sum(diag(t44))/sum(t44)
pp_4 = sum(col_max_4)/sum(t44)

#------------------------------------------------------------
  
pam_MAN <- pam(df_clust_2a[,2:18], metric = c("manhattan"), 5)
df_clust_2a$PAM_MAN = pam_MAN$clustering

t5 = table(df_clust_2a$Class, df_clust_2a$PAM_MAN)
res_5 = maximise_diag(t5)
t55 = res_5[[1]]
col_max_5 = apply(t55, 2, max)

tp_5 = sum(diag(t55))/sum(t55)
pp_5 = sum(col_max_5)/sum(t55)


#-----------------------------------------------------------------------------------------------------------------------------


#------------------------------------- HIERARCHICAL --------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------
hc_COM <- hclust(dist(df_clust_2a[,2:18]), method = "complete")
df_clust_2a$HC_COM = cutree(hc_COM, 5)

t6 = table(df_clust_2a$Class, df_clust_2a$HC_COM)
res_6 = maximise_diag(t6)
t66 = res_6[[1]]
col_max_6 = apply(t66, 2, max)

tp_6 = sum(diag(t66))/sum(t66)
pp_6 = sum(col_max_6)/sum(t66)

#------------------------------------------------------------

hc_AVG <- hclust(dist(df_clust_2a[,2:18]), method = "average")
df_clust_2a$HC_AVG = cutree(hc_AVG, 5)

t7 = table(df_clust_2a$Class, df_clust_2a$HC_AVG)
res_7 = maximise_diag(t7)
t77 = res_7[[1]]
col_max_7 = apply(t77, 2, max)

tp_7 = sum(diag(t77))/sum(t77)
pp_7 = sum(col_max_7)/sum(t77)

#------------------------------------------------------------

hc_SIN <- hclust(dist(df_clust_2a[,2:18]), method = "single")
df_clust_2a$HC_SIN = cutree(hc_SIN, 5)

t8 = table(df_clust_2a$Class, df_clust_2a$HC_SIN)
res_8 = maximise_diag(t8)
t88 = res_8[[1]]
col_max_8 = apply(t88, 2, max)

tp_8 = sum(diag(t88))/sum(t88)
pp_8 = sum(col_max_8)/sum(t88)

#------------------------------------------------------------------------------------------------------------------------------

df_2b_ext_measures <- data.frame("Algorithms" = c("K-means", "-", "-", "PAM","-", "Hierarchical", "-", "-"), "Types" = c("Hartigan-Wong", "Lloyd/Forgy", "MacQueen", "Euclidean", "Manhattan", "Complete", "Average", "Single") ,"TP Rate" = c(tp_1, tp_2, tp_3, tp_4, tp_5, tp_6, tp_7, tp_8), "Purity Rate" = c(pp_1, pp_2, pp_3, pp_4, pp_5, pp_6, pp_7, pp_8))
#df_2b_int_measures
write.csv(df_2b_ext_measures, "TP_Purity_measures_2b.csv")


##############################################################################################################################
###################################                                                      #####################################
###################################              INTERNAL           MEASURES             #####################################
###################################                                                      #####################################
##############################################################################################################################

distance_2b = dist(df_clust_2a[,2:18])

statistics_km_HW = cluster.stats(distance_2b, df_clust_2a$KM_HW)
statistics_km_LF = cluster.stats(distance_2b, df_clust_2a$KM_LF)
statistics_km_MQ = cluster.stats(distance_2b, df_clust_2a$KM_MQ)
statistics_pam_EUC = cluster.stats(distance_2b, df_clust_2a$PAM_EUC)
statistics_pam_MAN = cluster.stats(distance_2b, df_clust_2a$PAM_MAN)
statistics_hc_COM = cluster.stats(distance_2b, df_clust_2a$HC_COM)
statistics_hc_AVG = cluster.stats(distance_2b, df_clust_2a$HC_AVG)
statistics_hc_SIN = cluster.stats(distance_2b, df_clust_2a$HC_SIN)

# mean(statistics_hc_SIN$diameter)
# mean(statistics_hc_SIN$median.distance)
# mean(statistics_hc_SIN$average.distance)
# statistics_hc_SIN$diameter

df_2b_int_measures <- data.frame("Algorithms" = c("K-means", "-", "-", "PAM","-", "Hierarchical", "-", "-"), "Types" = c("Hartigan-Wong", "Lloyd/Forgy", "MacQueen", "Euclidean", "Manhattan", "Complete", "Average", "Single") ,
                                 "Mean Diameter" = c(mean(statistics_km_HW$diameter), mean(statistics_km_LF$diameter),mean(statistics_km_MQ$diameter),mean(statistics_pam_EUC$diameter),mean(statistics_pam_MAN$diameter),mean(statistics_hc_COM$diameter),mean(statistics_hc_AVG$diameter),mean(statistics_hc_SIN$diameter)), 
                                 "Mean of Avg Distances" = c(mean(statistics_km_HW$average.distance), mean(statistics_km_LF$average.distance),mean(statistics_km_MQ$average.distance),mean(statistics_pam_EUC$average.distance),mean(statistics_pam_MAN$average.distance),mean(statistics_hc_COM$average.distance),mean(statistics_hc_AVG$average.distance),mean(statistics_hc_SIN$average.distance)))
df_2b_int_measures
write.csv(df_2b_int_measures, "Int_measures_2b.csv")

df_stat_km_MQ <- data.frame("Algorithms" = c("K-means MacQueen", "-", "-", "-", "-"), "Cluster" = c(1,2,3,4,5), 
                                      "Diameter" = c(statistics_km_MQ$diameter[1],statistics_km_MQ$diameter[2],statistics_km_MQ$diameter[3],statistics_km_MQ$diameter[4],statistics_km_MQ$diameter[5]),
                                      "Avg Distance" = c(statistics_km_MQ$average.distance[1], statistics_km_MQ$average.distance[2], statistics_km_MQ$average.distance[3], statistics_km_MQ$average.distance[4], statistics_km_MQ$average.distance[5]))

#df_stat_km_MQ
write.csv(df_stat_km_MQ, "STAT_km_MQ_2b.csv")

df_stat_hc_SIN <- data.frame("Algorithms" = c("Hclust Single", "-", "-", "-", "-"), "Cluster" = c(1,2,3,4,5), 
                            "Diameter" = c(statistics_hc_SIN$diameter[1],statistics_hc_SIN$diameter[2],statistics_hc_SIN$diameter[3],statistics_hc_SIN$diameter[4],statistics_hc_SIN$diameter[5]),
                            "Avg Distance" = c(statistics_hc_SIN$average.distance[1], statistics_hc_SIN$average.distance[2], statistics_hc_SIN$average.distance[3], statistics_hc_SIN$average.distance[4], statistics_hc_SIN$average.distance[5]))
#df_stat_hc_SIN
write.csv(df_stat_hc_SIN, "STAT_hc_SIN_2b.csv")


######################
####  2 B ############
######################

# ------------------ BOX PLOT FOR KM - MacQueen -------------------------------------------------------------
boxplot(df_clust_2a[,2:18],main="Whole Data")
boxplot(df_clust_2a[df_clust_2a$KM_MQ==1,2:18],main="Cluster 1")
boxplot(df_clust_2a[df_clust_2a$KM_MQ==2,2:18],main="Cluster 2")
boxplot(df_clust_2a[df_clust_2a$KM_MQ==3,2:18],main="Cluster 3")
boxplot(df_clust_2a[df_clust_2a$KM_MQ==4,2:18],main="Cluster 4")
boxplot(df_clust_2a[df_clust_2a$KM_MQ==5,2:18],main="Cluster 5")

# ------------------ BOX PLOT FOR HC - S i n g l e -----------------------------------------------------------
boxplot(df_clust_2a[,2:18],main="Whole Data")
boxplot(df_clust_2a[df_clust_2a$HC_SIN==1,2:18],main="Cluster 1")
boxplot(df_clust_2a[df_clust_2a$HC_SIN==2,2:18],main="Cluster 2")
boxplot(df_clust_2a[df_clust_2a$HC_SIN==3,2:18],main="Cluster 3")
boxplot(df_clust_2a[df_clust_2a$HC_SIN==4,2:18],main="Cluster 4")
boxplot(df_clust_2a[df_clust_2a$HC_SIN==5,2:18],main="Cluster 5")
