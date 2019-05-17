######################################################################
# (i) Reduced dataset containing 10 principal components: ############
######################################################################

pca_ten = prcomp(df_for_pca, scale = TRUE)
# names(pca_ten)
#pca_ten

# ----------------------------------------------------------------------------------------------------------------

# Getting all the loading vectors:
ten_comps_PCA <- pca_ten$rotation[,c("PC1", "PC2","PC3","PC4","PC5", "PC6","PC7", "PC8", "PC9", "PC10")] #extarcting only the first 10 PRIN. COMPONENTS
#ten_comps_PCA

matrix_df_for_pca <- as.matrix(df_for_pca)
# matrix_df_for_pca

matrix_pca_ten_comps <- matrix_df_for_pca %*% ten_comps_PCA
pca_ten_comps_df = as.data.frame(matrix_pca_ten_comps)
pca_ten_comps_df <- cbind(pca_ten_comps_df, Class = plant_data$Class)
write.csv(pca_ten_comps_df, "3-f-ci-PCA_ten_Components.csv")
# View(pca_ten_comps_df)




#######################################################################
# (ii) Dataset after deletion of instances and attributes: ############
#######################################################################

replace_w_mean <- function (dataset){
  plant_data_w_Mean <- dataset %>% mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))
  return(plant_data_w_Mean)
}

plant_data_nomiss_mean_replaced = replace_w_mean(plant_data_cleaned_no_missing)
plant_data_nomiss_mean_replaced$Class = plant_data$Class
# summary(plant_data_nomiss_mean_replaced)
write.csv(plant_data_nomiss_mean_replaced, "3-f-cii-plant_data_noNA.csv")

#######################################################################
# (iii) Dataset after filling in missing values: ######################
#######################################################################

# (a) with zero:
# View(plant_data_w_zero)
plant_data_w_zero_2c <- as.data.frame(plant_data_w_zero)
plant_data_w_mean_2c$Class <- plant_data$Class


# (b) with mean:
# View(plant_data_w_mean)

plant_data_w_mean_2c <- as.data.frame(plant_data_w_mean)
plant_data_w_mean_2c$Class <- plant_data$Class
# View(plant_data_w_mean_2c)

# (c) with median:
# View(plant_data_w_median)

plant_data_w_median_2c <- as.data.frame(plant_data_w_median)
plant_data_w_median_2c$Class <- plant_data$Class
# View(plant_data_w_median_2c[,2:19])


# ----------------------------------------------------------------------------------------------------
# ---------------------------- K means on PCA 10 Components ------------------------------------------
# ----------------------------------------------------------------------------------------------------

km_i= kmeans(pca_ten_comps_df[, 1:10], 5, algorithm = c("MacQueen"), nstart = 50, iter.max=100) #applies k-means with 5 clusters and 100 iterations

pca_ten_comps_df$KM5= km_i$cluster #We are keeping the clustering results (i.e. the cluster with highest prob)
table_km_i = table(pca_ten_comps_df$Class, pca_ten_comps_df$KM5) #note how clusters are not aligned
#table_km_i

table_km_i_max = maximise_diag(table_km_i)
#table_km_2a_max

km_i_conf_mat_extract=table_km_i_max[[1]]

TP_km_i = sum(diag(km_i_conf_mat_extract))/sum(km_i_conf_mat_extract)

col_max_km_i = apply(km_i_conf_mat_extract, 2, max);
Purity_km_i = sum(col_max_km_i)/sum(km_i_conf_mat_extract)


# ----------------------------------------------------------------------------------------------------
# ---------------------------- K means on  plant_data_nomiss_mean_replaced ---------------------------
# ----------------------------------------------------------------------------------------------------

km_ii= kmeans(plant_data_nomiss_mean_replaced[, 2:18], 5, algorithm = c("MacQueen"), nstart = 50, iter.max=100) #applies k-means with 5 clusters and 100 iterations

plant_data_nomiss_mean_replaced$KM5= km_ii$cluster #We are keeping the clustering results (i.e. the cluster with highest prob)
table_km_ii = table(plant_data_nomiss_mean_replaced$Class, plant_data_nomiss_mean_replaced$KM5) #note how clusters are not aligned
#table_km_ii

table_km_ii_max = maximise_diag(table_km_ii)
#table_km_2a_max

km_ii_conf_mat_extract=table_km_ii_max[[1]]

TP_km_ii = sum(diag(km_ii_conf_mat_extract))/sum(km_ii_conf_mat_extract)

col_max_km_ii = apply(km_ii_conf_mat_extract, 2, max);
Purity_km_ii = sum(col_max_km_ii)/sum(km_ii_conf_mat_extract)


# ----------------------------------------------------------------------------------------------------
# ---------------------------- K means on  plant data zero replaced ----------------------------------
# ----------------------------------------------------------------------------------------------------

km_iiia= kmeans(plant_data_w_zero_2c[, 2:19], 5, algorithm = c("MacQueen"), nstart = 50, iter.max=100) #applies k-means with 5 clusters and 100 iterations

plant_data_w_zero_2c$KM5= km_iiia$cluster #We are keeping the clustering results (i.e. the cluster with highest prob)
table_km_iiia = table(plant_data_w_zero_2c$Class, plant_data_w_zero_2c$KM5) #note how clusters are not aligned
#table_km_iiia

table_km_iiia_max = maximise_diag(table_km_iiia)
#table_km_iiia_max

km_iiia_conf_mat_extract=table_km_iiia_max[[1]]

TP_km_iiia = sum(diag(km_iiia_conf_mat_extract))/sum(km_iiia_conf_mat_extract)

col_max_km_iiia = apply(km_iiia_conf_mat_extract, 2, max);
Purity_km_iiia = sum(col_max_km_iiia)/sum(km_iiia_conf_mat_extract)

# ----------------------------------------------------------------------------------------------------
# ---------------------------- K means on  plant_data with mean replaced -----------------------------
# ----------------------------------------------------------------------------------------------------

km_iiib= kmeans(plant_data_w_mean_2c[, 2:19], 5, algorithm = c("MacQueen"), nstart = 50, iter.max=100) #applies k-means with 5 clusters and 100 iterations

plant_data_w_mean_2c$KM5= km_iiib$cluster #We are keeping the clustering results (i.e. the cluster with highest prob)
table_km_iiib = table(plant_data_w_mean_2c$Class, plant_data_w_mean_2c$KM5) #note how clusters are not aligned
#table_km_iiia

table_km_iiib_max = maximise_diag(table_km_iiib)
#table_km_iiia_max

km_iiib_conf_mat_extract=table_km_iiib_max[[1]]

TP_km_iiib = sum(diag(km_iiib_conf_mat_extract))/sum(km_iiib_conf_mat_extract)

col_max_km_iiib = apply(km_iiib_conf_mat_extract, 2, max);
Purity_km_iiib = sum(col_max_km_iiib)/sum(km_iiib_conf_mat_extract)

# ----------------------------------------------------------------------------------------------------
# ---------------------------- K means on  plant_data with median replaced ---------------------------
# ----------------------------------------------------------------------------------------------------

km_iiic= kmeans(plant_data_w_median_2c[, 2:19], 5, algorithm = c("MacQueen"), nstart = 50, iter.max=100) #applies k-means with 5 clusters and 100 iterations

plant_data_w_median_2c$KM5= km_iiic$cluster #We are keeping the clustering results (i.e. the cluster with highest prob)
table_km_iiic = table(plant_data_w_median_2c$Class, plant_data_w_median_2c$KM5) #note how clusters are not aligned
#table_km_iiia

table_km_iiic_max = maximise_diag(table_km_iiic)
#table_km_iiia_max

km_iiic_conf_mat_extract=table_km_iiic_max[[1]]

TP_km_iiic = sum(diag(km_iiic_conf_mat_extract))/sum(km_iiic_conf_mat_extract)

col_max_km_iiic = apply(km_iiic_conf_mat_extract, 2, max);
Purity_km_iiic = sum(col_max_km_iiic)/sum(km_iiic_conf_mat_extract)

#-------------------------------------------------------------------------------------------------------------------------
# --------------------- DATAFRAME containing True Positive Rates and Purity Rates ----------------------------------------
#-------------------------------------------------------------------------------------------------------------------------

df_2c_measures <- data.frame("Dataset" = c("10 Prin Comps", "With no NA", "NA replaced with zero", "NA replaced with mean", "NA replaced with median"), "TP Rate" = c(TP_km_i, TP_km_ii, TP_km_iiia, TP_km_iiib, TP_km_iiic), "Purity Rate" = c(Purity_km_i, Purity_km_ii, Purity_km_iiia, Purity_km_iiib, Purity_km_iiic))
write.csv(df_2c_measures, "Part-2-C-TP_Purity_measures.csv")
#df_2c_measures

#-------------------------------------------------------------------------------------------------------------------------
#-----------------------------  B  O  X    P  L  O  T  S  ----------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------

# ------------------ BOX PLOT FOR PCA- 10 -------------------------------------------------------------
boxplot(pca_ten_comps_df[1:10],main="Whole Data")
boxplot(df_clust_2a[pca_ten_comps_df$KM5==1,1:10],main="Cluster 1")
boxplot(df_clust_2a[pca_ten_comps_df$KM5==2,1:10],main="Cluster 2")
boxplot(df_clust_2a[pca_ten_comps_df$KM5==3,1:10],main="Cluster 3")
boxplot(df_clust_2a[pca_ten_comps_df$KM5==4,1:10],main="Cluster 4")
boxplot(df_clust_2a[pca_ten_comps_df$KM5==5,1:10],main="Cluster 5")

# ------------------ BOX PLOT FOR Reduced dataset no NA ------------------------------------------------
boxplot(plant_data_nomiss_mean_replaced[,2:18],main="Whole Data")
boxplot(df_clust_2a[plant_data_nomiss_mean_replaced$KM5==1,2:18],main="Cluster 1")
boxplot(df_clust_2a[plant_data_nomiss_mean_replaced$KM5==2,2:18],main="Cluster 2")
boxplot(df_clust_2a[plant_data_nomiss_mean_replaced$KM5==3,2:18],main="Cluster 3")
boxplot(df_clust_2a[plant_data_nomiss_mean_replaced$KM5==4,2:18],main="Cluster 4")
boxplot(df_clust_2a[plant_data_nomiss_mean_replaced$KM5==5,2:18],main="Cluster 5")

# ------------------ BOX PLOT FOR replaced with zero ---------------------------------------------------
boxplot(plant_data_w_zero_2c[,2:19],main="Whole Data")
boxplot(df_clust_2a[plant_data_w_zero_2c$KM5==1,2:19],main="Cluster 1")
boxplot(df_clust_2a[plant_data_w_zero_2c$KM5==2,2:19],main="Cluster 2")
boxplot(df_clust_2a[plant_data_w_zero_2c$KM5==3,2:19],main="Cluster 3")
boxplot(df_clust_2a[plant_data_w_zero_2c$KM5==4,2:19],main="Cluster 4")
boxplot(df_clust_2a[plant_data_w_zero_2c$KM5==5,2:19],main="Cluster 5")

# ------------------ BOX PLOT FOR replaced with mean ---------------------------------------------------
boxplot(plant_data_w_mean_2c[,2:19],main="Whole Data")
boxplot(df_clust_2a[plant_data_w_mean_2c$KM5==1,2:19],main="Cluster 1")
boxplot(df_clust_2a[plant_data_w_mean_2c$KM5==2,2:19],main="Cluster 2")
boxplot(df_clust_2a[plant_data_w_mean_2c$KM5==3,2:19],main="Cluster 3")
boxplot(df_clust_2a[plant_data_w_mean_2c$KM5==4,2:19],main="Cluster 4")
boxplot(df_clust_2a[plant_data_w_mean_2c$KM5==5,2:19],main="Cluster 5")

# ------------------ BOX PLOT FOR median ---------------------------------------------------------------
boxplot(plant_data_w_median_2c[,2:19],main="Whole Data")
boxplot(df_clust_2a[plant_data_w_median_2c$KM5==1,2:19],main="Cluster 1")
boxplot(df_clust_2a[plant_data_w_median_2c$KM5==2,2:19],main="Cluster 2")
boxplot(df_clust_2a[plant_data_w_median_2c$KM5==3,2:19],main="Cluster 3")
boxplot(df_clust_2a[plant_data_w_median_2c$KM5==4,2:19],main="Cluster 4")
boxplot(df_clust_2a[plant_data_w_median_2c$KM5==5,2:19],main="Cluster 5")

# ------------------------------------------------------------------------------------------------------

