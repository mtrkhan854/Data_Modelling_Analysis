#############################################
#############    PART: 1(E)    ##############
#############################################



#plant_data <- read.csv(file="g54dma-plant-dataset.csv", header=TRUE, sep=",")

# ------------------------------------------------------------------------------------
# -------------------- Functions for Transformation  ---------------------------------
# ------------------------------------------------------------------------------------

#--------------------------------   Normalize   --------------------------------------
normalize <- function (dataset){
  replicate_dataset <- data.frame(dataset)
  
  for (j in 2: (ncol(replicate_dataset)-1)){
    
    min_val = min(replicate_dataset[,j])
    max_val = max(replicate_dataset[,j])
    range_val = max_val - min_val
    
    for (i in 1:nrow(replicate_dataset)){
      replicate_dataset[i,j] <- (replicate_dataset[i,j] - min_val)/range_val
    }
  }
  return(replicate_dataset)
}


#--------------------------------   Mean Center   ------------------------------------

mean_centering <- function (dataset){
  replicate_dataset <- data.frame(dataset)
  
  for (j in 2: (ncol(replicate_dataset)-1)){
    
    mean_val = mean(replicate_dataset[,j])
    
    for (i in 1:nrow(replicate_dataset)){
      replicate_dataset[i,j] <- (replicate_dataset[i,j] - mean_val)
    }
  }
  return(replicate_dataset)
}


#--------------------------------   Standardize  --------------------------------------

standardization <- function (dataset){
  replicate_dataset <- data.frame(dataset)
  
  for (j in 2: (ncol(replicate_dataset)-1)){
    
    mean_val = mean(replicate_dataset[,j])
    stan_dev = sd(replicate_dataset[,j])
    
    for (i in 1:nrow(replicate_dataset)){
      replicate_dataset[i,j] <- (replicate_dataset[i,j] - mean_val) / stan_dev
    }
  }
  return(replicate_dataset)
}
##########################################################################################


#----------------------  plant_data replace with zero replacement  -----------------------

plant_data_zero_normalized <- normalize(plant_data_w_zero)
# View(plant_data_zero_normalized)
plant_data_zero_mean_cen <- mean_centering(plant_data_w_zero)
# View(plant_data_zero_mean_cen)
plant_data_zero_std <- standardization(plant_data_w_zero)
# View(plant_data_zero_std)


#----------------------  plant_data replace with mean replacement  -----------------------

plant_data_mean_normalized <- normalize(plant_data_w_mean)
#View(plant_data_mean_normalized)
plant_data_mean_mean_cen <- mean_centering(plant_data_w_mean)
#View(plant_data_mean_mean_cen)
plant_data_mean_std <- standardization(plant_data_w_mean)
#View(plant_data_mean_std)

#----------------------  plant_data replace with median replacement  ---------------------

plant_data_median_normalized <- normalize(plant_data_w_median)
# View(plant_data_median_normalized)
plant_data_median_mean_cen <- mean_centering(plant_data_w_median)
# View(plant_data_median_mean_cen)
plant_data_median_std <- standardization(plant_data_w_median)
# View(plant_data_median_std)

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#-----------------------  S  U  M  M  A  R  Y      T  A  B  L  E  S  ---------------------
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------



summary_table_MC_zero <- data.frame(Attributes = c(colnames(plant_data_w_zero)[2:19]), #keeping the attribute names the same in all summary table
                                 Min. = as.numeric(sub('.*:', '', summary(plant_data_zero_mean_cen)[1,2:19])),
                                 First_Quart = as.numeric(sub('.*:', '', summary(plant_data_zero_mean_cen)[2,2:19])),
                                 Median = as.numeric(sub('.*:', '', summary(plant_data_zero_mean_cen)[3,2:19])), 
                                 Mean = as.numeric(sub('.*:', '', summary(plant_data_zero_mean_cen)[4,2:19])),
                                 Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_zero_mean_cen)[5,2:19])),
                                 Max. = as.numeric(sub('.*:', '', summary(plant_data_zero_mean_cen)[6,2:19])))

row.names(summary_table_MC_zero) <- NULL 
summary_table_MC_zero

#-------------------------------------------------------------------------------------------


summary_table_MC_mean <- data.frame(Attributes = c(colnames(plant_data_w_zero)[2:19]),
                                 Min. = as.numeric(sub('.*:', '', summary(plant_data_mean_mean_cen)[1,2:19])),
                                 First_Quart = as.numeric(sub('.*:', '', summary(plant_data_mean_mean_cen)[2,2:19])),
                                 Median = as.numeric(sub('.*:', '', summary(plant_data_mean_mean_cen)[3,2:19])), 
                                 Mean = as.numeric(sub('.*:', '', summary(plant_data_mean_mean_cen)[4,2:19])),
                                 Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_mean_mean_cen)[5,2:19])),
                                 Max. = as.numeric(sub('.*:', '', summary(plant_data_mean_mean_cen)[6,2:19])))

row.names(summary_table_MC_mean) <- NULL 
summary_table_MC_mean

#-------------------------------------------------------------------------------------------

summary_table_MC_median <- data.frame(Attributes = c(colnames(plant_data_median_mean_cen)[2:19]),
                                 Min. = as.numeric(sub('.*:', '', summary(plant_data_median_mean_cen)[1,2:19])),
                                 First_Quart = as.numeric(sub('.*:', '', summary(plant_data_median_mean_cen)[2,2:19])),
                                 Median = as.numeric(sub('.*:', '', summary(plant_data_median_mean_cen)[3,2:19])), 
                                 Mean = as.numeric(sub('.*:', '', summary(plant_data_median_mean_cen)[4,2:19])),
                                 Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_median_mean_cen)[5,2:19])),
                                 Max. = as.numeric(sub('.*:', '', summary(plant_data_median_mean_cen)[6,2:19])))

row.names(summary_table_MC_median) <- NULL 
summary_table_MC_median

#-------------------------------------------------------------------------------------------

summary_table_NORM_zero <- data.frame(Attributes = c(colnames(plant_data_zero_normalized)[2:19]),
                                 Min. = as.numeric(sub('.*:', '', summary(plant_data_zero_normalized)[1,2:19])),
                                 First_Quart = as.numeric(sub('.*:', '', summary(plant_data_zero_normalized)[2,2:19])),
                                 Median = as.numeric(sub('.*:', '', summary(plant_data_zero_normalized)[3,2:19])), 
                                 Mean = as.numeric(sub('.*:', '', summary(plant_data_zero_normalized)[4,2:19])),
                                 Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_zero_normalized)[5,2:19])),
                                 Max. = as.numeric(sub('.*:', '', summary(plant_data_zero_normalized)[6,2:19])))

row.names(summary_table_NORM_zero) <- NULL 
summary_table_NORM_zero

#-------------------------------------------------------------------------------------------

summary_table_NORM_mean <- data.frame(Attributes = c(colnames(plant_data_mean_normalized)[2:19]),
                                 Min. = as.numeric(sub('.*:', '', summary(plant_data_mean_normalized)[1,2:19])),
                                 First_Quart = as.numeric(sub('.*:', '', summary(plant_data_mean_normalized)[2,2:19])),
                                 Median = as.numeric(sub('.*:', '', summary(plant_data_mean_normalized)[3,2:19])), 
                                 Mean = as.numeric(sub('.*:', '', summary(plant_data_mean_normalized)[4,2:19])),
                                 Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_mean_normalized)[5,2:19])),
                                 Max. = as.numeric(sub('.*:', '', summary(plant_data_mean_normalized)[6,2:19])))

row.names(summary_table_NORM_mean) <- NULL 
summary_table_NORM_mean

#-------------------------------------------------------------------------------------------

summary_table_NORM_median <- data.frame(Attributes = c(colnames(plant_data_median_normalized)[2:19]),
                                 Min. = as.numeric(sub('.*:', '', summary(plant_data_median_normalized)[1,2:19])),
                                 First_Quart = as.numeric(sub('.*:', '', summary(plant_data_median_normalized)[2,2:19])),
                                 Median = as.numeric(sub('.*:', '', summary(plant_data_median_normalized)[3,2:19])), 
                                 Mean = as.numeric(sub('.*:', '', summary(plant_data_median_normalized)[4,2:19])),
                                 Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_median_normalized)[5,2:19])),
                                 Max. = as.numeric(sub('.*:', '', summary(plant_data_median_normalized)[6,2:19])))

row.names(summary_table_NORM_median) <- NULL 
summary_table_NORM_median

#-------------------------------------------------------------------------------------------

summary_table_STD_zero <- data.frame(Attributes = c(colnames(plant_data_zero_std)[2:19]),
                                 Min. = as.numeric(sub('.*:', '', summary(plant_data_zero_std)[1,2:19])),
                                 First_Quart = as.numeric(sub('.*:', '', summary(plant_data_zero_std)[2,2:19])),
                                 Median = as.numeric(sub('.*:', '', summary(plant_data_zero_std)[3,2:19])), 
                                 Mean = as.numeric(sub('.*:', '', summary(plant_data_zero_std)[4,2:19])),
                                 Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_zero_std)[5,2:19])),
                                 Max. = as.numeric(sub('.*:', '', summary(plant_data_zero_std)[6,2:19])))

row.names(summary_table_STD_zero) <- NULL 
summary_table_STD_zero

#-------------------------------------------------------------------------------------------

summary_table_STD_mean <- data.frame(Attributes = c(colnames(plant_data_mean_std)[2:19]),
                                 Min. = as.numeric(sub('.*:', '', summary(plant_data_mean_std)[1,2:19])),
                                 First_Quart = as.numeric(sub('.*:', '', summary(plant_data_mean_std)[2,2:19])),
                                 Median = as.numeric(sub('.*:', '', summary(plant_data_mean_std)[3,2:19])), 
                                 Mean = as.numeric(sub('.*:', '', summary(plant_data_mean_std)[4,2:19])),
                                 Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_mean_std)[5,2:19])),
                                 Max. = as.numeric(sub('.*:', '', summary(plant_data_mean_std)[6,2:19])))

row.names(summary_table_STD_mean) <- NULL 
summary_table_STD_mean

#-------------------------------------------------------------------------------------------

summary_table_STD_median <- data.frame(Attributes = c(colnames(plant_data_median_std)[2:19]),
                                 Min. = as.numeric(sub('.*:', '', summary(plant_data_median_std)[1,2:19])),
                                 First_Quart = as.numeric(sub('.*:', '', summary(plant_data_median_std)[2,2:19])),
                                 Median = as.numeric(sub('.*:', '', summary(plant_data_median_std)[3,2:19])), 
                                 Mean = as.numeric(sub('.*:', '', summary(plant_data_median_std)[4,2:19])),
                                 Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_median_std)[5,2:19])),
                                 Max. = as.numeric(sub('.*:', '', summary(plant_data_median_std)[6,2:19])))

row.names(summary_table_STD_median) <- NULL 
summary_table_STD_median

#-------------------------------------------------------------------------------------------


comparison_LeafW_partE <- data.frame("Dataset" = c("Original", "Mean.Cen Zero Replaced", "Mean.Cen Mean Replaced", "Mean.Cen Median Replaced", "NORM Zero Replaced", "NORM Mean Replaced", "NORM Median Replaced", "STD Zero Replaced", "STD Mean Replaced", "STD Median Replaced"), 
                                     "Minimum" = c(summary_table[16,2], summary_table_MC_zero[16,2], summary_table_MC_mean[16,2], summary_table_MC_median[16,2], summary_table_NORM_zero[16,2], summary_table_NORM_mean[16,2], summary_table_NORM_median[16,2], summary_table_STD_zero[16,2], summary_table_STD_mean[16,2], summary_table_STD_median[16,2]),
                                     "First_Quantile" = c(summary_table[16,3], summary_table_MC_zero[16,3], summary_table_MC_mean[16,3], summary_table_MC_median[16,3], summary_table_NORM_zero[16,3], summary_table_NORM_mean[16,3], summary_table_NORM_median[16,3], summary_table_STD_zero[16,3], summary_table_STD_mean[16,3], summary_table_STD_median[16,3]),
                                     "Median" = c(summary_table[16,4], summary_table_MC_zero[16,4], summary_table_MC_mean[16,4], summary_table_MC_median[16,4], summary_table_NORM_zero[16,4], summary_table_NORM_mean[16,4], summary_table_NORM_median[16,4], summary_table_STD_zero[16,4], summary_table_STD_mean[16,4], summary_table_STD_median[16,4]),
                                     "Mean" = c(summary_table[16,5], summary_table_MC_zero[16,5], summary_table_MC_mean[16,5], summary_table_MC_median[16,5], summary_table_NORM_zero[16,5], summary_table_NORM_mean[16,5], summary_table_NORM_median[16,5], summary_table_STD_zero[16,5], summary_table_STD_mean[16,5], summary_table_STD_median[16,5]),
                                     "Third_Quantile" = c(summary_table[16,6], summary_table_MC_zero[16,6], summary_table_MC_mean[16,6], summary_table_MC_median[16,6], summary_table_NORM_zero[16,6], summary_table_NORM_mean[16,6], summary_table_NORM_median[16,6], summary_table_STD_zero[16,6], summary_table_STD_mean[16,6], summary_table_STD_median[16,6]),
                                     "Maximum" = c(summary_table[16,7], summary_table_MC_zero[16,7], summary_table_MC_mean[16,7], summary_table_MC_median[16,7], summary_table_NORM_zero[16,7], summary_table_NORM_mean[16,7], summary_table_NORM_median[16,7], summary_table_STD_zero[16,7], summary_table_STD_mean[16,7], summary_table_STD_median[16,7]))
comparison_LeafW_partE
write.csv(comparison_LeafW_partE, "Part-1-E-comparison_LeafW.csv")
