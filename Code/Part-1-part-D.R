#############################################
#############    PART: 1(D)    ##############
#############################################

library("dplyr")
library("zoo")

plant_data <- read.csv(file="g54dma-plant-dataset.csv", header=TRUE, sep=",")


# ------------------------------------------------------------------------------------
# -------------------- D(i) Replace with 0 -------------------------------------------
# ------------------------------------------------------------------------------------

replace_w_zero <- function (dataset){
  plant_data_w_0 <- data.frame(dataset)
  plant_data_w_0[is.na(plant_data_w_0)] <- 0
  return(plant_data_w_0)
}

plant_data_w_zero = replace_w_zero(plant_data)
#View(plant_data_w_zero)
#summary(plant_data_w_zero)
write.csv(plant_data_w_zero, "1-D-plant_data_zero.csv")


# ---------------------------------------------------------------------------------------
# -------------------- D(i) Replace with mean -------------------------------------------
# ---------------------------------------------------------------------------------------


replace_w_mean <- function (dataset){
  plant_data_w_Mean <- dataset %>% mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))
  return(plant_data_w_Mean)
}

plant_data_w_mean = replace_w_mean(plant_data)
plant_data_w_mean["Class"] = plant_data["Class"]
# summary(plant_data_w_mean)
# View(plant_data_w_mean)
write.csv(plant_data_w_mean, "1-D-plant_data_mean.csv")


# -----------------------------------------------------------------------------------------
# -------------------- D(i) Replace with median -------------------------------------------
# -----------------------------------------------------------------------------------------

replace_w_median <- function (dataset){
  plant_data_w_Median <- dataset %>% mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))
  return(plant_data_w_Median)
}


plant_data_w_median = replace_w_median(plant_data)
plant_data_w_median["Class"] = plant_data["Class"]
# summary(plant_data_w_median)
# View(plant_data_w_median)
write.csv(plant_data_w_median, "1-D-plant_data_median.csv")

#---------------------------------------------------------------------------------------------------------
#------------------------------------PART - D (ii)  - COMPARISON -----------------------------------------
#---------------------------------------------------------------------------------------------------------

summary_table_zero <- data.frame(Attributes = c(colnames(plant_data_w_zero)[2:19]),
                            Min. = as.numeric(sub('.*:', '', summary(plant_data_w_zero)[1,2:19])),
                            First_Quart = as.numeric(sub('.*:', '', summary(plant_data_w_zero)[2,2:19])),
                            Median = as.numeric(sub('.*:', '', summary(plant_data_w_zero)[3,2:19])), 
                            Mean = as.numeric(sub('.*:', '', summary(plant_data_w_zero)[4,2:19])),
                            Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_w_zero)[5,2:19])),
                            Max. = as.numeric(sub('.*:', '', summary(plant_data_w_zero)[6,2:19])))

row.names(summary_table_zero) <- NULL 
summary_table_zero

#------------------------------------------------------------------------------------------------------------
summary_table_mean <- data.frame(Attributes = c(colnames(plant_data_w_mean)[2:19]),
                                 Min. = as.numeric(sub('.*:', '', summary(plant_data_w_mean)[1,2:19])),
                                 First_Quart = as.numeric(sub('.*:', '', summary(plant_data_w_mean)[2,2:19])),
                                 Median = as.numeric(sub('.*:', '', summary(plant_data_w_mean)[3,2:19])), 
                                 Mean = as.numeric(sub('.*:', '', summary(plant_data_w_mean)[4,2:19])),
                                 Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_w_mean)[5,2:19])),
                                 Max. = as.numeric(sub('.*:', '', summary(plant_data_w_mean)[6,2:19])))

row.names(summary_table_mean) <- NULL 
summary_table_mean

#------------------------------------------------------------------------------------------------------------
summary_table_median <- data.frame(Attributes = c(colnames(plant_data_w_median)[2:19]),
                                 Min. = as.numeric(sub('.*:', '', summary(plant_data_w_median)[1,2:19])),
                                 First_Quart = as.numeric(sub('.*:', '', summary(plant_data_w_median)[2,2:19])),
                                 Median = as.numeric(sub('.*:', '', summary(plant_data_w_median)[3,2:19])), 
                                 Mean = as.numeric(sub('.*:', '', summary(plant_data_w_median)[4,2:19])),
                                 Third_Quart = as.numeric(sub('.*:', '', summary(plant_data_w_median)[5,2:19])),
                                 Max. = as.numeric(sub('.*:', '', summary(plant_data_w_median)[6,2:19])))

row.names(summary_table_median) <- NULL 
summary_table_median

#-------------------------------------------------------------------------------------------------------------

comparison_centX_partD <- data.frame("Dataset" = c("Original", "Zero Replaced", "Mean Replaced", "Median Replaced"), 
                                  "Minimum" = c(summary_table[1,2], summary_table_zero[1,2], summary_table_mean[1,2], summary_table_median[1,2]),
                                  "First_Quantile" = c(summary_table[1,3], summary_table_zero[1,3], summary_table_mean[1,3], summary_table_median[1,3]),
                                  "Median" = c(summary_table[1,4], summary_table_zero[1,4], summary_table_mean[1,4], summary_table_median[1,4]),
                                  "Mean" = c(summary_table[1,5], summary_table_zero[1,5], summary_table_mean[1,5], summary_table_median[1,5]),
                                  "Third_Quantile" = c(summary_table[1,6], summary_table_zero[1,6], summary_table_mean[1,6], summary_table_median[1,6]),
                                  "Maximum" = c(summary_table[1,7], summary_table_zero[1,7], summary_table_mean[1,7], summary_table_median[1,7]))
comparison_centX_partD
write.csv(comparison_centX_partD, "D-ii-comparison_centX.csv")

ggplot(plant_data, aes(x=Class, y=CentroidX)) + geom_boxplot(alpha = 0.5) + stat_summary(fun.y = mean, geom='point', size = 2)+ 
  geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0, fill = NA) + ggtitle("Boxplot of Class vs. CentroidX - Original Dataset")

ggplot(plant_data_w_zero, aes(x=Class, y=CentroidX)) + geom_boxplot(alpha = 0.5) + stat_summary(fun.y = mean, geom='point', size = 2)+ 
  geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0, fill = NA) + ggtitle("Boxplot of Class vs. CentroidX - Zero Replaced Dataset")

ggplot(plant_data_w_mean, aes(x=Class, y=CentroidX)) + geom_boxplot(alpha = 0.5) + stat_summary(fun.y = mean, geom='point', size = 2)+ 
  geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0, fill = NA) + ggtitle("Boxplot of Class vs. CentroidX - Mean Replaced Dataset")

ggplot(plant_data_w_median, aes(x=Class, y=CentroidX)) + geom_boxplot(alpha = 0.5) + stat_summary(fun.y = mean, geom='point', size = 2)+ 
  geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0, fill = NA) + ggtitle("Boxplot of Class vs. CentroidX - Median Replaced Dataset")

#---------------------------------------------------------------------------------------------------------------------------------------------------


comparison_LeafW_partD <- data.frame("Dataset" = c("Original", "Zero Replaced", "Mean Replaced", "Median Replaced"), 
                                     "Minimum" = c(summary_table[16,2], summary_table_zero[16,2], summary_table_mean[16,2], summary_table_median[16,2]),
                                     "First_Quantile" = c(summary_table[16,3], summary_table_zero[16,3], summary_table_mean[16,3], summary_table_median[16,3]),
                                     "Median" = c(summary_table[16,4], summary_table_zero[16,4], summary_table_mean[16,4], summary_table_median[16,4]),
                                     "Mean" = c(summary_table[16,5], summary_table_zero[16,5], summary_table_mean[16,5], summary_table_median[16,5]),
                                     "Third_Quantile" = c(summary_table[16,6], summary_table_zero[16,6], summary_table_mean[16,6], summary_table_median[16,6]),
                                     "Maximum" = c(summary_table[16,7], summary_table_zero[16,7], summary_table_mean[16,7], summary_table_median[16,7]))
comparison_LeafW_partD
write.csv(comparison_LeafW_partD, "D-ii-comparison_LeafW.csv")

ggplot(plant_data, aes(x=Class, y=Leaf.weight)) + geom_boxplot(alpha = 0.5) + stat_summary(fun.y = mean, geom='point', size = 2)+ 
  geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0, fill = NA) + ggtitle("Boxplot of Class vs. Leaf Weight - Original Dataset")

ggplot(plant_data_w_zero, aes(x=Class, y=Leaf.weight)) + geom_boxplot(alpha = 0.5) + stat_summary(fun.y = mean, geom='point', size = 2)+ 
  geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0, fill = NA) + ggtitle("Boxplot of Class vs. Leaf Weight - Zero Replaced Dataset")

ggplot(plant_data_w_mean, aes(x=Class, y=Leaf.weight)) + geom_boxplot(alpha = 0.5) + stat_summary(fun.y = mean, geom='point', size = 2)+ 
  geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0, fill = NA) + ggtitle("Boxplot of Class vs. Leaf Weight - Mean Replaced Dataset")

ggplot(plant_data_w_median, aes(x=Class, y=Leaf.weight)) + geom_boxplot(alpha = 0.5) + stat_summary(fun.y = mean, geom='point', size = 2)+ 
  geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0, fill = NA) + ggtitle("Boxplot of Class vs. Leaf Weight - Median Replaced Dataset")
