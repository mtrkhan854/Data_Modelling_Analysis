

# This script contains parts f(i) and f(ii)

# Unused:
# library("lattice")
# library("survival")
# library("Formula")
# library("matrixcalc")

plant_data <- read.csv(file="g54dma-plant-dataset.csv", header=TRUE, sep=",")
#---------------------------------------     Missing Attributes   -----------------------------------------------------------
attr_missing_val <- function(dataset){
    
    threshold = 0.5
    attr_del_list <- c()
    
    for (j in 2:(ncol(dataset)-1)){
      
      
      if ((sum(is.na(dataset[j])) / (nrow(dataset))) > threshold){ 
          attr_del_list <- c(attr_del_list,colnames(dataset[j]))
      }
    }
    
    
    if (length(attr_del_list) > 0){
      return(attr_del_list)
    }
    
    else{
      attr_to_del <- 0
      #print("No attribute fills the threshold criteria. Therefore nothing to delete")
      return(attr_to_del)
    }
}

#---------------------------------------     Missing Instances   -----------------------------------------------------------

instance_missing_val <- function(dataset){
  
  threshold = 0.5
  instance_del_list <- c()
  
  for (i in 1:(nrow(dataset))){
    
    
    if ((sum(is.na(dataset[i,])) / (ncol(dataset))) > threshold){ 
      instance_del_list <- c(instance_del_list, as.numeric(rownames(dataset[i,])))
    }
  }
  
  
  if (length(instance_del_list) > 0)
    return(instance_del_list)
  
  else{
    instance_to_del <- 0
    #print("No instance fills the threshold criteria. Therefore nothing to delete")
    return(instance_to_del)
  }
}

#---------------------------------------------------------------------------------------------------------------------

# attr_missing_val(plant_data)
# instance_missing_val(plant_data)
# 
# #plant_data_cleaned_miss_vals <- subset(plant_data, select =-c(unquote(attr_missing_val(plant_data))))
# drop_attrs <- c(attr_missing_val(plant_data))
# drop_instances <- instance_missing_val(plant_data)
# drop_instances
# 
# plant_data_cleaned_miss_vals = plant_data[ -c(3), !(names(plant_data) %in% drop_attrs)]
# View(plant_data_cleaned_miss_vals)


#-------------------------     Cleaning Missing Attributes and  Instances with >50% missing data  ---------------------

cleaned_missing_vals <- function(dataset){
  drop_attrs <- c(attr_missing_val(dataset))
  drop_instances <- instance_missing_val(dataset)
  
  if (drop_attrs!= 0 & drop_instances != 0){
    plant_data_cleaned_miss_vals = dataset[ -c(drop_instances), !(names(dataset) %in% drop_attrs)]
  }
  
  else if (drop_attrs != 0 & drop_instances == 0){
    plant_data_cleaned_miss_vals = dataset[ , !(names(dataset) %in% drop_attrs)]
  }
  
  else if (drop_attrs == 0 & drop_instances != 0){
    plant_data_cleaned_miss_vals = dataset[-c(drop_instances) , ]
  } 
  else{
    plant_data_cleaned_miss_vals = dataset[,]
  }
  
  return(plant_data_cleaned_miss_vals)
}

plant_data_cleaned_no_missing = cleaned_missing_vals(plant_data) #does contain NA in instances/rows
View(plant_data_cleaned_no_missing)
write.csv(plant_data_cleaned_no_missing, "Part-1-F-i-plant_data_wo_LW.csv")

#-------------------- Correlation between Attributes w/ Outliers------------------------------------------------------------------


#correlation_finder <- rcorr(as.matrix(plant_data[,2:19]), type = "pearson")
correlation_finder_alt <- cor(as.matrix(plant_data[,2:19]), method = c("pearson"), use ="pairwise.complete.obs")

#correlation_table <- signif(correlation_finder$r, 6)
#correlation_table
correlation_df <- as.data.frame(correlation_finder_alt)
write.csv(correlation_df, "Part-1-F-ii-Correlation_Table.csv")

# pval_table <- signif(correlation_finder$P, 6)
# pval_df <- as.data.frame(pval_table)
# write.csv(pval_df, "F-ii-pval_table.csv")
library("corrplot") #for nice display purposes only. Base R package

#------------------------- correlation plot without any attribute deletion -------------------------------------------------------

corrplot(correlation_table, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(correlation_finder_alt, method = "color") # used in coverpage

# ---------------------------- Drop correlations outside of 0 to 0.7* range -------------------------------------------------------

drop_corr <- function(dframe){
  
  upper_lim = 0.7 #at the boundary of strong correlation, since the dataset is highly correlated. anything above this is dropped
  
  for (j in 1:ncol(dframe)){
    
    for (i in 1:nrow(dframe)){
      
      if (is.na(dframe[i,j]) | abs(dframe[i,j]) > upper_lim){
        dframe[i,j] = 0
      }
      
    }
    
  }
  
  return(dframe)
} 

uncor_vals = drop_corr(correlation_df)
#uncor_vals
uncor_vals_matrix = as.matrix(uncor_vals)
#uncor_vals_matrix
#upper.tri(uncor_vals_matrix, diag = FALSE)
uncor_vals_matrix[lower.tri(uncor_vals_matrix, diag = TRUE)] <- NA
#uncor_vals_matrix


#----------------------------------------     correlation plot without any attribute deletion    ---------------------------------------------------------------------------------
uncor_vals_disp = drop_corr(correlation_df)
uncor_vals_matrix_disp = as.matrix(uncor_vals_disp)
# uncor_vals_matrix_disp[lower.tri(uncor_vals_matrix_disp, diag = TRUE)] <- 0
#uncor_vals_matrix_disp_mag = uncor_vals_matrix_disp * 3 #used previously for increasing the amplitude - to see the circles
#uncor_vals_matrix_disp_mag
corrplot(uncor_vals_matrix_disp, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


#---------------------------------------------------------------------------------------------------------------------------------
list_cor_attr <- function(mtx){
  
  cor_attr_holder <- matrix(nrow = nrow(mtx), ncol = ncol(mtx))
  
  
  for (i in 1:nrow(mtx)){
    
    for (j in 1:ncol(mtx)){
      
      if (is.na(mtx[i,j])) {
        next
      }
      
      else if (mtx[i,j] == 0){
        cor_attr_holder[i,j] = colnames(mtx)[j]  
        
      } 
      
    }
    
  }
  
  return(cor_attr_holder)
}

# test = as.data.frame(list_cor_attr(uncor_vals_matrix))
# test

all_cor_attr = as.data.frame(list_cor_attr(uncor_vals_matrix))


row_names = c(rownames(correlation_df))
rownames(all_cor_attr) = row_names
col_names = c(colnames(correlation_df))
colnames(all_cor_attr) = col_names

all_cor_attr
write.csv(all_cor_attr, "Part-1-f-ii-all_correlated_attr.csv", na= "")

# ----------------------- Attempt at using P-val --------------------------------------------------------------------------------

# finding_uncor <- function(dataset){
#   alpha = 0.05
#   
#   uncor_tracker <- matrix(nrow = nrow(dataset), ncol = ncol(dataset))
#   
#   for (j in 1:ncol(dataset)){
#     
#       for (i in 1:nrow(dataset)){
#       
#         if (is.na(dataset[i,j]) | dataset[i,j] < alpha){
#           uncor_tracker[i,j] = 0
#         }
#       
#         else{
#           uncor_tracker[i,j] = 1
#         }
#       
#       }
#     
#   }
#   return(uncor_tracker)
# }
# 
# #--------------------------------------------------------------------------------------------------------
# uncor_tracking = finding_uncor(pval_df)  
# pval_matrix = as.matrix(pval_df) 
# masking_uncor_finder = uncor_tracking * pval_matrix


#-----------------------------------------#################################---------------------------------------------------------------
# 
# finding_uncor_attr <- function(MatrX){
#     
#     
#   
#   
#   
# }

# correlation_table <- cor(plant_data[2:19], method = "pearson", use = "complete.obs")
# correlation_df <- as.data.frame(correlation_table)
# #correlation_df
# #is.data.frame(correlation_df)
# write.csv(correlation_df, "F-ii-Correlation_Table.csv")
# 
# flattenCorrMatrix <- function(cormat, pmat) {
#   ut <- upper.tri(cormat)
#   data.frame(
#     row = rownames(cormat)[row(cormat)[ut]],
#     column = rownames(cormat)[col(cormat)[ut]],
#     cor  =(cormat)[ut],
#     p = pmat[ut]
#   )
# }
# 
# flattenCorrMatrix(res2$r, res2$P)

# cor.test(plant_data$CentroidX,plant_data$Orientation2, method = "pearson")
# cor(plant_data$Orientation7, plant_data$Orientation1, method = "pearson", use = "complete.obs")





#-------------------- Cleaning all Outliers (Attempted)  ------------------------------------------------------------------


# 
# finding_bounds <- function (dataset){
#   
#   plant_data_replicate <- data.frame(dataset)
#   
#   lower_bound <- c()
#   upper_bound <- c()
#   
#   for (j in 2:(ncol(dataset)-1)){
#     
#     lower_bound = c(lower_bound, quantile(plant_data_replicate[j], .25, na.rm= TRUE) - 3.0*IQR(plant_data_replicate[j], na.rm= TRUE))
#     upper_bound = c(upper_bound, quantile(plant_data_replicate[j], .75, na.rm= TRUE) + 3.0*IQR(plant_data_replicate$LeafArea, na.rm= TRUE))
#   }
#   
#   for (i in 2:(ncol(dataset)-1)){
#     
#     
#   }  
#   
# }
# 
# 
# Lower_bound = quantile(plant_data_replicate$LeafArea, .25, na.rm= TRUE) - 3.0*IQR(plant_data_replicate$LeafArea, na.rm= TRUE)
# Upper_bound = quantile(plant_data_replicate$LeafArea, .75, na.rm= TRUE) + 3.0*IQR(plant_data_replicate$LeafArea, na.rm= TRUE)
# 
# plant_data_no_outlier <- plant_data_replicate [plant_data_replicate$LeafArea > Lower_bound & 
#                                                plant_data_replicate$LeafArea < Upper_bound,]
# View(plant_data_no_outlier)
# 
# ggplot(plant_data_no_outlier, aes(x=Class, y=LeafArea)) + geom_point(aes(color=Class))+
#   geom_boxplot(outlier.colour="black", outlier.shape=2,outlier.size=5, fill = NA) + stat_summary(fun.y = mean, geom='point', size = 2)+ 
#   geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0.2, vjust = -0.5, fill = NA) +theme_grey() +
#   ggtitle("Scatter Plot of Class vs. Leaf Area")
# 
# summary(plant_data_no_outlier)
# 
# no_outliers = plant_data_replicate[!plant_data_replicate[2:19] %in% boxplot.stats(plant_data_replicate[2:19])$out]
# summary(no_outliers)

#---------------------------------------------------------------------------------------------------------------------------------
#------------------------ PART OF PART 1 - F - ii --------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------

plant_data_uncorrelated_only = data.frame(plant_data)

# Done by checking the high correlation table that was formed earlier. Output from R to excel.
# Criteria 1: checking number of missing values
# Criteria 2: if number of missing values were same, then check with however many other attributes was the attribute correlated with.
# Decision: Choose the one that is lower in both criteria

drops_correlated <- c("Depth","Orientation0", "Orientation5", "Orientation1", "Orientation2", "Orientation4", "Orientation7", "Orientation8", "Orientation6")
plant_data_uncorrelated_only = data.frame(plant_data_uncorrelated_only[ , !(names(plant_data_uncorrelated_only) %in% drops_correlated)])

pd_uncor_no_LW = cleaned_missing_vals(plant_data_uncorrelated_only)
#View(pd_uncor_no_LW)

plant_data_uncor_no_NA = replace_w_mean(pd_uncor_no_LW)
plant_data_uncor_no_NA["Class"] = plant_data["Class"]
#View(plant_data_uncor_no_NA)
write.csv(plant_data_uncor_no_NA, "Part-1-f-ii-uncorrelated_no_NA.csv")
