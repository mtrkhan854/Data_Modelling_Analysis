library("ggplot2")

#------------------------  Finding instances with Missing Attributes ----------------------------------------------------
#------------------------  for F - i ------------------------------------------------------------------------------------ 
# This function is used for illustration only

num_instance_na <- function(dataset){
  
  instance_w_na <- c()
  
  for (i in 1:nrow(dataset)){
    
    for (j in 1:ncol(dataset)){
      
      if (is.na(dataset[i,j])) {

        instance_w_na <- c(instance_w_na, as.numeric(rownames(dataset[i,])))
        
      }
      
      else {
        next 
        
      } 
      
    }
    
  }
  
  temp_instances_missing_vals = as.data.frame(table(instance_w_na))
  colnames(temp_instances_missing_vals) <- c("Row_Numbers", "Missing_Attributes")
  
  instances_missing_vals = as.data.frame(table(temp_instances_missing_vals$Missing_Attributes))
  colnames(instances_missing_vals) <- c("Missing_Attributes", "Instances")
  
  return(instances_missing_vals)
  
  
}



num_instance_na_DF = num_instance_na(plant_data_cleaned_no_missing)
num_instance_na_DF

ggplot(data=num_instance_na_DF, aes(x=Missing_Attributes, y=Instances)) + geom_bar(stat="identity", width=0.5) + 
    labs(title="Number of Instances with Missing Attributes", x="Number of Missing Attributes", y = "Number of Instances")+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=18,face="bold")) +
    scale_y_continuous(limits = c(0,100), breaks = c(seq(0,100, by = 25), 93))

#---------------------------------------------------------------------------------------------------------------------------
    

#------------------------  Dataframe w/o (i) Leaf.weight (LW) ; (ii) mean replaced  ; (iii) Standardized ; w/o Sample ID and Class -------------------------------------------------------

# (i):
df_pca_no_LW  = cleaned_missing_vals(plant_data)

# (ii):
df_pca_mean_replaced = replace_w_mean(df_pca_no_LW)

# (iii):
df_pca_w_SID_Class = standardization(df_pca_mean_replaced)

# (iv):
df_for_pca <- within(df_pca_w_SID_Class, rm(Sample_ID, Class))
#View(df_pca)


# --------------------------------------------------------------------------------------------------------------
# ------------------------  Principal Component Analysis -------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------

pca_f_iii = prcomp(df_for_pca, scale = TRUE)
# names(pca_f_iii)
# pca_f_iii$center

#pve = percentage of variance explaind
pca_f_iii.var = (pca_f_iii$sdev)^2
pve = pca_f_iii.var/sum(pca_f_iii.var ) #pve -> percentage of variance explained
pve

plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1) ,type='b') + title("Variance Explained by each Principle Component")+ axis(side = 1, seq(1:17))
plot(cumsum (pve ), xlab=" Principal Component ", ylab =" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type='b') + title("Principle Component wise Cumulative Variance") + axis(side = 1, seq(1:17))

cum_comp = cumsum(pve)
cum_comp

cum_7_comp = cum_comp[1:7]  #previously cum_7_comp was cum_ten_comp
cum_7_comp

cum_7_comp_df = as.data.frame(cum_7_comp)
row.names(cum_7_comp_df) <- c("PC1", "PC2","PC3","PC4","PC5", "PC6","PC7")
colnames(cum_7_comp_df) <- c("CVE")
cum_7_comp_df

# ----------------------------------------------------------------------------------------------------------------

# Getting all the loading vectors:
seven_comps_PCA <- pca_f_iii$rotation[,c("PC1", "PC2","PC3","PC4","PC5", "PC6","PC7")]
# seven_comps_PCA

matrix_df_for_pca <- as.matrix(df_for_pca)
# matrix_df_for_pca

matrix_pca_seven_comps <- matrix_df_for_pca %*% seven_comps_PCA
pca_seven_comps_df = as.data.frame(matrix_pca_seven_comps)
pca_seven_comps_df <- cbind(pca_seven_comps_df, Class = plant_data$Class)
write.csv(pca_seven_comps_df, "f-iii-PCA_7_Components.csv")

#-------------------------------------------------------------------------------------------------------------------

bip(pca_f_iii, choices = 1:2, scale = 1, pc.biplot = FALSE, col=df_pca_w_SID_Class$Class, pch=16) 

