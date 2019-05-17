#############################################
#############    PART: 1(B)    ##############
#############################################

library("ggplot2")
library("ggpubr")
#View(plant_data)
plant_data <- read.csv(file="g54dma-plant-dataset.csv", header=TRUE, sep=",")
# ----------------------------------------------------------------------------------
# -------------------- B(i) Correlations -------------------------------------------
# ----------------------------------------------------------------------------------


cor_or1_or7 = cor(plant_data$Orientation1, plant_data$Orientation7, 
                  method = "pearson", use = "complete.obs") #complete.obs  for handling missing data by case wise deletion

cor_mass_or0 = cor(plant_data$Mass, plant_data$Orientation0, 
                   method = "pearson", use = "complete.obs")

cor_or7_or8 = cor(plant_data$Orientation7, plant_data$Orientation8, 
                   method = "pearson", use = "complete.obs")

reg1<-lm(Orientation7 ~ Orientation1, data = plant_data)

ggplot(data = plant_data, aes(Orientation1, Orientation7, color = Class)) +
  geom_point() + labs(title="Scatterplot of Orien1 vs Orien7", x="Orientation1", y="Orientation7")+
  geom_smooth(color = "black", method = "lm") + scale_colour_manual(values=c("green", "blue", "red","orange", "violet"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data = plant_data, aes(Mass, Orientation0, color = Class)) +
  geom_point() + labs(title="Scatterplot of Mass vs Orientation0", x="Mass", y="Orientation0")+
  geom_smooth(color = "black", method = "lm") + scale_colour_manual(values=c("green", "blue", "red","orange", "violet"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data = plant_data, aes(Orientation7, Orientation8, color = Class)) +
  geom_point() + labs(title="Scatterplot of Orientation7 vs Orientation8", x="Orientation7", y="Orientation8")+
  geom_smooth(color = "black", method = "lm") + scale_colour_manual(values=c("green", "blue", "red","orange", "violet"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#----------------------------------------------------------------------------------
#------------------------ T H E   M O S T  / L E A S T   C O R R  -----------------
#----------------------------------------------------------------------------------

temp_cor_tab = correlation_table
#temp_cor_tab
diag(temp_cor_tab) <- NA

#********************************************************************************************
# ---- finding max correlated value -------------------------
max_cor_value = (mmax <- max(abs(temp_cor_tab), na.rm=TRUE))

which(abs(temp_cor_tab) == mmax, arr.ind=TRUE)

sprintf("Maximum correlated value: %f", max_cor_value)

# ---- finding min correlated value -------------------------
min_cor_value = (mmin <- min(abs(temp_cor_tab), na.rm=TRUE))

which(abs(temp_cor_tab) == mmin, arr.ind=TRUE)
sprintf("Minimum correlated value: %f", min_cor_value)
#********************************************************************************************

#----------------------------------------------------------------------------------

ggscatter(plant_data, x = "Orientation1", y = "Orientation7", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          main = "Correlation betweeen Orientation 1 and Orientation 7", 
          xlab = "Orientation 1", ylab = "Orientation 7") 

ggscatter(plant_data, x = "Mass", y = "Orientation0", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          main = "Correlation betweeen Mass and Orientation 0",
          xlab = "Mass", ylab = "Orientation 0") 

ggscatter(plant_data, x = "Orientation7", y = "Orientation8", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          main = "Correlation betweeen Orientation 7 and Orientation 8",
          xlab = "Orientation 7", ylab = "Orientation 8") 


# -----------------------------------------------------------------------------------
# -------------------- B(iv) Scatterplots -------------------------------------------
# -----------------------------------------------------------------------------------


ggplot(plant_data, aes(x=Class, y=Orientation2)) + geom_point(aes(color=Class))+
  stat_summary(fun.y = mean, geom='point', size = 2)+ 
  geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0, fill = NA) + theme_grey() + 
  ggtitle("Scatter Plot of Class vs. Orientation 2")

ggplot(plant_data, aes(x=Class, y=Depth)) + geom_point(aes(color=Class))+
  stat_summary(fun.y = mean, geom='point', size = 2)+ 
  geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0, vjust = -0.1,  fill = NA) + theme_grey() +
  ggtitle("Scatter Plot of Class vs. Depth")

ggplot(plant_data, aes(x=Class, y=LeafArea)) + geom_point(aes(color=Class))+
  stat_summary(fun.y = mean, geom='point', size = 2)+ 
  geom_label(stat = 'summary', fun.y=mean, aes(label =round(..y.., 4)),label.size=0, nudge_x = 0.1, hjust = 0.2, vjust = -0.5, fill = NA) +theme_grey() +
  ggtitle("Scatter Plot of Class vs. Leaf Area")
