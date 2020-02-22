#############################################
#############    PART: 1(A)    ##############
#############################################

library("ggplot2")
#library("cowplot")
#library("sjlabelled")

plant_data <- read.csv(file="g54dma-plant-dataset.csv", header=TRUE, sep=",")
#View(plant_data)


# ----------------------------------------------------------------------------------
# -------------------- A(i) Centrality and Dispersion ------------------------------
# ----------------------------------------------------------------------------------



# centX_summary = summary (plant_data$CentroidX) # or, p =  summary(plant_data$CentroidX); p["Min."] <- IE do individually
# centX_summary
# centY_summary = summary (plant_data$CentroidY)
# 
# mass_summary = summary (plant_data$Mass)
# width_summary = summary (plant_data$Width)
# depth_summary = summary (plant_data$Depth)
# 
# 
# orien0_summary = summary (plant_data$Orientation0)
# orien1_summary = summary (plant_data$Orientation1)
# orien2_summary = summary (plant_data$Orientation2)
# orien3_summary = summary (plant_data$Orientation3)
# 
# orien4_summary = summary (plant_data$Orientation4)
# orien5_summary = summary (plant_data$Orientation5)
# orien6_summary = summary (plant_data$Orientation6)
# orien7_summary = summary (plant_data$Orientation7)
# 
# orien8_summary = summary (plant_data$Orientation8)
# orien9_summary = summary (plant_data$Orientation9)
# 
# leaf_weight_summary = summary (plant_data$Leaf.weight)
# leaf_area_summary = summary (plant_data$LeafArea)
# leaf_hue_summary = summary (plant_data$Leaf.Hue)
# 
# class_summary = summary (plant_data$Class)
# 
# 
# 
# # ----------------------------------------------------------------------------------
# # -------------------- A(i) Missing values in Each Attribute -----------------------
# # ----------------------------------------------------------------------------------
# 
# centX_missing = sum(is.na(plant_data$CentroidX))
# centY_missing = sum(is.na(plant_data$CentroidY))
# 
# mass_missing = sum(is.na(plant_data$Mass))
# width_missing = sum(is.na(plant_data$Width))
# depth_missing = sum(is.na(plant_data$Depth))
# 
# orien0_missing = sum(is.na(plant_data$Orientation0))
# orien1_missing = sum(is.na(plant_data$Orientation1))
# orien2_missing = sum(is.na(plant_data$Orientation2))
# orien3_missing = sum(is.na(plant_data$Orientation3))
# 
# orien4_missing = sum(is.na(plant_data$Orientation4))
# orien5_missing = sum(is.na(plant_data$Orientation5))
# orien6_missing = sum(is.na(plant_data$Orientation6))
# orien7_missing = sum(is.na(plant_data$Orientation7))
# 
# orien8_missing = sum(is.na(plant_data$Orientation8))
# orien9_missing = sum(is.na(plant_data$Orientation9))
# 
# leaf_weight_missing = sum(is.na(plant_data$Leaf.weight))
# leaf_area_missing = sum(is.na(plant_data$LeafArea))
# leaf_hue_missing = sum(is.na(plant_data$Leaf.Hue))
# 
# class_missing = sum(is.na(plant_data$Class))

# ------------------------------- COMBINED TABLE FOR A(i) ---------------------------------

summary_table <- data.frame(Attributes = c(colnames(plant_data)[2:19]),
                            Min. = as.numeric(sub('.*:', '', summary(plant_data)[1,2:19])),
                            First_Quart = as.numeric(sub('.*:', '', summary(plant_data)[2,2:19])),
                            Median = as.numeric(sub('.*:', '', summary(plant_data)[3,2:19])), 
                            Mean = as.numeric(sub('.*:', '', summary(plant_data)[4,2:19])),
                            Third_Quart = as.numeric(sub('.*:', '', summary(plant_data)[5,2:19])),
                            Max. = as.numeric(sub('.*:', '', summary(plant_data)[6,2:19])),
                            Miss_Vals = as.numeric(sub('.*:', '', summary(plant_data)[7,2:19])))
row.names(summary_table) <- NULL 
summary_table
write.csv(summary_table, "A-i-summary_table.csv")


# -----------------------------------------------------------------------------------
# --------------------------  A(ii) Histograms  -------------------------------------
# -----------------------------------------------------------------------------------


  
################# Histogram for Centriodx before cleaning ###############################################



####################################################################################################################

#calculating Freedman-Diaconis Bin Width

CentX_bins = round((summary_table[1,7] - summary_table[1,2])/(2*(summary_table[1,6] - summary_table[1,3])*(nrow(plant_data))^(-1/3)))


centX_hist <- ggplot(data=plant_data, aes(x=plant_data$CentroidX)) + 
              geom_histogram(bins = CentX_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
              # scale_fill_gradient("Count", low="green", high="red")+
              labs(title="Histogram for Centroid X", x="CentroidX", y="Frequency") + 
              geom_vline(aes(xintercept=mean(plant_data$CentroidX), color="Mean"), size=1)+
              geom_vline(aes(xintercept=median(plant_data$CentroidX), color="Median"),linetype='longdash', size=1)+
              geom_vline(aes(xintercept=quantile(plant_data$CentroidX)[2],color="First_Quantile"), size=1)+
              geom_vline(aes(xintercept=quantile(plant_data$CentroidX)[4],color="Third_Quantile"), size=1) +
              # theme_grey()+ add this for only one of the histograms for labels on the side
              theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
              
              scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))

  
              # add stat labels at the top only
centX_hist

####################################################################################################################

CentY_bins = round((summary_table[2,7] - summary_table[2,2])/(2*(summary_table[2,6] - summary_table[2,3])*(nrow(plant_data))^(-1/3)))


centY_hist <- ggplot(data=plant_data, aes(x=plant_data$CentroidY)) + 
  geom_histogram(bins = CentY_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Centroid Y", x="Centroid Y", y="Frequency") + 
  geom_vline(aes(xintercept=mean(plant_data$CentroidY), color="Mean"), size=1)+
  geom_vline(aes(xintercept=median(plant_data$CentroidY), color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=quantile(plant_data$CentroidY)[2],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=quantile(plant_data$CentroidY)[4],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



centY_hist

####################################################################################################################

mass_bins = round((summary_table[3,7] - summary_table[3,2])/(2*(summary_table[3,6] - summary_table[3,3])*(nrow(plant_data))^(-1/3)))


mass_hist <- ggplot(data=plant_data, aes(x=plant_data$Mass)) + 
  geom_histogram(bins = mass_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Mass", x="Mass", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[3,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[3,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[3,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[3,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



mass_hist

####################################################################################################################

width_bins = round((summary_table[4,7] - summary_table[4,2])/(2*(summary_table[4,6] - summary_table[4,3])*(nrow(plant_data))^(-1/3)))


width_hist <- ggplot(data=plant_data, aes(x=plant_data$Width)) + 
  geom_histogram(bins = width_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Width", x="Width", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[4,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[4,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[4,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[4,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



width_hist


####################################################################################################################

depth_bins = round((summary_table[5,7] - summary_table[5,2])/(2*(summary_table[5,6] - summary_table[5,3])*(nrow(plant_data))^(-1/3)))


depth_hist <- ggplot(data=plant_data, aes(x=plant_data$Depth)) + 
  geom_histogram(bins = depth_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Depth", x="Width", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[5,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[5,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[5,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[5,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



depth_hist


####################################################################################################################

orientation0_bins = round((summary_table[6,7] - summary_table[6,2])/(2*(summary_table[6,6] - summary_table[6,3])*(nrow(plant_data))^(-1/3)))


orientation0_hist <- ggplot(data=plant_data, aes(x=plant_data$Orientation0)) + 
  geom_histogram(bins = orientation0_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Orientation0", x="Orientation0", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[6,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[6,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[6,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[6,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



orientation0_hist


####################################################################################################################

orientation1_bins = round((summary_table[7,7] - summary_table[7,2])/(2*(summary_table[7,6] - summary_table[7,3])*(nrow(plant_data))^(-1/3)))


orientation1_hist <- ggplot(data=plant_data, aes(x=plant_data$Orientation1)) + 
  geom_histogram(bins = orientation1_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Orientation1", x="Orientation1", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[7,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[7,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[7,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[7,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



orientation1_hist


####################################################################################################################

orientation2_bins = round((summary_table[8,7] - summary_table[8,2])/(2*(summary_table[8,6] - summary_table[8,3])*(nrow(plant_data))^(-1/3)))


orientation2_hist <- ggplot(data=plant_data, aes(x=plant_data$Orientation2)) + 
  geom_histogram(bins = orientation2_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Orientation2", x="Orientation2", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[8,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[8,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[8,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[8,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



orientation2_hist


####################################################################################################################

orientation3_bins = round((summary_table[9,7] - summary_table[9,2])/(2*(summary_table[9,6] - summary_table[9,3])*(nrow(plant_data))^(-1/3)))


orientation3_hist <- ggplot(data=plant_data, aes(x=plant_data$Orientation3)) + 
  geom_histogram(bins = orientation3_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Orientation3", x="Orientation3", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[9,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[9,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[9,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[9,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



orientation3_hist


####################################################################################################################

orientation4_bins = round((summary_table[10,7] - summary_table[10,2])/(2*(summary_table[10,6] - summary_table[10,3])*(nrow(plant_data))^(-1/3)))


orientation4_hist <- ggplot(data=plant_data, aes(x=plant_data$Orientation4)) + 
  geom_histogram(bins = orientation4_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Orientation4", x="Orientation4", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[10,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[10,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[10,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[10,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



orientation4_hist


####################################################################################################################

orientation5_bins = round((summary_table[11,7] - summary_table[11,2])/(2*(summary_table[11,6] - summary_table[11,3])*(nrow(plant_data))^(-1/3)))


orientation5_hist <- ggplot(data=plant_data, aes(x=plant_data$Orientation5)) + 
  geom_histogram(bins = orientation5_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Orientation5", x="Orientation5", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[11,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[11,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[11,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[11,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



orientation5_hist


####################################################################################################################

orientation6_bins = round((summary_table[12,7] - summary_table[12,2])/(2*(summary_table[12,6] - summary_table[12,3])*(nrow(plant_data))^(-1/3)))


orientation6_hist <- ggplot(data=plant_data, aes(x=plant_data$Orientation6)) + 
  geom_histogram(bins = orientation6_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Orientation6", x="Orientation6", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[12,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[12,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[12,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[12,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



orientation6_hist

####################################################################################################################

orientation7_bins = round((summary_table[13,7] - summary_table[13,2])/(2*(summary_table[13,6] - summary_table[13,3])*(nrow(plant_data))^(-1/3)))


orientation7_hist <- ggplot(data=plant_data, aes(x=plant_data$Orientation7)) + 
  geom_histogram(bins = orientation7_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Orientation7", x="Orientation7", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[13,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[13,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[13,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[13,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



orientation7_hist


####################################################################################################################

orientation8_bins = round((summary_table[14,7] - summary_table[14,2])/(2*(summary_table[14,6] - summary_table[14,3])*(nrow(plant_data))^(-1/3)))


orientation8_hist <- ggplot(data=plant_data, aes(x=plant_data$Orientation8)) + 
  geom_histogram(bins = orientation8_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Orientation8", x="Orientation8", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[14,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[14,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[14,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[14,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



orientation8_hist


####################################################################################################################

orientation9_bins = round((summary_table[15,7] - summary_table[15,2])/(2*(summary_table[15,6] - summary_table[15,3])*(nrow(plant_data))^(-1/3)))


orientation9_hist <- ggplot(data=plant_data, aes(x=plant_data$Orientation9)) + 
  geom_histogram(bins = orientation9_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Orientation9", x="Orientation9", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[15,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[15,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[15,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[15,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



orientation9_hist


####################################################################################################################

leafw_bins = round((summary_table[16,7] - summary_table[16,2])/(2*(summary_table[16,6] - summary_table[16,3])*(nrow(plant_data))^(-1/3)))


leafw_hist <- ggplot(data=plant_data, aes(x=plant_data$Leaf.weight)) + 
  geom_histogram(bins = leafw_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Leaf Weight", x="Leaf Weight", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[16,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[16,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[16,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[16,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



leafw_hist


####################################################################################################################

leafa_bins = round((summary_table[17,7] - summary_table[17,2])/(2*(summary_table[17,6] - summary_table[17,3])*(nrow(plant_data))^(-1/3)))


leafa_hist <- ggplot(data=plant_data, aes(x=plant_data$LeafArea)) + 
  geom_histogram(bins = leafa_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Leaf Area", x="Leaf Area", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[17,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[17,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[17,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[17,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))



leafa_hist


####################################################################################################################

leafh_bins = round((summary_table[18,7] - summary_table[18,2])/(2*(summary_table[18,6] - summary_table[18,3])*(nrow(plant_data))^(-1/3)))


leafh_hist <- ggplot(data=plant_data, aes(x=plant_data$Leaf.Hue)) + 
  geom_histogram(bins = leafh_bins ,
                 color = "darkblue" ,fill="#99FFFF", 
                 aes(fill=..count..), show.legend = FALSE) +
  labs(title="Histogram for Leaf Hue", x="Leaf Hue", y="Frequency") + 
  geom_vline(aes(xintercept=summary_table[18,5], color="Mean"), size=1)+
  geom_vline(aes(xintercept=summary_table[18,4], color="Median"),linetype='longdash', size=1)+
  geom_vline(aes(xintercept=summary_table[18,3],color="First_Quantile"), size=1)+
  geom_vline(aes(xintercept=summary_table[18,6],color="Third_Quantile"), size=1) +
  theme(axis.line = element_line(size = 1, colour = "black"), legend.key = element_rect(fill = "white"), panel.background = element_blank())+
  
  scale_color_manual(name = "Statistics Labels", values = c(Mean = "#FF0000", Median = "#3399FF", First_Quantile = "#66CC33", Third_Quantile = "#FF6600"),
                     breaks = c("First_Quantile", "Mean", "Median", "Third_Quantile"))

leafh_hist


####################################################################################################################






