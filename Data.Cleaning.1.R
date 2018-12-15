################################################################################

#                        Data Cleaning 1                                       #

################################################################################


getwd()
setwd("1.Raw.Data")
data <- read_xlsx("dataset_information.xlsx")
setwd("~/GitHub/Final_Project_Paige_Kyra")

library(ggplot2)
library(dplyr)
#===================== Visualize Data ==========================================

#---- Metastic Sites -----------------------------------------------------------

# look frequency of metastic sites in data set

metastasis.site.freq <- data.frame(table(data$Metastasis_site))
# 66 different metastic sites identified. 

# visualize frequency of metastic sites
ggplot(metastasis.site.freq, x=metastasis.site.freq$Var1, 
       y= metastasis.site.freq$Freq)+ 
  geom_col(aes(x=metastasis.site.freq$Var1,y= metastasis.site.freq$Freq))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(size =4, angle=45)) 

# lymph node is the highest frequency (2741), then liver, lung, unknown and brain
# many that are put down for multiple metastic sites... 

#---- Primary Sites ------------------------------------------------------------

# identifying primary sites
primary.site.freq <- data.frame(table(data$Primary_site))
# we have 41 different primary sites 

# plot
ggplot(primary.site.freq, x=primary.site.freq$Var1, 
       y= primary.site.freq$Freq)+ 
  geom_col(aes(x=primary.site.freq$Var1,y=primary.site.freq$Freq))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(size =4, angle=45)) 
# highest frequency primary site is breast, then colorectum, prostate and thyroid

