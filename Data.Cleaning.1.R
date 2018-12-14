################################################################################

#                        Data Cleaning 1                                       #

################################################################################


getwd()
setwd("1.Raw.Data")
raw.data <- read_xlsx("dataset_information.xlsx")
setwd("~/GitHub/Final_Project_Paige_Kyra")

library(ggplot2)
#===================== Visualize Data ==========================================

# look frequency of metastic sites in data set

metastasis.site.freq <- data.frame(table(raw.data$Metastasis_site))

plot(metastasis.site.freq)

