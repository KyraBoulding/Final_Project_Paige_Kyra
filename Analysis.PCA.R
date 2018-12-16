
#==== MCA ANALYSIS ====

head(breast.data)
breast.pca <- prcomp(breast.data[], center = TRUE,scale. = TRUE)

install.packages(c("FactoMineR", "factoextra"))
library(FactoMineR, factoextra)
breast.MCA <- MCA(breast.data, ncp =5 , graph = TRUE)

head(breast.data)
summary(breast.data)[, 1:4]

breast.mcs <- MCA(breast.data, graph = FALSE)

breast.data <- breast.data %>% mutate_if(is.character,as.factor)
head(breast.data)
str(breast.data)
