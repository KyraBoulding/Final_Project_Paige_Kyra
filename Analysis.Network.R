






#=================== Visulaizing Weights with Heat Map ========================

#   This section contains code that creates a heat map of primary sites 
#   plotted against their metastasis sites. The colour gradient represents the
#   weight given to each metastasis site for it corrisonding primary site.
#   This heat map will allow detection of any over arching patterns in 
#   metastasis site. For example this heat map would show if the liver happend
#   to be the most common metastasis site regardless of primary site.


# Visulaize the weight of each metastic site corrisonding with each primary site
heat.map.weight <- ggplot(all.prime.sites.weight, 
                          aes(Primary_site, Matastatic_Site))+
  geom_raster(aes(fill = Weight))+
  labs(title ="Heat Map", x = "Primary Site", y = "Metastatic Site")+
  scale_fill_continuous(name = "Weight",  breaks = c(1.0, 0.75,0.5, 0.25, 0),
                        labels = c("1.0", "0.75", "0.5", "0.25", "0"),
                        low = "light blue", high = "navy")+  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(size =8, angle=45, vjust = 0.5))

# write as a pdf

pdf(paste(path.g, "HeatMap_AllPrimary.pdf"))
plot(heat.map.weight)
dev.off()

#============================= Binomial Test ===================================

#   The goal of this section is to examine whether certain primary sites have
#   higher probability than average of the metastasis occuring in the same site
#   as the primary tumor. We determined the probability of the primary and 
#   metastasis site being the same within all primary sites in the 
#   Data.Cleaning.1 Script (overall.prob). To compare this probability to the
#   probability within each primary site we will use a two-tailed binomal test.
#   This test will tell us whether there is significant difference between the 
#   probability we found for all primary sites and the ones found for each 
#   primary site.




for(i in 1:nrow(paired.prob.subset)){

  l<- binom.test(paired.prob.subset[i,4], paired.prob.subset[i,2],
                 overall.prob, alternative = "two.sided" )
  
  p.corrected<- (l$p.value)*(nrow(paired.prob.subset))  #making the correction
  # to the p-value for multiple comparisons. We are using Bonferroni which 
  # multiplies the p-value by the number of comparisons.
  
  paired.prob.subset[i,5] <- p.corrected #fill column in the table established in 
  # Data.Cleaning.1 Script
  paired.prob.subset[i,6] <- l$conf.int[1]
  paired.prob.subset[i,7] <- l$conf.int[2]
  
}
write.csv(paired.prob.subset, paste(path.a, "Same_Site_Prob.csv"))

# subset the table to the ones with significant p-values

subset.pval.prob <- paired.prob.subset%>%
  filter(P_value < 0.05)


#now we will deplay it as a table
library(grid)
library(gridExtra)

# create table with all the catagories with significantly different probabilities
# of haveing metastasis occur in the same site as the primary tumor
table.c<- grid.table(subset.pval.prob[1:9, 1:7], theme = ttheme_default())

pdf(paste(path.t, "Significant_Probs.pdf"))
plot(table.c)
dev.off()





#########
install.packages("igraph")
install.packages("networkD3")
library(igraph)
library(networkD3)
library(dplyr)
library(reshape2)
#

data.for.graph <- all.prime.sites.weight%>%
  select(Primary_site, Matastatic_Site, Weight)

names(data.for.graph)[3] <- "weight"
g <- graph.data.frame(data.for.graph, directed = F)
V(g)$type <- V(g)$name %in% data.for.graph[,2] #the second column of edges is TRUE type

coords <- layout_as_bipartite(g)

V(g)$color <- V(g)$type
V(g)$color=gsub("FALSE","salmon",V(g)$color)
V(g)$color=gsub("TRUE","light blue",V(g)$color)
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
plot(g, edge.color="gray30",edge.width=E(g)$weight, layout=coords,
     vertex.label.cex = 0.8, vertex.label.color = "black", vertext.lable.dis = c(rep(4, length(coords))),
     vertex.size = 7, asp=0.3) 










