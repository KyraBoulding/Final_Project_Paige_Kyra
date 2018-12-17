

# Visulaize the weight of each metastic site corrisonding with each primary site
heat.map.weight <- ggplot(all.prime.sites.weight, aes(Primary_site, Matastatic_Site))+
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

#---- Binomial Test ---------------------------
# The goal of this section is to determine if certain primary tumor have greater
# of lesser chance of metastasis in the same site than normal. We determined the
# "normal" probability as the probability within the entire dataset.


# subset data into two tables: one where probabilities found are greater than the
# overall.prob for all primary sites and one where the prob is lower that the
# overall.prob
str(overall.prob)

# filtering by probability > overall.prob
higher.prob <- paired.prob.subset%>%
  filter(probability > overall.prob)

# filter by all values less than overall.prob
lower.prob <- paired.prob.subset%>%
  filter(probability < overall.prob)

# Run for loop on the higher.prob data. This for loop will run an upper tailed 
# binomial test in order to determine if the probability of metastasis in the
# primary tumor site is significantly greater in these specific cancer type than
# in the predetermined average (overall.prob)


for(i in 1:nrow(higher.prob)){

  l<- binom.test(higher.prob[i,4], higher.prob[i,2], overall.prob, alternative = "greater" )
  
  
  higher.prob[i,5] <- l$p.value #fill column in the table established in 
  # Data.Cleaning.1 Script
  higher.prob[i,6] <- l$conf.int[1]
  higher.prob[i,7] <- l$conf.int[2]
  
}
write.csv(higher.prob, paste(path.a, "Higher_Same_Site_Prob.csv"))

table.a <- qplot(1:150, 1:150, geom = "blank") + theme_bw() + theme(line = element_blank(), text = element_blank()) +
  # Then I add my table :
  annotation_custom(grob = tableGrob(higher.prob))

#save as a pdf to the Table folder
pdf(paste(path.t, "Table_Higher_Prob.pdf"))
plot(table.a)
dev.off()

# Now we will do the same with the lower.prob data set, except we will run a 
# lower tailed binomial test to determine if any of the cancer primary sites
# have significantly decreased probabilities of metastasis occuring in the same
# site.

for(i in 1:nrow(lower.prob)){
  
  l<- binom.test(lower.prob[i,4], lower.prob[i,2], overall.prob, alternative = "less" )
  
  
  lower.prob[i,5] <- l$p.value #fill column in the table established in 
  # Data.Cleaning.1 Script
  lower.prob[i,6] <- l$conf.int[1]
  lower.prob[i,7] <- l$conf.int[2]
  
}
write.csv(lower.prob, paste(path.a, "Lower_Same_Site_Prob.csv"))

library(gridExtra)



tt1 <- ttheme_default()
grid.arrange(tableGrob(lower.prob[1:30, 1:7], theme=tt1))


# save as a pdf
pdf(paste(path.t, "Table_Lower_Porbability.pdf"))
plot(table)
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


V(g)$color <- V(g)$type
V(g)$color=gsub("FALSE","salmon",V(g)$color)
V(g)$color=gsub("TRUE","light blue",V(g)$color)
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
plot(g, edge.color="gray30",edge.width=E(g)$weight, layout=coords,
     vertex.label.cex = 0.8, vertex.label.color = "black", vertext.lable.dis = c(rep(4, length(coords))),
     vertex.size = 7, asp=0.3) 

coords <- layout_as_bipartite(g)


######
install.packages("sna")
install.packages("network")
library(sna)
library(network)

relNet<-network(all.prime.sites.weight,ignore.eval = FALSE,names.eval='Weight',matrix.type='edgelist',directed=FALSE)
warnings()

# valued construct adjacency matrix
adjMat<-as.matrix(relNet,attrname='Weight')
# convert from distances to similarities
adjMat[adjMat!=0]<-4-adjMat[adjMat!=0]
# construct an appropriate geodesic distance matrix from the similarities
distMat<-geodist(adjMat,ignore.eval=FALSE,inf.replace = sqrt(network.size(relNet)))$gdist
# compute coords using distance matrix and kk algorithm
coords<-network.layout.kamadakawai(relNet,layout.par=list(elen=distMat))
# plot using precomputed coords
plot(relNet,displaylabels=TRUE,coord=coords,edge.label='Weight',edge.lwd='Weight')





