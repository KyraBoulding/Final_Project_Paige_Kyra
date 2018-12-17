

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
  theme(axis.text.x = element_text(size =8, angle=45))



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

g <- graph.data.frame(data.for.graph, directed = FALSE)





