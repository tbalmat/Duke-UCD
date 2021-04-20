# Moustache plot using ggraph

# https://www.r-graph-gallery.com/311-add-labels-to-hierarchical-edge-bundling.html
# https://www.r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend.html
# https://ggraph.data-imaginist.com/index.html
# https://ggraph.data-imaginist.com/reference/index.html

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(RNeo4j)
library(igraph)
library(ggraph)

# Connect to DB
db <- startGraph(c("http://localhost:7474/db/data/", "http://localhost:7479/db/data/")[1],
                 username="neo4j",
                 password=c("neo4j01", "Duke123!")[1])

######################################################################################################
# Construct plot from randomly generated data
######################################################################################################

# Edges from the origin to groups and from groups to leaves
# Generate random edges in three levels
n <- 100
eij <- cbind(rep(0, n),
             sample(1:3, n, prob=c(1, 2, 3), replace=T), 
             sample(1:6, n, replace=T),
             sample(1:8, n, replace=T),
             sample(1:8, n, replace=T),
             sample(1:10, n, replace=T))
eij <- eij[which(!duplicated(eij[,1:(ncol(eij)-1)])),]

h2 <- rbind(
        data.frame("from"="0", "to"="0"),
        data.frame("from"="0", "to"=paste("0-", unique(eij[,2]), sep="")),
        unique(data.frame("from"=paste("0-", eij[,2], sep=""),
                          "to"=apply(as.matrix(eij[,1:3]), 1, function(x) paste(x, collapse="-", sep="")))),
        unique(data.frame("from"=apply(as.matrix(eij[,1:3]), 1, function(x) paste(x, collapse="-", sep="")),
                          "to"=apply(as.matrix(eij[,1:4]), 1, function(x) paste(x, collapse="-", sep="")))),
        do.call(rbind,
          apply(as.matrix(sample(1:nrow(eij), as.integer(nrow(eij)/2), replace=F)), 1, 
            function(i)
              unique(data.frame("from"=paste(eij[i,1:4], collapse="-", sep=""),
                                "to"=paste(eij[i,1:5], collapse="-", sep="")))))
      )

# Randomly duplicate some nodes
k <- sample(1:nrow(h2), 25, replace=F)
h2 <- rbind(h2, do.call(rbind,
                  apply(as.matrix(k), 1,
                    function(i) data.frame("from"=h2[i,"to"], "to"=paste(h2[i,"to"], "-b-", 1:sample(1:10, 1), sep="")))))

g <- graph_from_data_frame(h2, vertices=data.frame(unique(c(h2[,"from"], h2[,"to"])),
                                                   "label"=unique(c(h2[,"from"], h2[,"to"]))))

png("C:\\Projects\\Duke\\Nursing\\SemanticsOfRareDisease\\UCD\\UCDApp\\EdgeBundle\\Moustache.png", res=300, height=2400, width=2400)
# ..index.. is continous from 0 to 1 along the length of an edge
ggraph(g, layout=c("dendrogram", "linear", "treemap", "matrix")[1], circular=F) +
  #geom_edge_diagonal(aes(x=x, y=y, width=..index.., alpha=..index..), lineend="round") +
  #geom_edge_diagonal(aes(x=x, y=y)) +
  #geom_edge_bend() +
  geom_edge_elbow() +
  #geom_edge_bend(aes(width=..index.., alpha=..index..), lineend="round") +
  #geom_edge_fan(aes(width=..index.., alpha=..index..), lineend="round") +
  #geom_edge_tile(aes(x=x, y=y)) +
  scale_edge_width(range=c(0.2, 2.5), guide=F) +
  scale_edge_alpha(guide=F) +
  #geom_node_point(aes(x=x, y=y), size=3, color="blue3") +
  geom_node_text(aes(x=x, y=y, label=label), color="skyblue", angle=90) +
  theme_void() + theme(plot.margin=margin(0.25, 0.25, 0.25, 0.25, "in"))
dev.off()

######################################################################################################
# Construct plot from SNOMED concept relationships
######################################################################################################

id <- cypher(db, "match(x:ObjectConcept) where x.FSN='Motor dysfunction (finding)' return x.sctid limit 1")

q <- paste("match(x:ObjectConcept)-[r:ISA*1..2]->(y:ObjectConcept)
            where y.sctid='", id, "'
            unwind r as r2
            return startNode(r2).FSN as FSN1, endNode(r2).FSN as FSN2", sep="")
x <- cypher(db, q)
