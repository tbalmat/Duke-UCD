# UCD Hierarchical edge bundle

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

# Get some data
# Connect to DB
db <- startGraph(c("http://localhost:7474/db/data/", "http://localhost:7479/db/data/")[1],
                 username="neo4j",
                 password=c("neo4j01", "Duke123!")[1])

x <- cypher(db, "
match  (prt:Participant)-[:P_SCT]->(sct0:ObjectConcept)-[rsct:ISA*]->(sct2:ObjectConcept)
where  1=1 and sct2.sctid in['52559000']
       and sct0.active='1' and sct2.active='1'

with   prt, sct0, startNode(last(rsct)) as sct1
where  sct1.active='1'

optional match(prt)-[:P_RX]->(rx0:RXCUI)
where  toLower(rx0.status)='active'

optional match(rx0)<-[:SUBSUMES]-(rx1:RXCUI)
where  toLower(rx1.status)='active'
with   prt, sct0, sct1, rx0, rx1

optional match(sct0)-[role1:HAS_ROLE_GROUP]->(rg)-[role2:FINDING_SITE]->(fst:ObjectConcept)
where  role2.active='1' and fst.active='1'
with distinct   prt.ParticipantId as participantID,
                case when(labels(prt)=['Participant'])then 'UCDDist'
                     when(labels(prt)=['Participant','UCD_Proximal'])then 'UCDProx'
                     else 'na'
                end as UCDProxDist, prt.Sex as Sex, prt.UCDDx as UCDDx, prt.HASxLast as HASxLast,
                case when(toInteger(prt.OnsetAgeDays)<11)then '0-11'
                     when(toInteger(prt.OnsetAgeDays)<101)then '11-100' 
                     when(toInteger(prt.OnsetAgeDays)<1001)then '101-1000'
                     when(toInteger(prt.OnsetAgeDays)<10001)then '1001-10000'
                     when(toInteger(prt.OnsetAgeDays) is not null)then '>10000'
                     else null   end as onsetAgeDays,   sct1.sctid as conceptID, sct1.FSN as conceptFSN,
                coalesce(rx1.id, rx0.id) as rxSubsID, coalesce(rx1.name, rx0.name) as rxSubsName,
                rx0.id as rxID, rx0.name as rxName, fst.sctid as findingSiteID, fst.FSN as findingSiteFSN,
                type(role2) as fsRole where 1=1
return   participantID, coalesce(UCDProxDist, 'na') as UCDProxDist,
         coalesce(Sex, 'na') as Sex, coalesce(HASxLast, 'na') as HASxLast,
         coalesce(UCDDx, 'na') as UCDDx, coalesce(onsetAgeDays, 'na') as onsetAgeDays,
         conceptID, conceptFSN,   coalesce(rxSubsID, 'na') as rxSubsID, coalesce(rxSubsName, 'na') as rxSubsName,
         coalesce(rxID, 'na') as rxID, coalesce(rxName, 'na') as rxName,
         coalesce(findingSiteID, 'na') as findingSiteID, coalesce(findingSiteFSN, 'na') as findingSiteFSN,
         coalesce(fsRole, 'na') as fsRole")

# Subset observations and create interaction(s)
y <- subset(x,
            UCDDx %in% c("ALD", "ASD") &
            rxSubsName %in% c("Arginine", "phenylbutyrate", "Buphenyl", "Sodium Benzoate", "Prevacid",
                              "Arginine hydrochloride", "Potassium chloride", "Sodium phenylbutyrate",
                              "Prograf", "Aspirin", "Ranitidine"))
y[,"UCD_HA"] <- paste(y[,"UCDDx"], "_X_", y[,"HASxLast"], sep="")

v <- c("UCD_HA", "conceptFSN", "rxSubsName")

# Create hierarchy
# The dendrogram has an origin for groups, an origin for each group, and leaves (try ggraph with circular=F)
# From and to, here, instruct from which graph origin a line originates and where it terminates
# The groups originate at the origin and termnate at a group
# The leaves originate at a group and terminate in a leaf (the unique label within the data for a group)
h <- rbind(data.frame("from"="origin", "to"="origin", "n"=length(unique(y[,"participantID"]))),
           data.frame("from"="origin", "to"=v, "n"=length(unique(y[,"participantID"]))),
           do.call(rbind, lapply(v,
                            function(v)
                              setNames(aggregate(1:nrow(y), by=list(rep(v, nrow(y)), y[,v]),
                                                 function(k) length(unique(y[k,"participantID"]))),
                                       c("from", "to", "n")))))

# Create vertices
vertex <- setNames(h[,c("to", "from", "n")], c("name", "v", "n"))

# Create igraph object
g <- graph_from_data_frame(h, vertices=vertex)

# Review plot and group origins
ggraph(g, layout='dendrogram', circular=F) + 
  geom_edge_diagonal() +
  theme_void()

# Add points
ggraph(g, layout='dendrogram', circular=F) +
  geom_node_point(aes(x=x, y=y, size=n)) +
  geom_edge_diagonal() +
  theme_void()

# Add labels
ggraph(g, layout='dendrogram', circular=F) +
  # Coordinates:
  # Plot extents are (0,0) to (n_leaves-1,n_levels-1), where n_levels is the number of levels in the
  # hierarchy (one for origin to origin, one for the groups with from="origin," and one for leaves)
  # leaves are placed at x=0..n_leaves-1, y=0
  geom_node_point(aes(x=x, y=y, size=n,
                      color=ifelse(v!="origin", v, ifelse(name!="origin", name, "Participants")))) +
  geom_node_text(aes(x=ifelse(v!="origin", x, x+1),
                     y=ifelse(v!="origin", y-0.2, y),
                     label=ifelse(name!="origin", name, "Participant"), #paste("(", x, ",", y, ")", sep=""),
                     color=ifelse(v!="origin", v, ifelse(name!="origin", name, "Participants")),
                     angle=ifelse(v!="origin", 90, 0),
                     vjust=0.5,
                     hjust=ifelse(v!="origin", 1, 0),
                     # Note that node and text sizes are from disjoint spaces, but ggplot does not
                     # support multiple scale_size functions
                     # Therefore, scale text the range of point sizes, which are based on participant
                     # counts
                     size=ifelse(v!="origin", 10, ifelse(name!="origin", 20, 30)))) +
  scale_color_manual(values=setNames(c("black",
                                       rgb(matrix(col2rgb("#FFEE66"), nrow=1)/390),
                                       rgb(matrix(col2rgb("#66AAFF"), nrow=1)/300),
                                       rgb(matrix(col2rgb("#88AAAA"), nrow=1)/280)), c("Participants", v)),
                                     guide=F) +
  #scale_size_manual(values=c("t1"=3, "t2"=4, "t3"=5), guide=F) +
  geom_edge_diagonal() +
  expand_limits(x=c(NA, length(which(h[,"from"]!="origin"))+5), y=c(-2.5, NA)) +
  theme_void() + theme(plot.margin=margin(0.25, 0.25, 0.25, 0.25, "in"))

# Include some edges
# Note that from and to correspond to positions in the hierarchy matrix (1 indicates the diagram root or
# center, with three variables, 2 through 4 indicate the group roots (where the lines bundle), and the
# remaining indices indicate the leaves)
ggraph(g, layout='dendrogram', circular=T) + 
  geom_edge_diagonal() +
  geom_conn_bundle(data=get_con(from=c(1,2,3), to=c(10, 20, 30)), alpha=1, width=1, colour="skyblue", tension = 1) +
  theme_void()

# Create edges using hierarchy row indices
# Note the assumption of unique variable, label combinations (aggreagte provides this)
edge <- do.call(rbind,
          apply(t(combn(1:3, 2)), 1,
            function(j) {
              # Index the first rows in hierarchy for each variable
              k1 <- which(h[,"from"]==v[j[1]])
              k2 <- which(h[,"from"]==v[j[2]])
              k10 <- min(k1)
              k20 <- min(k2)
              # Index labels into respective groups
              # Offset index into hierarchy based on least index for a variable
              data.frame("from"=k10+match(y[,v[j[1]]], h[k1,"to"])-1,
                         "to"=  k20+match(y[,v[j[2]]], h[k2,"to"])-1,
                         "v12"=paste(v[j[1]], "_", v[j[2]], sep=""))
            }))

# Draw all edges
ggraph(g, layout='dendrogram', circular=T) +
  #geom_edge_diagonal() +
  geom_conn_bundle(data=get_con(from=edge[,1], to=edge[,2], n=), alpha=1, width=1, colour="skyblue", tension = 1) +
  theme_void()

# Include points (nodes)
ggraph(g, layout='dendrogram', circular=T) +
  # Filter=leaf omits points for graph and group origins
  geom_node_point(aes(x=x*1.05, y=y*1.05, filter=leaf)) +
  # Draw lines to origins
  #geom_edge_diagonal(alpha=0.3) +
  #geom_conn_bundle(data=get_con(from=edge[,"from"], to=edge[,"to"], n=edge[,"n"]),
  #                 aes(width=n, color=..index.., alpha=1), tension=0.75) +
  geom_conn_bundle(data=get_con(from=edge[,"from"], to=edge[,"to"]),
                   color="blue", alpha=0.1, tension=0.75) +
  theme_void()

# Draw points, labels, and edges, sized and colored
xyfactor <- 3
glim <- c(-10, 10)
tsize <- 3.5
ecolor <- setNames(c(rgb(matrix((col2rgb("#FFEE66")+col2rgb("#66AAFF"))/2, nrow=1)/300),
                     rgb(matrix((col2rgb("#FFEE66")+col2rgb("#88AAAA"))/2, nrow=1)/300),
                     rgb(matrix((col2rgb("#66AAFF")+col2rgb("#88AAAA"))/2, nrow=1)/300)),
                   apply(t(combn(1:3, 2)), 1, function(j) paste(v[j[1]], "_", v[j[2]], sep="")))
png("C:\\Projects\\Duke\\Nursing\\SemanticsOfRareDisease\\UCD\\UCDApp\\EdgeBundle\\UCD-HA-Concept-Prescrip7.png",
    res=300, height=2400, width=2400)
ggraph(g, layout='dendrogram', circular=T) +
  # Draw arcs first, so that points overlay ends
  geom_conn_bundle(data=get_con(from=edge[,"from"], to=edge[,"to"], v12=edge[,"v12"]),
                   aes(x=x*xyfactor, y=y*xyfactor, color=v12), edge_width=0.35, alpha=0.1, tension=0.75) +
  scale_edge_color_manual(values=ecolor) +
  #geom_conn_bundle(data=get_con(from=edge[,"from"], to=edge[,"to"]), aes(color=..index..), alpha=0.1, tension=0.75) +
  #scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_point(aes(filter=leaf, x=x*xyfactor, y=y*xyfactor, size=n, color=v), alpha=0.8) +
  scale_size_continuous(range=c(3, 10)) +
  geom_node_text(aes(x=xyfactor*1.2*x, y=xyfactor*1.2*y, filter=leaf, label=name, color=v,
                     angle=ifelse((node_angle(x, y)+270)%%360 > 180, 0, 180)+node_angle(x, y),
                     vjust=ifelse((node_angle(x, y)+270)%%360 > 180, 0.25, 0.5)), size=tsize, hjust="outward") +
  scale_color_manual(values=setNames(c(rgb(matrix(col2rgb("#FFEE66"), nrow=1)/390),
                                       rgb(matrix(col2rgb("#66AAFF"), nrow=1)/300),
                                       rgb(matrix(col2rgb("#88AAAA"), nrow=1)/280)), v)) +
  theme_void() +
  theme(legend.position="none") +
  expand_limits(x=glim, y=glim)
dev.off()

# Hierarchical diagram with points, labels, and edges
ggraph(g, layout='dendrogram', circular=F) +
  # Coordinates:
  # Plot extents are (0,0) to (n_leaves-1,n_levels-1), where n_levels is the number of levels in the
  # hierarchy (one for origin to origin, one for the groups with from="origin," and one for leaves)
  # leaves are placed at x=0..n_leaves-1, y=0
  # Draw arcs first, so that points overlay ends
  geom_edge_diagonal(alpha=0.5) +
  geom_conn_bundle(data=get_con(from=edge[,"from"], to=edge[,"to"], v12=edge[,"v12"]),
                   aes(x=x, y=y, color=v12), edge_width=0.35, alpha=0.1, tension=0.75) +
  scale_edge_color_manual(values=ecolor, guide=F) +
  geom_node_point(aes(x=x, y=y, size=n,
                      color=ifelse(v!="origin", v, ifelse(name!="origin", name, "Participants")))) +
  geom_node_text(aes(x=ifelse(v!="origin", x, x+1),
                     y=ifelse(v!="origin", y-0.2, y),
                     label=ifelse(name!="origin", name, "Participant"), #paste("(", x, ",", y, ")", sep=""),
                     color=ifelse(v!="origin", v, ifelse(name!="origin", name, "Participants")),
                     angle=ifelse(v!="origin", 90, 0),
                     vjust=0.5,
                     hjust=ifelse(v!="origin", 1, 0),
                     # Note that node and text sizes are from disjoint spaces, but ggplot does not
                     # support multiple scale_size functions
                     # Therefore, scale text the range of point sizes, which are based on participant
                     # counts
                     size=ifelse(v!="origin", 10, ifelse(name!="origin", 20, 30)))) +
  scale_color_manual(values=setNames(c("black",
                                       rgb(matrix(col2rgb("#FFEE66"), nrow=1)/390),
                                       rgb(matrix(col2rgb("#66AAFF"), nrow=1)/300),
                                       rgb(matrix(col2rgb("#88AAAA"), nrow=1)/280)), c("Participants", v)),
                                     guide=F) +
  #scale_size_manual(values=c("t1"=3, "t2"=4, "t3"=5), guide=F) +
  expand_limits(x=c(NA, length(which(h[,"from"]!="origin"))+5), y=c(-2.5, NA)) +
  theme_void() + theme(plot.margin=margin(0.25, 0.25, 0.25, 0.25, "in"))

# Horizontal points and arcs (bipartite with a single column)
ggraph(g, layout='dendrogram', circular=F) +
  # Coordinates:
  # Plot extents are (0,0) to (n_leaves-1,n_levels-1), where n_levels is the number of levels in the
  # hierarchy (one for origin to origin, one for the groups with from="origin," and one for leaves)
  # leaves are placed at x=0..n_leaves-1, y=0
  # Draw arcs first, so that points overlay ends
  # Scale to avoid excessive overlay
  geom_conn_bundle(data=get_con(from=edge[,"from"], to=edge[,"to"], v12=edge[,"v12"]),
                   aes(x=x, y=y*as.integer(factor(v12))/length(unique(v12)), color=v12),
                   edge_width=0.5, alpha=0.1, tension=0.75) +
  scale_edge_color_discrete(guide=F) +
  geom_node_point(aes(filter=leaf, x=x, y=y, size=n, color=v)) +
  geom_node_text(aes(filter=leaf, x=x, y=y-0.05, label=name, color=v), size=2.75, vjust=0.5, hjust=1) +
  scale_color_discrete(guide=F) +
  #scale_color_manual(values=setNames(c("black",
  #                                     rgb(matrix(col2rgb("#FFEE66"), nrow=1)/390),
  #                                     rgb(matrix(col2rgb("#66AAFF"), nrow=1)/300),
  #                                     rgb(matrix(col2rgb("#88AAAA"), nrow=1)/280)), c("Participants", v)),
  #                                   guide=F) +
  coord_flip() +
  expand_limits(y=c(-1, NA)) +
  theme_void() + theme(plot.margin=margin(0.25, 0.25, 0.25, 0.25, "in"))

