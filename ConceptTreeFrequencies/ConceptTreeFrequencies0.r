# Exhaustively span SNOMED CT concepts in active paths of UCD database and accumulate unique
# participant frequencies for each node (concept) at each level of each path

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(RNeo4j)
library(ggplot2)
library(parallel)

setwd("C:\\Projects\\Duke\\Nursing\\SemanticsOfRareDisease\\UCD\\ConceptTreeFrequencies")

# Connect to DB
db <- startGraph(c("http://localhost:7474/db/data/", "http://localhost:7479/db/data/")[1],
                 username="neo4j",
                 password=c("neo4j01", "Duke123!")[1])

#################################################################################################
# Traverse concept paths from inital participant-concept relationship to root SNOMED concept
#################################################################################################

# Retrieve concepts with P_SCT edge to participants
# Create vector of 
psctid <- cypher(db, "
match(p:Participant)-[:P_SCT]->(x:ObjectConcept)
where x.active='1'
return x.sctid as sctid,
       reduce(a='', id in collect(distinct p.ParticipantId)|
              a+case when(a<>'')then ';' else '' end+id) as pid")
psctid[,"sctid"] <- as.numeric(psctid[,"sctid"])

# Convert pid string to a vector
pid <- lapply(1:nrow(psctid),
         function(i)
           as.numeric(strsplit(psctid[i,"pid"], ";", fixed=T)[[1]]))

# Retrieve paths from participant associated concepts to the root SNOMED concept
# Note that multiple paths may exist for each participant concept
# Assemble paths into a string of successive, connected concept IDs, beginning with root SNOMED
t0 <- proc.time()
path <- do.call(rbind,
          apply(as.matrix(psctid[,"sctid"]), 1,
            function(id)
              cypher(db,
                     paste(" match(x:ObjectConcept)-[r:ISA*]->(y:ObjectConcept)",
                           " where x.sctid='", id, "' and y.sctid='138875005'",
                           "       and all(r2 in r where r2.active='1'",
                           "               and startNode(r2).active='1' and endNode(r2).active='1')",
                           " return reduce(a='', r2 in r|endNode(r2).sctid+';'+a)+x.sctid as path",
                           sep=""))))[,"path"]
proc.time()-t0

# Convert paths into vectors of connected concept IDs
path2 <- lapply(path, function(p) as.numeric(strsplit(p, ";", fixed=T)[[1]]))

# Enumerate concepts in use
length(unique(unlist(path2))) 

# Create index relating sct IDs in paths to element postion in path2
p2Index <- do.call(rbind,
             lapply(1:length(path2),
               function(i)
                 data.frame("i"=i, "sctid"=path2[[i]])))

cl <- makePSOCKcluster(rep("localhost", 4))
clusterExport(cl, c("psctid", "path2", "pid", "p2Index"))

# Compose table of leading and trailing concepts for all concepts appearing in all
# paths associated with participants
t0 <- proc.time()
do.call(rbind,
  parApply(cl, as.matrix(unique(unlist(path2))[1:10]), 1,
    function(id) {
      # Retrieve sets of leading, centered, and trailing nodes, along with initial participant concept
      pathSegment <- do.call(rbind,
                       # Iterate through paths containing current concept
                       lapply(unique(p2Index[which(p2Index[,"sctid"]==id), "i"]),
                         function(i) {
                           k <- which(path2[[i]]==id)
                           nc <- length(path2[[i]])
                           data.frame("idFrom"=ifelse(k[1]<nc, path2[[i]][k[1]+1], 0),
                                      "id"=id,
                                      "idTo"=ifelse(k[1]>1, path2[[i]][k[1]-1], 0),
                                      "psctid"=path2[[i]][nc])
                         }))
      # Identify all leading and trailing nodes adjacent to the current concept
      # Enumerate unique leading and trailing node pairs for the current concept
      # Enumerate unique participants for each leading and trailing node pair
      z <- aggregate(1:nrow(pathSegment),
                by=list(pathSegment[,"idFrom"], pathSegment[,"id"], pathSegment[,"idTo"]),
                function(k)
                  c("nPaths"=length(k),
                    "nParticipant"=length(unique(unlist(
                                     lapply(unique(pathSegment[k,"psctid"]),
                                       function(sctid)
                                         pid[[which(psctid[,"sctid"]==sctid)]])
                                   )))
                  ))
      data.frame("idFrom"=z[,1], "id"=z[,2], "idTo"=z[,3], "nPaths"=z[,4][,1], "nParticipants"=z[,4][,2])
    }
  ))
proc.time()-t0

stopCluster(cl)

gc()

#################################################################################################
# Create histograms of participant frequency by covariate and number of concepts
#################################################################################################

xpc <- cypher(db, "
match  (p:Participant)-[r:P_SCT]->(x:ObjectConcept)
where  x.active='1'
return p.ParticipantId as ParticipantId, p.Sex as Sex, p.UCDDx as UCDDx,
       case when(labels(p)=['Participant'])then 'UCDDist'
            when(labels(p)=['Participant','UCD_Proximal'])then 'UCDProx'
            else 'na'
       end as UCDProxDist,
       case when(toLower(p.HASxLast)='ha events with or without symptoms')then 'HA+'
            when(toLower(p.HASxLast) in ['symptoms but no ha events', 'no reported ha symptoms'])then 'HA-'
            else 'na'
       end as HA,
       case when(toInteger(p.OnsetAgeDays)<11)then '0-11'
            when(toInteger(p.OnsetAgeDays)<101)then '11-100'
            when(toInteger(p.OnsetAgeDays)<1001)then '101-1000'
            when(toInteger(p.OnsetAgeDays)<10001)then '1001-10000'
            when(toInteger(p.OnsetAgeDays) is not null)then '>10000'
            else null
       end as onsetAgeDays,
       count(1) as n
order by p.ParticipantId")

xpc[,"onsetAgeDays"] <- factor(xpc[,"onsetAgeDays"], levels=c("0-11", "11-100", "101-1000", "1001-10000", ">10000", NA))

fn <- 
xscaleint <- 25

#png(paste(imgdir, "\\LegalTopics\\images\\DistributionDifferenceInCasesByTopic.png", sep=""), res=300, width=2400, height=2400)
ggplot() +
  geom_histogram(data=xpc, aes(x=n), color="gray85", fill="blue3") +
  #scale_x_continuous(limits=c(-2500, 500), labels=function(x) format(x, big.mark=",")) +
  scale_x_continuous(breaks=seq(0, max(xpc[,"n"])+xscaleint, xscaleint)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
  #facet_grid(UCDProxDist~Sex) +
  #facet_grid(UCDProxDist~UCDDx) +
  #facet_grid(UCDProxDist~HA) +
  #facet_grid(UCDProxDist~onsetAgeDays) +
  #facet_grid(Sex~UCDDx) +
  #facet_grid(Sex~HA) +
  #facet_grid(Sex~onsetAgeDays) +
  #facet_grid(UCDDx~HA) +
  #facet_grid(UCDDx~onsetAgeDays) +
  facet_grid(HA~onsetAgeDays) +
  theme(plot.title=element_text(size=12, hjust=0.5),
        plot.subtitle=element_text(size=10, hjust=0.5),
        plot.caption=element_text(size=10, hjust=0.5),
        panel.background=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_rect(fill=NA, color="gray75"),
        panel.spacing=unit(0, "inches"),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=10, angle=90, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=10),
        #axis.ticks=element_blank(),
        strip.text=element_text(size=8),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color=NA),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=10),
        legend.title=element_text(size=10)) +
  labs(x="\nconcept frequency", y="number of participants\n")
#dev.off()





