# Exhaustively span SNOMED CT concepts in active paths of UCD database
# Assemble triplets for each concept appearing in any path associated with participants to
# indicate leading and trailing concepts
# Accumulate frequencies of path segment triplet occurrence and total unique participants
# for each triplet (union of all participants for all occurrences of a given triplet)

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

# Retrieve concepts associated with participants (P_SCT relationship)
psctid <- cypher(db, "
match(p:Participant)-[:P_SCT]->(x:ObjectConcept)
where x.active='1'
return x.sctid as sctid,
       reduce(a='', id in collect(distinct p.ParticipantId)|
              a+case when(a<>'')then ';' else '' end+id) as pid")

psctid <- list(
  "sctid"=psctid[,"sctid"],
  # Convert pid string to a vector of numeric IDs (for improved search performance)
  # Note that all pids are well below the double precision accuracy threshold
  "pid"=lapply(1:nrow(psctid),
          function(i)
            as.numeric(strsplit(psctid[i,"pid"], ";", fixed=T)[[1]])))

# Retrieve paths from participant associated concepts to the root SNOMED concept
# Note that multiple paths may exist for each participant concept
# Assemble paths into successive, connected concept IDs, beginning with root SNOMED
t0 <- proc.time()
psctid[["path"]] <- unlist(
                      lapply(psctid[["sctid"]],
                        function(id) {
                          x <- cypher(db,
                                      paste(" match(x:ObjectConcept)-[r:ISA*]->(y:ObjectConcept)",
                                            " where x.sctid='", id, "' and y.sctid='138875005'",
                                            "       and all(r2 in r where r2.active='1'",
                                            "               and startNode(r2).active='1' and endNode(r2).active='1')",
                                            " return reduce(a='', r2 in r|endNode(r2).sctid+';'+a)+x.sctid as path",
                                            sep=""))[,"path"]
                          # Parse path ID strings into vectors 
                          lapply(x, function(p) strsplit(p, ";", fixed=T)[[1]])
                        }), recursive=F)
proc.time()-t0

# Generate index version of all concepts appearing in paths
# Use of factors avoids loss of digits with lengthy sct IDs when converting to numeric
# Conversion from character dramatically improves search and comparison performance
# It is tempting to convert IDs to numeric, here, but certain of them overflow integer, and even
# numeric capacity (for instance, '12246311000119108' is converted to 12246311000119109, since the
# maximum integer that can be represented exactly as a double precision float, with 52 bits of
# precision, is 4503599627370495, one digit less than is needed)
sctidIndex <- sort(unique(unlist(psctid[["path"]])))

# Generate indexed version of initial concepts (with relationships to participants) and all paths
# These are simply positions in the sct ID index, are numeric, and improve efficiency of comparisons
# and searches
psctid[["isctid"]] <- apply(as.matrix(psctid[["sctid"]]), 1, function(id) match(id, sctidIndex))
psctid[["ipath"]] <- lapply(psctid[["path"]], function(p) match(p, sctidIndex))

# Enumerate concepts in use
length(unique(unlist(psctid[["path"]]))) 
length(sctidIndex)

# Create index relating sct IDs in paths to element postion in path list
# This is used to identify which path contains a given sct ID being searched for
sctidPathIndex <- do.call(rbind,
                    lapply(1:length(psctid[["ipath"]]),
                      function(i)
                        data.frame("i"=i, "sctid"=psctid[["ipath"]][[i]])))

gc()


cl <- makePSOCKcluster(rep("localhost", 4))
clusterExport(cl, c("psctid", "sctidPathIndex"))

# Compose table of leading and trailing concepts for all concepts appearing in all
# paths associated with participants
t0 <- proc.time()
conceptRelations <-
	do.call(rbind,
	   # Iterate through all (unique) sct IDs in all paths
    parApply(cl, as.matrix(unique(unlist(psctid[["ipath"]]))), 1,
      function(ix) {
        #print(ix)
        # Retrieve, for the current concept ID, sets of leading, centered, and trailing concepts,
      	 # along with initial participant concept
        pathSegment <- do.call(rbind,
                         # Iterate through all paths containing current concept ID
                         lapply(unique(sctidPathIndex[which(sctidPathIndex[,"sctid"]==ix), "i"]),
                           function(j) {
                           	 # Identify position of current concept ID in current (ith) path vector 
                             k <- which(psctid[["ipath"]][[j]]==ix)
                             nc <- length(psctid[["ipath"]][[j]])
                             # Return leading, trailing, current, and initial concepts
                             # Recall that concepts in path vector are ordered from root SNOMED
                             # in position1 to the participant associated concept in position nc
                             data.frame("ixFrom"=ifelse(k[1]<nc, psctid[["ipath"]][[j]][k[1]+1], 0),
                                        "ix"=ix,
                                        "ixTo"=ifelse(k[1]>1, psctid[["ipath"]][[j]][k[1]-1], 0),
                                        "psctix"=psctid[["ipath"]][[j]][nc])
                           }))
        # Enumerate unique leading and trailing node pairs for the current concept
        # Enumerate unique participants for each leading and trailing node pair
        z <- aggregate(1:nrow(pathSegment),
                       by=list(pathSegment[,"ixFrom"], pathSegment[,"ix"], pathSegment[,"ixTo"]),
                       function(k)
                         c(# Path frequency for each from, current, to concept set is the
                         	 # number of times that set occurs in the path segment list
                         	 "nPaths"=length(k),
                         	 # Unique participant frequency for each from, current, to set is
                         	 # the order (size) of the union of participants associated with
                           # the initial concept (psctid) indicated for each path through
                           # the from, center, to segment (indexed by k) 
                         	 # Iterate through k, retrieve participant IDs for each unique
                         	 # associated concept (at the path tail), concatenate (unlist),
                         	 # then measure the number of uniqe participant IDs
                           "nParticipant"=length(unique(unlist(
                                            lapply(unique(pathSegment[k,"psctix"]),
                                              function(sctix)
                                                psctid[["pid"]][[which(psctid[["isctid"]]==sctix)]]
                                            ))))
                      ))
        # Return from, current, to concept IDs, along with segment and participant frequencies
        data.frame("ixFrom"=z[,1], "ix"=z[,2], "ixTo"=z[,3], "nPaths"=z[,4][,1], "nParticipants"=z[,4][,2])
      }
  ))
proc.time()-t0

stopCluster(cl)
rm(cl)

gc()

# Append FSNs for from, center, and to concepts in each relationship triplet
# Executing this in parallel is tempting, but confidence in the ability of Ne04j to
# accomodate high density, simultaneous queries is not high
# Further, review of process during single-core execution reveals all cores in use (with 70%
# total CPU utilization), suggesting that Neo4j implements some method of parallelization
# in executing queries

# Query FSNs for each sct ID, in batches of idelta
np <- length(sctidIndex)
idelta <- 100
FSN <- do.call(rbind,
	       apply(as.matrix(seq(1, np, idelta)), 1,
	         function(i)
	       	   cypher(db, paste("match(x:ObjectConcept) where x.sctid in['",
	     	   	                  paste(unique(unlist(sctidIndex))[i:min(i+idelta-1, np)], collapse="', '", sep=""),
	     	 	                    "'] return x.sctid, x.FSN", sep=""))))
colnames(FSN) <- c("sctid", "FSN")

# Join FSNs to index positions
FSN[,"ix"] <- match(FSN[,"sctid"], sctidIndex)

# Join from, center, and to sct index values to sct IDs and FSNs
# Center
k <- which(conceptRelations[,"ix"]>0)
conceptRelations[k,"centerSCTID"] <- sctidIndex[conceptRelations[k,"ix"]]
conceptRelations[,"centerFSN"] <- FSN[match(conceptRelations[,"ix"], FSN[,"ix"]),"FSN"]
# From
k <- which(conceptRelations[,"ixFrom"]>0)
conceptRelations[k,"fromSCTID"] <- sctidIndex[conceptRelations[k,"ixFrom"]]
conceptRelations[,"fromFSN"] <- FSN[match(conceptRelations[,"ixFrom"], FSN[,"ix"]),"FSN"]
# To
k <- which(conceptRelations[,"ixTo"]>0)
conceptRelations[k,"toSCTID"] <- sctidIndex[conceptRelations[k,"ixTo"]]
conceptRelations[,"toFSN"] <- FSN[match(conceptRelations[,"ixTo"], FSN[,"ix"]),"FSN"]

# Save path segments
write.table(conceptRelations[,c("fromFSN", "fromSCTID", "centerFSN", "centerSCTID", "toFSN", "toSCTID",
                                "nPaths", "nParticipants")],
            "UCD-SNOMEDCT-Concept-Path-Segments.csv",
            row.names=F, col.names=T, sep=",", quote=c(1, 3, 5))

#################################################################################################
# Create histogram of concept frequency distribution
#################################################################################################

xc <- cypher(db, "
match  (p:Participant)-[r:P_SCT]->(x:ObjectConcept)
where  x.active='1'
return x.sctid, x.FSN, count(1) as n")

sum(xc[,"n"])
max(xc[,"n"])
xc[which(xc[,"n"]==224),]

png("Review\\images\\ConceptFrequency-LE30.png", res=300, width=2400, height=2400)
ggplot() +
  geom_histogram(data=xc[which(xc[,"n"]<=30),], aes(x=n), color="gray85", fill="blue3") +
  #scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25, 30)) +
  #scale_x_continuous(limits=c(0, 100)) +
  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
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
        axis.text.x=element_text(size=10),
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
  labs(x="\nparticipant assignment frequency", y="number of concepts\n")
dev.off()

# Visually verify frequencies
y <- cypher(db, "
match  (p:Participant)-[r:P_SCT]->(x:ObjectConcept)
where  x.active='1'
return x.sctid, x.FSN, p.ParticipantId
order by x.sctid, p.ParticipantId")

z <- aggregate(1:nrow(y), by=list(y[,"x.sctid"], y[,"x.FSN"]), length)
length(which(z[,"x"]==1))
nrow(z)
table(z[,"x"])

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

sum(xpc[,"n"])
min(xpc[,"n"])
which(xpc[,"n"]==1)
max(xpc[,"n"])
which(xpc[,"n"]==145)
median(xpc[,"n"])

xpc[,"onsetAgeDays"] <- factor(xpc[,"onsetAgeDays"], levels=c("0-11", "11-100", "101-1000", "1001-10000", ">10000", NA))

xscaleint <- 50
png("Review\\images\\Concepts-Per-Participant-HA-Age.png", res=300, width=2400, height=2400)
ggplot() +
  geom_histogram(data=xpc, aes(x=n), color="gray85", fill="blue3") +
  scale_x_continuous(breaks=seq(0, max(xpc[,"n"])+xscaleint, xscaleint)) +
  scale_y_continuous(labels=function(x) format(x, big.mark=",")) +
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
        axis.text.x=element_text(size=12, angle=90, hjust=1, vjust=0.5),
        axis.text.y=element_text(size=12),
        #axis.ticks=element_blank(),
        strip.text=element_text(size=12),
        strip.background=element_blank(),
        legend.position="bottom",
        legend.background=element_rect(color=NA),
        legend.key=element_rect(fill="white"),
        legend.box="horizontal",
        legend.text=element_text(size=10),
        legend.title=element_text(size=10)) +
  labs(x="\nconcept assignment frequency", y="number of participants\n")
dev.off()





