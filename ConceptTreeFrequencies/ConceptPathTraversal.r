# Exhaustively span SNOMED CT concepts in active paths of UCD database and accumulate unique
# participant frequencies for each node (concept) at each level of each path

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)
options(device="windows")

library(RNeo4j)
library(ggplot2)

#setwd("C:\\Projects\\Duke\\Nursing\\SemanticsOfRareDisease\\UCD\\ConceptTreeFrequencies")

# Connect to DB
db <- startGraph(c("http://localhost:7474/db/data/", "http://localhost:7479/db/data/")[2],
                 username="neo4j",
                 password=c("neo4j01", "Duke123!")[2])

# Retrieve participants and paths to concepts up to order 2
# Note that R saves Neo4j paths as a lists with elements for each relationship
x <- cypher(db, "
match(p:Participant)-[:P_SCT]->(x:ObjectConcept)-[r:ISA*1..2]->(y:ObjectConcept)
where x.active='1' and all(r2 in r where r2.active='1') and y.active='1'
return p.ParticipantId, r")

# Retrieve concepts with P_SCT edge to participants
# Enumerate participants by concept
x <- cypher(db, "
match(p:Participant)-[:P_SCT]->(x:ObjectConcept)
where x.active='1'
return x.sctid, count(distinct p.ParticipantId) as n")

# Retrieve paths from root to terminating concepts
# Note that multiple paths may exist for each terminating concept
# Paths are saved as a list with one element per relationship, each element
# containing the properties of the corresponding Neo4j path object
y <- cypher(db, "
match(x:ObjectConcept)-[r:ISA*]->(y:ObjectConcept)
where x.sctid='161891005' and y.sctid='138875005'
       and all(r2 in r where r2.active='1' and
               startNode(r2).active='1' and endNode(r2).active='1')
return x.sctid, r")

# Retrieve paths from root to terminating concepts
# Note that multiple paths may exist for each terminating concept
# Assemble paths into a string of successive, connected concept IDs
y <- cypher(db, "
match(x:ObjectConcept)-[r:ISA*]->(y:ObjectConcept)
where x.sctid='161891005' and y.sctid='138875005'
       and all(r2 in r where r2.active='1' and
               startNode(r2).active='1' and endNode(r2).active='1')
return reduce(a='', r2 in r|endNode(r2).sctid+';'+a)+x.sctid")

# Retrieve leading and trailing nodes in paths around a specified concept
# Enumerate unique participants associated with specified concept
id <- "102957003"
t0 <- proc.time()
cypher(db, paste(" match(p:Participant)-[:P_SCT]->(x:ObjectConcept)-[r:ISA*]->(y:ObjectConcept)",
                 " where (x.sctid='", id, "' or y.sctid='", id, "')",
                 "       and all(r2 in r where r2.active='1'",
                 "               and startNode(r2).active='1' and endNode(r2).active='1')",
                 " return x.sctid, y.sctid, count(distinct p.ParticipantId) as n", sep=""))
proc.time()-t0

cypher(db, paste(" match(p:Participant)-[:P_SCT]->(x:ObjectConcept {sctid:'", id, "'})",
                 " return x", sep=""))
 