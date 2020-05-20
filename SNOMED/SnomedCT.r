# SNOMED CT

options(max.print=1000)      # number of elements, not rows
options(stringsAsFactors=F)
options(scipen=999999)

library(RODBC)

# Identifiers sourced from the Unified Medical Language System
# https://www.nlm.nih.gov/healthit/snomedct/international.html

setwd("C:\\Projects\\Duke\\SemanticsOfRareDisease\\UnifiedMedicalLanguageSystem")

zf <- "SnomedCT_InternationalRF2_PRODUCTION_20190131T120000Z.zip"
zd <- "SnomedCT_InternationalRF2_PRODUCTION_20190131T120000Z/Full/Terminology"

concept <- read.table(unz(zf, paste(zd, "/sct2_Concept_Full_INT_20190131.txt", sep="")), header=T, sep="\t", as.is=T, strip.white=T, quote="", comment="") 
length(unique(concept[,"moduleId"]))
table(concept[,"moduleId"])
table(concept[,"definitionStatusId"])
#identifier <- read.table(unz(zf, paste(zd, "/sct2_Identifier_Full_INT_20190131.txt", sep="")), header=T, sep="\t", as.is=T) 
definition <- read.table(unz(zf, paste(zd, "/sct2_TextDefinition_Full-en_INT_20190131.txt", sep="")), header=T, sep="\t", as.is=T, strip.white=T, quote="", comment="") 
description <- read.table(unz(zf, paste(zd, "/sct2_Description_Full-en_INT_20190131.txt", sep="")), header=T, sep="\t", as.is=T, strip.white=T, quote="", comment="") 
table(description[,"caseSignificanceId"])
relationship <- read.table(unz(zf, paste(zd, "/sct2_Relationship_Full_INT_20190131.txt", sep="")), header=T, sep="\t", as.is=T, strip.white=T, quote="", comment="") 


# Diagnoses
# Some concept IDs are duplicated in concept table with multiple dates and definitionStatusId
k <- grep("diabet", description[,"term"])
x <- description[k[which(description[k,"active"]==1)],]
cbind(length(k), length(unique(x[,"conceptId"])))
length(which(concept[,"id"] %in% unique(x[,"conceptId"])))
y <- concept[which(concept[,"id"] %in% unique(x[,"conceptId"])),]

z <- definition[which(definition[,"conceptId"]==4855003),]

z <- relationship[which(relationship[,"sourceId"]==4855003),]
x2 <- description[which(description[,"conceptId"] %in% z[,"destinationId"]),]


# Verify that each description record has a corresponding concept record by concept ID and date
# Some are missing
z <- merge(description, concept, by.x=c("conceptId", "effectiveTime"), by.y=c("id", "effectiveTime"), all.x=T)


#################################################################################################################
# Create SQL database of SNOMED terms and relationships
#################################################################################################################

db <- odbcDriverConnect(connection="driver={SQL Server}; server=DESKTOP-BTIITJP\\SQLEXPRESS; database=master; trusted_connection=true", readOnlyOptimize=T)
sqlQuery(db, "drop database Snomed")
sqlQuery(db, "create database Snomed")
odbcClose(db)
db <- odbcDriverConnect(connection="driver={SQL Server}; server=DESKTOP-BTIITJP\\SQLEXPRESS; database=Snomed; trusted_connection=true", readOnlyOptimize=T)

#sqlQuery(db, "drop table concept")
#sqlQuery(db, "create table concept(id varchar(25) not null)")
#sqlQuery(db, "drop table description")
sqlQuery(db, "create table description(id varchar(25) not null, conceptId varchar(25) not null, moduleId varchar(25) not null,
              typeId varchar(25) not null, term varchar(255) not null, active bit not null, effectiveTime int not null,
              DukeSelection bit default(0), DukeID int identity primary key(id, conceptid, typeId, active, effectiveTime))")
#sqlQuery(db, "drop table definition")
sqlQuery(db, "create table definition(id varchar(25) not null, conceptId varchar(25) not null, moduleId varchar(25) not null,
              typeId varchar(25) not null, term varchar(1200) not null, active bit not null, effectiveTime int not null,
              DukeSelection bit default(0), DukeID int identity primary key(id, conceptid, typeId, active, effectiveTime))")
#sqlQuery(db, "drop table relationship")
sqlQuery(db, "create table relationship(id varchar(25) not null, sourceId varchar(25) not null, destinationId varchar(25) not null,
              moduleId varchar(25) not null, typeId varchar(25) not null, relationshipGroup	smallint not null,
              characteristicTypeId	varchar(25) not null, modifierId varchar(25) not null, active bit not null, effectiveTime int not null,
              DukeSelection bit default(0), DukeID int identity primary key(id, sourceid, destinationid, typeid, active, effectivetime))")

# Import all description, relationship, and definition source records

# Notes:
#
# Typically, multiple description records exist per date, with different caseSignificantId codes
# Treatment of case is indicated by the concept corresponding to caseSignificantId, but is ignored here
#
# Case:
# Multiple SNOMED terms have been observed that apparently refer to a single concept (Silvered leaf monkey, for
# instance).  The description table contains a column labeled 'caseSignificanceId' which refers to a concept
# indicating case sensitivity for the referred concept.  Since terms were imported exactly as they appear in
# the source data, care must be taken when searching for values in a case sensitive environment.  SQL Server
# (where the imported data reside) is not case sensitive, while R and Neo4j are.
# It is suggest to use the R lower() function when comparing text values.
#
# Inactive relationships (apparent method of declaring obsolescence):
# Active and inactive relationships have been observed for the most recent date appearing for a single concept
# A review of the SNOMED browser (https://browser.ihtsdotools.org) for select cases (Silvered leaf monkey) reveals
# active concepts and relationships - therefore, a concept can be considered active if it appears as both active
# and inactive in the source data
#

# Import descriptions
# Insert in batches of idelt records
idelt <- 100
n <- nrow(description)
sqlQuery(db, "truncate table description")
for(i in seq(1, n, idelt)) {
  k <- i:min(i+idelt-1, n)
  sql <- paste("insert into description(id, conceptId, moduleId, typeId, term, active, effectiveTime) values ",
               paste(paste("('", description[k,"id"], "', '", description[k,"conceptId"], "', '",
                           description[k,"moduleId"], "', '", description[k,"typeId"], "', '",
                           gsub("'", "''", description[k,"term"]), "', ", description[k,"active"], ", ",
                           description[k,"effectiveTime"], ")", sep=""), sep="", collapse=", "), sep="")
  x <- sqlQuery(db, sql)
  if(length(x)>0)
    print(c(i, x[1]))
}
sqlQuery(db, "create index INdescription_conceptId on description(conceptId)")
sqlQuery(db, "create index INdescription_typeId on description(typeId)")
sqlQuery(db, "create index INdescription_term on description(term)")
sqlQuery(db, "create index INdescription_active on description(active)")
sqlQuery(db, "create index INdescription_effectiveTime on description(effectiveTime)")
sqlQuery(db, "create index INdescription_DukeSelection on description(DukeSelection)")

# Import definitions
idelt <- 100
n <- nrow(definition)
sqlQuery(db, "truncate table definition")
for(i in seq(1, n, idelt)) {
  k <- i:min(i+idelt-1, n)
  sql <- paste("insert into definition (id, conceptId, moduleId, typeId, term, active, effectiveTime) values ",
               paste(paste("('", definition[k,"id"], "', '", definition[k,"conceptId"], "', '",
                           definition[k,"moduleId"], "', '", definition[k,"typeId"], "', '",
                           gsub("'", "''", definition[k,"term"]), "', ", definition[k,"active"], ", ",
                           definition[k,"effectiveTime"], ")", sep=""), sep="", collapse=", "), sep="")
  x <- sqlQuery(db, sql)
  if(length(x)>0)
    print(c(i, x[1]))
}
sqlQuery(db, "create index INdefinition_conceptId on definition(conceptId)")
sqlQuery(db, "create index INdeinition_typeId on definition(typeId)")
sqlQuery(db, "create index INdefinition_term on definition(term)")
sqlQuery(db, "create index INdefinition_active on definition(active)")
sqlQuery(db, "create index INdefinition_effectiveTime on definition(effectiveTime)")
sqlQuery(db, "create index INdefinition_DukeSelection on definition(DukeSelection)")
#sqlQuery(db, "alter table definition add constraint FKdefinition_description foreign key (conceptId) references description(conceptId)")

# Relationships
idelt <- 100
n <- nrow(relationship)
sqlQuery(db, "truncate table relationship")
for(i in seq(1, n, idelt)) {
  k <- i:min(i+idelt-1, n)
  sql <- paste("insert into relationship(id, sourceId, destinationId, moduleId, typeId, ",
               "                         relationshipGroup, characteristicTypeId, modifierId, ",
               "                         active, effectiveTime) values ",
               paste(paste("('", relationship[k,"id"], "', '", relationship[k,"sourceId"], "', '",
                           relationship[k,"destinationId"], "', '", relationship[k,"moduleId"], "', '",
                           relationship[k,"typeId"], "', ", relationship[k,"relationshipGroup"], ", '",
                           relationship[k,"characteristicTypeId"], "', '", relationship[k,"modifierId"], "', ",
                           relationship[k,"active"], ", ", relationship[k,"effectiveTime"], ")", sep=""),
                     sep="", collapse=", "), sep="")
  x <- sqlQuery(db, sql)
  if(length(x)>0)
    print(c(i, x[1]))
}
sqlQuery(db, "create index INrelation_source_dest on relationship(sourceId)")
sqlQuery(db, "create index INrelation_dest on relationship(destinationId)")
sqlQuery(db, "create index INrelation_type on relationship(typeId)")
sqlQuery(db, "create index INrelationshipGroup on relationship(relationshipGroup)")
sqlQuery(db, "create index INmodifierId on relationship(modifierId)")
sqlQuery(db, "create index INactive on relationship(active)")
sqlQuery(db, "create index INeffectiveTime on relationship(effectiveTime)")
sqlQuery(db, "create index INrelationship_DukeSelection on relationship(DukeSelection)")
#sqlQuery(db, "alter table relationship add constraint FKrelation_source_description foreign key (sourceId) references description(conceptId)")
#sqlQuery(db, "alter table relationship add constraint FKrelation_destination_description foreign key (destinationId) references description(conceptId)")

odbcClose(db)


######################################################################################################################
# Generate csv files containing vertex and edge data
# These are used by the Neo4j load command
# With an index on the vid attribute of vertices, Neo4j requires approximately one second to create each edge
#
# A NOTE ON CASE:
# Multiple SNOMED terms have been observed that apparently refer to a single concept (Silvered leaf monkey, for
# instance).  The description table contains a column labeled 'caseSignificanceId' which refers to a concept
# indicating case sensitivity for the referred concept.  Since terms were imported exactly as they appear in
# the source data, care must be taken when searching for values in a case sensitive environment.  SQL Server
# (where the imported data reside) is not case sensitive, while R and Neo4j are.
# It is suggest to use the R lower() function when comparing text values.
######################################################################################################################

db <- odbcDriverConnect(connection="driver={SQL Server}; server=DESKTOP-BTIITJP\\SQLEXPRESS; database=Snomed; trusted_connection=true", readOnlyOptimize=T)

interm <- c("",
            "hepatitis", "multiple sclerosis", "psoriasis",
            "ape", "monkey", "human", "silvered leaf monkey", "order primates", "abert''s squirrel",
            "biological function", "laparoscopic repair of hernia",
            "body structure", "entire body as a whole", "body organ", "bone", "ankle",
            "thumb", "digit of hand",
            ".45 inch caliber bullet, device", "boomerang",
            "bookkeeper", "language", "cognitive functions", "color",
            "boston")[8]
liketerm <- c("", "%step child%", "%stepchild%", "%hominidae%", "hand structure%", "%squirrel%", "bookkeep%")[1]
dir <- c("in", "out")[1:2]
concepttype <- c("", "fully specified name", "synonym")[3]

sql <- paste("Neo4jNetConfiguration @style='', @likeTerm='", liketerm, "', @inTerm='", paste(interm, collapse="|", sep=""),
             "', ", "@dir='", paste(dir, collapse="-", sep=""), "', @conceptType='", concepttype, "'", sep="")
#writeLines(sql)

x <- sqlQuery(db, sql)
write.table(x[which(x[,"rectype"]=="vertex"),c("conceptID1", "term", "type")], "Neo4j\\Snomed-Neo4j-All-vertex.csv",
            row.names=F, col.names=c("conceptID", "term", "type"), sep=",", quote=T)
write.table(x[which(x[,"rectype"]=="edge"),c("conceptID1", "conceptID2", "type")], "Neo4j\\Snomed-Neo4j-All-edge.csv",
            row.names=F, col.names=c("v1", "v2", "type"), sep=",", quote=F)

odbcClose(db)


######################################################################################################################
# Generate Neo4j instructions to create graph database for a specified set of conditions
# Note that all vertex and edge creation instructions must be executed from within a single batch
# An output file is appended as needed to create a single set of database instructions 
######################################################################################################################

db <- odbcDriverConnect(connection="driver={SQL Server}; server=DESKTOP-BTIITJP\\SQLEXPRESS; database=Snomed; trusted_connection=true", readOnlyOptimize=T)

term <- "%diabetes%"

# Vertices
# Note that Neo4j requires vertex labels to begin with an alpha character ("c" for condition here)
# Default union behavior is distinct, which eliminates duplicated vertex records
sql <- paste(" select 'create (c' + conceptId + ':condition {id:\"' + conceptId + '\", term:\"' + term + '\"})'",
             " from   description",
             " where  term like '", term, "'",
             # Include all destination vertices on edges with specified source (so that edges can be constructed)
             " union",
             " select 'create (c' + d2.conceptId + ':condition {id:\"' + d2.conceptId + '\", term:\"' + d2.term + '\"})'",
             " from   description d1 join relationship r on d1.conceptId=r.sourceId",
             "        join description d2 on r.destinationId=d2.conceptId",
             " where  d1.term like '", term, "'", sep="")
write.table(sqlQuery(db, sql), paste("Neo4j\\Snomed-Neo4j-createDB-", gsub("\\%", "", term), ".sql", sep=""),
            row.names=F, col.names=F, sep="\n", quote=F)

# Edges
# Note the prepending of source and destination IDs with a "c" to be consistent with vertex labels
sql <- paste(" select 'create (c' + sourceId + ')-[:associated]->(c' + destinationId + ')'",
             " from   relationship",
             " where  sourceId in(select conceptId from description where term like '", term, "')", sep="")
write.table(sqlQuery(db, sql), paste("Neo4j\\Snomed-Neo4j-createDB-", gsub("\\%", "", term), ".sql", sep=""),
            row.names=F, col.names=F, sep="\n", quote=F, append=T)

odbcClose(db)


######################################################################################################################
# Generate csv or SQL instruction files containing vertex and edge data
# These are used by the Neo4j load command or executed directly by the Neo4j command interpreter
# With an index on the vid attribute of vertices, Neo4j requires approximately one second to create each edge
# In general, these methods are infeasible when all concepts (description table) are loaded, even with an index
######################################################################################################################

db <- odbcDriverConnect(connection="driver={SQL Server}; server=DESKTOP-BTIITJP\\SQLEXPRESS; database=Snomed; trusted_connection=true", readOnlyOptimize=T)

# Vertices
# Note that Neo4j requires vertex labels to begin with an alpha character ("c" for concept here)
# Creation of approximately 440,000 vertices requires approximately six minutes
sql <- "select 'c' + conceptId as conceptID, term from description"
write.table(sqlQuery(db, sql), "Neo4j\\Snomed-Neo4j-All-vertex.csv", row.names=F, col.names=T, sep=",", quote=T)

# Edges
# Note the prepending of source and destination IDs with a "c" to be consistent with vertex labels
sql <- "select 'c' + sourceId as sourceID, 'c' + destinationId as destinationID
        from   relationship
        where  convert(bigint, convert(real, id)/10000000)=0"
x <- sqlQuery(db, sql)

# File for load csv method 
#write.table(x, "Neo4j\\Snomed-Neo4j-All-edge.csv", row.names=F, col.names=T, sep=",", quote=F, append=F)

# Compose Neo4j instruction to create relations for n edges
# In an attempt to break query and insert into steps, and to leverage the index on vertices,
# the following selects source nodes as v1, followed by selection of source, destination node pairs
# from which to construct edges
# However, even with small n (100), where no vertices are paired with more than three destination nodes,
# execution requires more than 20 minutes (how much more, we do not know, due to forced termination)
# With over two million edges to create, this method is infeasible
n <- 100
y <- paste("match (v1) where v1.vid in['", paste(unique(x[,"sourceID"])[1:n], collapse="', '"), "'] ",
           "with v1 match(v2) where ",
           paste(apply(as.matrix(unique(x[,"sourceID"])[1:n]), 1,
                   function(id1)
                     paste("v1.id='", id1, "' and v2.vid in['",
                           paste(x[which(x[,"sourceID"]==id1), "destinationID"], collapse="', '", sep=""),
                           "'] ", sep="")),
                 collapse=" or "),
           "merge (v1)-[:related {v1:v1.vid, v2:v2.vid}]-(v2)", sep="")

# Create csv file containing source and destination IDs for edges
# This is used by the load csv command
# With even a small number of edges (100) load csv requires at least 20 minute so complete (how
# much more is not known, due to forced termination)
# With over two million edges to create, this method is infeasible
write.table(y, "Neo4j\\Snomed-Neo4j-All-edge.csv", row.names=F, col.names=T, sep=",", quote=F, append=F)

odbcClose(db)


#############################################################################################################
# Good ideas, but infeasible (extremely lengthy time required to load DB), due to number of edges generated
#############################################################################################################

# Generate edges for all vertices that have any path, regardless of length, to any selected vertex

# Vertices
# Note that Neo4j requires vertex labels to begin with an alpha character ("c" for concept here)
sql <- paste(" select 'c' + conceptID as conceptID, term from description where term like '", term, "'", sep="")
write.table(sqlQuery(db, sql), "Neo4j\\Snomed-Neo4j-All-vertex.csv", row.names=F, col.names=T, sep=",", quote=T)

# Edges
# Note the prepending of source and destination IDs with a "c" to be consistent with vertex labels


sql <- "select 'c' + sourceId as sourceID, 'c' + destinationId as destinationID
        from   relationship
        where  convert(bigint, convert(real, id)/10000000)=0"
x <- sqlQuery(db, sql)

# File for load csv method 
#write.table(x, "Neo4j\\Snomed-Neo4j-All-edge.csv", row.names=F, col.names=T, sep=",", quote=F, append=F)

# Compose Neo4j instruction to create relations for n edges
# In an attempt to break query and insert into steps, and to leverage the index on vertices,
# the following selects source nodes as v1, followed by selection of source, destination node pairs
# from which to construct edges
# However, even with small n (100), where no vertices are paired with more than three destination nodes,
# execution requires more than 20 minutes (how much more, we do not know, due to forced termination)
# With over two million edges to create, this method is infeasible
n <- 100
y <- paste("match (v1) where v1.vid in['", paste(unique(x[,"sourceID"])[1:n], collapse="', '"), "'] ",
           "with v1 match(v2) where ",
           paste(apply(as.matrix(unique(x[,"sourceID"])[1:n]), 1,
                   function(id1)
                     paste("v1.id='", id1, "' and v2.vid in['",
                           paste(x[which(x[,"sourceID"]==id1), "destinationID"], collapse="', '", sep=""),
                           "'] ", sep="")),
                 collapse=" or "),
           "merge (v1)-[:related {v1:v1.vid, v2:v2.vid}]-(v2)", sep="")

# Create csv file containing source and destination IDs for edges
# This is used by the load csv command
# With even a small number of edges (100) load csv requires at least 20 minutes so complete (how
# much more is not known, due to forced termination)
# With over two million edges to create, this method is infeasible
write.table(y, "Neo4j\\Snomed-Neo4j-All-edge.csv", row.names=F, col.names=T, sep=",", quote=F, append=F)

odbcClose(db)


#############################################################################################################
# Query Neo4j database using neo4r package
# Note that several serious problems are encountered when using neo4r, including
# 1. Results are ALWAYS returned as a tibble, even when rectangular results are generated
# 2. Use of the Neo4j function labels() causes an "invalid subscript" error
# 3. as.data.frame(result_tibble) strips column names
#############################################################################################################

library(neo4r)
con <- neo4j_api$new(url="http://localhost:7474", user="neo4j", password="neo4j01")
cypher <- "match(x) return x limit 100"
x <- call_neo4j(cypher, con, type=c("row", "graph")[1], output=c("r","json")[1], include_stats=F, include_meta=F)
as.data.frame(x)

# Type II diabetes disorders
cypher <- "
match(x:condition)
where toLower(x.term) contains 'type 2' or toLower(x.term) contains 'type ii'
return x
order by x.term"
x <- call_neo4j(cypher, con, type=c("row", "graph")[1], output=c("r","json")[1], include_stats=F, include_meta=F)

# Conditions with at least four successive associations
cypher <- "
match(x1:condition)-[:associated]->(x2:condition)
where toLower(x1.term) contains 'gestational diabetes' and not toLower(x1.term) contains 'pregestational diabetes'
with x1, x2
match(x2:condition)-[:associated]->(x3:condition)
with x1, x2, x3
match(x3:condition)-[:associated]->(x4:condition)
with x1, x2, x3
match(x3:condition)-[:associated]->(x5:condition)
return x3, x5;"
x <- call_neo4j(cypher, con, type=c("row", "graph")[1], output=c("r","json")[1], include_stats=F, include_meta=F)

# Path
cypher <- "
match (x1:condition)-[p:associated]->(x2:condition)
where toLower(x1.term) contains 'gestational diabetes' and not toLower(x1.term) contains 'pregestational diabetes'
return p;"
x <- call_neo4j(cypher, con, type=c("row", "graph")[2], output=c("r","json")[1], include_stats=F, include_meta=F)

#############################################################################################################
# Query UCD (Neo4j) database
# More neo4r (ill-advised)
# Set active DB in C:\Software\Neo4j\neo4j-community-3.5.8\conf\neo4j.conf
#############################################################################################################

library(neo4r)

rm(dbcon)

# Do user and password have any effect?
dbcon <- neo4j_api$new(url="http://localhost:7474", user="neo4j", password="neo4j01")

# Participants
query <- "match(x:Participant) return x"
x <- as.data.frame(call_neo4j(query, dbcon, type=c("row", "graph")[1], output=c("r","json")[1], include_stats=F, include_meta=F))

# Labels
query <- "match(x:ObjectConcept) return distinct(labels(x)) as label"
x <- as.data.frame(call_neo4j(query, dbcon, type=c("row", "graph")[1], output=c("r","json")[1], include_stats=F, include_meta=F))

# SNOMED root (contains)
query <- "match(x:ObjectConcept) where x.FSN contains 'SNOMED CT Concept' and x.active='1' return labels(x) as label, x.sctid as sctid, x.FSN as FSN limit 1"
x <- as.data.frame(call_neo4j(query, dbcon, type=c("row", "graph")[1], output=c("r","json")[1], include_stats=F, include_meta=F))

# SNOMED root (=)
query <- "match(x:ObjectConcept) where x.FSN='SNOMED CT Concept (SNOMED RT+CTV3)' and x.active='1' return x limit 1"
x <- setNames(as.data.frame(call_neo4j(query, dbcon, type=c("row", "graph")[1], output=c("r","json")[1], include_stats=F, include_meta=F))[1,c("x.FSN", "x.sctid")], c("FSN", "sctid"))

query <- "match(x:ObjectConcept) where x.FSN contains('SNOMED CT Concept (SNOMED RT+CTV3)') and x.active='1' return x.sctid as sctid, x.FSN as FSN"
x <- call_neo4j(query, dbcon, type=c("row", "graph")[1], output=c("r","json")[1], include_stats=F, include_meta=F)
length(x)
x[[1]]
names(x)
x[[2]]
setNames(as.data.frame(x), names(x))

query <- "match(x:ObjectConcept) where x.FSN='SNOMED CT Concept (SNOMED RT+CTV3)' and x.active='1' return labels(x) as label, x.sctid as sctid, x.FSN as FSN limit 1"
# Eee-gads!  Handhold result generation due to call_neo4j() returning a tibble and list names disapperaing during as.data.frame
x <- call_neo4j(query, dbcon)

query <- "match(x:ObjectConcept) where x.FSN contains 'SNOMED CT Concept' and x.active='1' return x.sctid, x.FSN limit 1"
x <- as.data.frame(call_neo4j(query, dbcon, type=c("row", "graph")[1], output=c("r","json")[1], include_stats=F, include_meta=F))

#############################################################################################################
# Query UCD (Neo4j) database using the RNeo4j package
# Set active DB in C:\Software\Neo4j\neo4j-community-3.5.8\conf\neo4j.conf
#############################################################################################################

library(RNeo4j)

rm(db)

# Do user and password have any effect?  Yes.
db <- startGraph("http://localhost:7474/db/data/", username="neo4j", password="neo4j01")

# Participants
# Note that, since entire nodes are selected, the result set is a list with one element per node
query <- "match(x:Participant) return x"
x <- cypherToList(db, query)
x[[1]]

# Labels are returned
query <- "match(x:ObjectConcept) return distinct(labels(x)) as label"
x <- cypher(db, query)

# SNOMED root (contains)
query <- "match(x:ObjectConcept) where x.FSN contains 'SNOMED CT Concept' and x.active='1' return labels(x) as label, x.sctid as sctid, x.FSN as FSN limit 1"
x <- cypher(db, query)

# Prescriptions
query <- "match(x:RXCUI) return x limit 1"
x <- cypherToList(db, query)

# Note the direction of prescription to participant relationships (from part to pre) 
query <- "match(w:RXCUI)-[:P_RX]->(x:Participant)-[:P_SCT]->(y:ObjectConcept)-[:ISA]->(z:ObjectConcept) return w limit 1"
query <- "match(w:RXCUI)<-[:P_RX]-(x:Participant)-[:P_SCT]->(y:ObjectConcept)-[:ISA]->(z:ObjectConcept) return w limit 1"
x <- cypherToList(db, query)

query <- "match (x)-[r]->(y) return distinct labels(x), labels(y), type(r), count(1) order by labels(x), labels(y), type(r)"
x <- cypher(db, query)

#############################################################################################################
# Verify unique aggregated node and edge participant counts
# Instructions are taken from the UCD Shiny app
#############################################################################################################

library(RNeo4j)
db <- startGraph("http://localhost:7474/db/data/", username="neo4j", password="neo4j01")

query <- "match(x:Participant)-[:P_SCT]->(y:ObjectConcept)-[r:ISA*]->(z:ObjectConcept)
          where z.sctid='106145009' and y.active='1' and z.active='1'
          with case when(labels(x)=['Participant'])then 'UCDDist'
                    when(labels(x)=['Participant','UCD_Proximal'])then 'UCDProx' end as UCDProxDist,
               x.ParticipantId as participantID, x.Sex as Sex, x.UCDDx as UCDDx,
               case when(toInteger(x.OnsetAgeDays)<11)then '0-11'
                    when(toInteger(x.OnsetAgeDays)<101)then '11-100'
                    when(toInteger(x.OnsetAgeDays)<1001)then '101-1000'
                    when(toInteger(x.OnsetAgeDays)<10001)then '1001-10000'
                    when(toInteger(x.OnsetAgeDays) is not null)then '>10000'
                    else null end as onsetAgeDays, startNode(last(r)) as concept
          return  UCDProxDist, participantID, Sex, UCDDx, onsetAgeDays,
                  concept.sctid as conceptID, labels(concept) as conceptLabel,
                  concept.FSN as FSN, count(1) as n"

pConcept <- cypher(db, query)
nodeVar1 <- "UCDDx"
nodeVar2 <- "FSN"
accMethod <- "PID"

# Tabulate vertices
# Vertex set v1 corresponds to nodeVar1, set v2 to nodeVar2
# If accMethod is unrecognized then frequencies for unique values of nodeVar1 and nodeVar2 are accumulated
v1 <- aggregate(1:nrow(pConcept), by=list(pConcept[,nodeVar1]),
                function(k)
                  if(tolower(accMethod) %in% c("pid", "v1pidv2nen", "v1pidv2piden")) {
                    length(unique(pConcept[k,"participantID"]))
                  } else {
                    length
                  })
colnames(v1) <- c("lab", "n")
v2 <- aggregate(1:nrow(pConcept), by=list(pConcept[,nodeVar2]),
                function(k)
                  if(tolower(accMethod) %in% c("pid", "v1pidv2piden")) {
                    length(unique(pConcept[k,"participantID"]))
                  } else if(tolower(accMethod)=="v1pidv2nen") {
                    sum(pConcept[k,"n"])
                  } else {
                    length
                  })
colnames(v2) <- c("lab", "n")

length(unique(pConcept[,"participantID"]))

agglist <- list(pConcept[,nodeVar1], pConcept[,nodeVar2])
cname <- c("v1", "v2", "n")
edat <- aggregate(1:nrow(pConcept), by=agglist,
                  function(k)
                    if(tolower(accMethod)=="pid") {
                      length(unique(pConcept[k,"participantID"]))
                    } else if(tolower(accMethod) %in% c("v1pidv2piden", "v1pidv2nen")) {
                      sum(pConcept[k,"n"])
                    } else {
                      length
                    })
colnames(edat) <- cname

k <- which(pConcept[,nodeVar2]=="Motor coordination finding (finding)")
w <- pConcept[k,c(nodeVar1, nodeVar2, "participantID")]

#############################################################################################################
# Evaluate SNOMEDCT concept hierarchies
#############################################################################################################

library(RNeo4j)
db <- startGraph("http://localhost:7474/db/data/", username="neo4j", password="neo4j01")

# Retrieve root node ID
id <- cypher(db, "match(x) where x.FSN contains 'SNOMED CT Concept' and x.active='1'
                  return   labels(x) as label, x.sctid as sctid, x.FSN as FSN limit 1")[1,"sctid"]

# Retrieve all nodes with a direct relationship (one level) to the specified node
q <- paste(" match(x:ObjectConcept)-[:ISA]->(y:ObjectConcept)",
           " where y.sctid='", id, "' and x.active='1' and y.active='1'",
           " return labels(x) as label, x.sctid as sctid, x.FSN as FSN",
           " order by x.FSN", sep="")
x <- cypher(db, q)
