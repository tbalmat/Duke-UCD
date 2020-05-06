// Modify Neo4jRoot\conf\neo4j.conf dbms.active_database to reference Neo4jRoot\data\databases\Snomed

// Execute this script with
// call apoc.cypher.runFile("C:/Projects/Duke/SemanticsOfRareDisease/UnifiedMedicalLanguageSystem/Neo4j/Snomed-Neo4j-createDB-All.sql")

// Drop all objects
match(x) detach delete x;

// Inspect vertices
//load csv with headers from 'file:///C:/Projects/Duke/SemanticsOfRareDisease/UnifiedMedicalLanguageSystem/Neo4j/Snomed-Neo4j-All-vertex.csv' as v
//return v

// Load vertices (fully specified names)
load csv with headers from 'file:///C:/Projects/Duke/SemanticsOfRareDisease/UnifiedMedicalLanguageSystem/Neo4j/Snomed-Neo4j-All-vertex.csv' as v
with v where v.type='Fully specified name'
create (:fullName {vid:v.conceptID, term:v.term, type:v.type});
//create index on :concept(vid);
create constraint on (x:concept) assert x.vid is unique;

// Load vertices (synonyms)
load csv with headers from 'file:///C:/Projects/Duke/SemanticsOfRareDisease/UnifiedMedicalLanguageSystem/Neo4j/Snomed-Neo4j-All-vertex.csv' as v
with v where v.type starts with 'Synonym'
create (:synonym {vid:v.conceptID, term:v.term, type:v.type});
//create index on :concept(vid);
create constraint on (x:concept) assert x.vid is unique;

// Load edges
//using periodic commit
load csv with headers from 'file:///C:/Projects/Duke/SemanticsOfRareDisease/UnifiedMedicalLanguageSystem/Neo4j/Snomed-Neo4j-All-edge.csv' as ed
match (v1), (v2) where v1.vid=ed.v1 and v2.vid=ed.v2
merge (v1)-[:rel {v1:ed.v1, v2:ed.v2, type:ed.type}]-(v2)