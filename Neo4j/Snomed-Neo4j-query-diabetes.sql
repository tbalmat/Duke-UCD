// Drop all nodes and edges
match(x)
detach delete x

// Type II diabetes disorders
match(x:condition)
where toLower(x.term) contains 'type 2' or toLower(x.term) contains 'type ii'
return x
order by x.term

// Type II diabetes disorders with family history
match(x:condition)
where toLower(x.term) contains 'family' and (toLower(x.term) contains 'type 2' or toLower(x.term) contains 'type ii')
return x
order by v.term

// Gestational diabetes
match(x:condition)
where toLower(x.term) contains 'gestational diabetes' and not toLower(x.term) contains 'pregestational diabetes'
return x
order by x.term

// Gestational diabetes with associated nodes
match(x:condition)-[:associated]->(r:condition)
where toLower(x.term) contains 'gestational diabetes' and not toLower(x.term) contains 'pregestational diabetes'
return x, r
order by x.term

// Conditions with at least four successive associations
match(x:condition)-[:associated]->(r1:condition)
where toLower(x.term) contains 'gestational diabetes' and not toLower(x.term) contains 'pregestational diabetes'
with x, r1
match(r1:condition)-[:associated]->(r2:condition)
with x, r1, r2
match(r2:condition)-[:associated]->(r3:condition)
with x, r1, r2, r3
match(r3:condition)-[:associated]->(r4:condition)
//return x, r1, r2, r3, r4
return x, r4

// Conditions with a minimum number of outgoing associations
match(x:condition)-[:associated]->()
where toLower(x.term) contains 'gestational diabetes' and not toLower(x.term) contains 'pregestational diabetes'
with x, count(1) as n
match(x:condition)-[:associated]->(r1:condition)
where n>4
return x, n, r1

// Conditions with a minimum number of incoming associations
match()-[:associated]->(x:condition)
where toLower(x.term) contains 'gestational diabetes' and not toLower(x.term) contains 'pregestational diabetes'
return x

// Conditions with a minimum number of incoming associations
match()-[:associated]->(x:condition)
with x, count(1) as n
match(r1:condition)-[:associated]->(x:condition)
where n>10
return r1, n, x
