// Query subnet of a single species, path to endpoint (Snomed terminology top node)
match(x), (y:concept) where x.term="Silvered leaf monkey"
with x, y
match p=(x)-[:rel*]->(y)
return *