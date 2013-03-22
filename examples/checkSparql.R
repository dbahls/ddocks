library(SPARQL) 


endpoint="http://localhost:8090/openrdf-sesame/repositories/koenkerzeileis_v1"

#q = "
#PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n
#PREFIX demoArchive:<http://demoarchive.demo/>\n
#select * where {\n
#	demoArchive:019a08e0f8bc68c50eb5c1bb225d89f895553e1c7bedc1ca6b1ad30eb3a6c94c36c7e287980d63093dbfcec19bbb2280cf231da347c6115d795afcb17b1befb3 rdf:value ?val763 .\n
#}\n
#"

q<- readLines("query2.txt")

q<- paste(q, collapse="\n", sep="")

print(length(q))


cat(q)
print(length(q))



res <- SPARQL(url=endpoint, q)$results

print(res)
