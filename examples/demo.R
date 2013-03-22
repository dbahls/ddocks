#
# try stuff here 
#

library(SPARQL)
library(stringr)
#require(ddocks)


#
# set input parameters for the data example
#
tmpltFile  <- "C:\\data\\workspace-juno\\ddocks\\samples\\koenkerzeileis09\\generated\\data.dj.ddocks"


#
# declare variables / set defaults
#
tokenOpen  = "<"
tokenClose = ">"
nsmap <- new.env(hash=T, parent=emptyenv())


# maps from namespace to a list of all data reference IDs of that namespace 
idsbyns <- new.env(hash=T, parent=emptyenv())


id2values <- new.env(hash=T, parent=emptyenv())




print(paste("opening template file: ", tmpltFile))


con  <- file(tmpltFile, open = "r")

#dataList <- list()
#ecdfList <- list()





#
# parse template header
#
print("parsing header...")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {

	# check first line character, break when finished header processing 
	isHeader = length(grep("#", line, fixed=TRUE)>0) > 0;
	if (!isHeader) break
	

	# namespace declarations
	res <- str_match(line, "@namespace[[:blank:]]([[:graph:]]+)[[:blank:]]([[:graph:]]+)[[:blank:]]([[:graph:]]+)")
	if(!is.na(res[,1])) {
		
		# an ns declaration as array
		#print(res[,2:4])
		
		ns = res[,2]
		uriprefix = res[,3]
		urlendpoint = res[,4]
			
		# maps from ns prefix to a 2-dim array of uri and sparql endpoint
		nsmap[[ns]] <- res[,3:4]
		print(paste("namespace: ", ns, paste(nsmap[[ns]], collapse='    '),sep = " ", collapse = NULL))
		
	}
	
	res <- str_match(line, "@tokens[[:blank:]]([[:graph:]]+)[[:blank:]]([[:graph:]]+)")
	if(!is.na(res[,1])) {
		
		# token declarations
		# for data reference detection
		
		tokenOpen  = res[,2]
		tokenClose = res[,3]
		
		# maps from ns prefix to a 2-dim array of uri and sparql endpoint
		print(paste("tokens: ", tokenOpen, tokenClose,sep = " ", collapse = NULL))
		
	}
} 


#
# processing template body
#
print("processing body...")

#regex = sprintf("(?U)%s([[:graph:]]+)%s", tokenOpen, tokenClose)
##regex = "http"
#print(sprintf("using regex: %s", regex))
#
#while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
#
#	#print(paste("new line: ", line))
#	res <- grep(regex, line, value=TRUE)
#	#res <- str_match(line, regex)
#	
#	print("match")
#	print(res)
#}



#
# collect all data references in the template
# organize them by repository, so we can generate one query per repository to request the values
#
regex = sprintf("%s.*?%s", tokenClose, tokenOpen)
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
	
	#print(paste("new line: ", line))
	res <- str_split(line, regex)

	# weird parsing.... but works 
	for (id in res[[1]]) {
		id = sub(sprintf(".*%s", tokenOpen), "", id)
		id = sub(sprintf("%s.*", tokenClose), "", id)
		
		# todo: what if the full URI is given, not using namespace? nasty workaround required?
		# also not fail-safe yet (array index)
		ns = str_match(id, "([[:graph:]]+)\\:")[2]

		idsbyns[[ns]] = c(idsbyns[[ns]], id)
	}
	
	
	#res <- grep(regex, line, value=TRUE)
	#res <- str_match(line, regex)
	
	#print("match")
	#print(res)
}


#
# iterate over ns / repositories and request values for the given ids
#
for (ns in ls(idsbyns)) {
	print(sprintf("processing ids for ns: %s", ns))

	endpoint = nsmap[[ns]][2]
	print(sprintf("Pulling values from endpoint: %s", endpoint))
	
	# construct sparql query header
	# todo: handle when ns unknown in nsmap, e.g. not declared in header
	
	tmpIDs = idsbyns[[ns]]
	
	# how many values should be queried in one go
	idsPerCall = 30
	
	
	for (index in seq(1, length(tmpIDs), by=idsPerCall)) {
#	for (index in seq(1321, length(tmpIDs), by=idsPerCall)) {
		
		# init query
		queryBuffer = list(
				"PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
				sprintf("PREFIX %s:<%s>", ns, nsmap[[ns]][1]),
				"select * where {"
		)
		
		# query the next <idsPerCall> values
		for (i in index:min(index+idsPerCall-1, length(tmpIDs))) {
	
			# dynamically add rows for every id to be queried
			queryBuffer = c(queryBuffer, sprintf("\t%s rdf:value ?val%d .", tmpIDs[i], i))
		}

		# close query and convert the list (string buffer) to a string
		queryBuffer = c(queryBuffer, "}")
		q = paste(queryBuffer, collapse="\n", sep="")
		
		cat(q)
		fileConn<-file("query.txt")
		writeLines(q, fileConn)
		close(fileConn)
		
		# run this query against the sparql endpoint for the given namespace
		d <- SPARQL(url=endpoint, q, extra=list(resultFormat="xml"), ns=c("demoArchive", "http://demoarchive.demo/", "rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
		res <- d$results

		print(res)
		
		
		#print(typeof(res))
		#print(length(res))
		
	}
	
}







close(con)





#print('the list')
#print(nl)










