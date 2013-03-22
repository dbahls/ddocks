
library(SPARQL)
library(stringr)




#' Reconstruct data set by template
#'
#' @param con Connection (file, pipe, url,...) to a ddocks data template
#' @return character reconstructed data set
#'
#' @examples
#' ddocks_get(file("samples/koenkerzeileis09/generated/data.dj.ddocks", open = "r"))
#'
#' @author daniel
#' @export
ddocks_get <- function(con){
	
	
	
	#
	# declare variables / set defaults
	#
	tokenOpen  = "<"
	tokenClose = ">"
	nsmap <- new.env(hash=T, parent=emptyenv())
	
	idsbyns <- new.env(hash=T, parent=emptyenv()) 		# maps from namespace to a list of all data reference IDs of that namespace
	
	id2values <- new.env(hash=T, parent=emptyenv())
	
	tmpltBody  = list()		# the template without header  (a list of lines)
	datasetStr = list()		# final data set string        (a list of lines)
	
	
	
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
	regex  = sprintf("%s.*?%s", tokenClose, tokenOpen)
	regex2 = sprintf("%s([[:graph:]]+)%s", tokenOpen, tokenClose)
	repeat {
		
#		print(sprintf("processing line: %s", line))
		
		# store line in memory
		tmpltBody = c(tmpltBody, line)
		
		#print(paste("new line: ", line))
		res <- str_split(line, regex)
		#print(length(res[[1]]))
		if (length(res[[1]])>1 || (length(greptmp <- grep(regex2, line, value=TRUE))>0 && greptmp[[1]]==line)) {
			
			# weird parsing.... but works 
			for (id in res[[1]]) {
				id = sub(sprintf(".*%s", tokenOpen), "", id)
				id = sub(sprintf("%s.*", tokenClose), "", id)
				
				# todo: what if the full URI is given, not using namespace? nasty workaround required?
				# also not fail-safe yet (array index)
				ns = str_match(id, "([[:graph:]]+)\\:")[2]
				
#				print(sprintf("found id [%s] for ns [%s]", id, ns))
				
				idsbyns[[ns]] = c(idsbyns[[ns]], id)
			}
		}	
		
		
		#res <- grep(regex, line, value=TRUE)
		#res <- str_match(line, regex)
		
		#print("match")
		#print(res)
		
		
		if (length(line <- readLines(con, n = 1, warn = FALSE)) <= 0) break
		
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
			
			#cat(q)
			#fileConn<-file("query.txt")
			#writeLines(q, fileConn)
			#close(fileConn)
			
			# run this query against the sparql endpoint for the given namespace
			d <- SPARQL(url=endpoint, q, extra=list(resultFormat="xml"), ns=c("demoArchive", "http://demoarchive.demo/", "rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
			res <- d$results
			#print(res)
			
			# bind results in id2values map
			for (vname in colnames(res)) {
				
				i= as.integer(str_match(vname, "\\d+"))
				id=tmpIDs[[i]]
				
				val = res[vname]
				#print(sprintf("%s <- %s", vname, val))
				
				id2values[[id]] = val
				
			}
			
		}
		
	}
	
	
	#
	# eventually create data set with the actual values
	#
	body = paste(tmpltBody, collapse="\n", sep="")
	#print(tmpltBody)
	
#	print(length(ls(id2values)))
	for (id in ls(id2values)) {
		
#		print(sprintf("replace id <%s> with value [%s]", id, id2values[[id]]))
		
		
		body = gsub(sprintf("<%s>", id), id2values[[id]], body, fixed=TRUE)
		
	}
	#print(body)
		
	#cat(body)
	
	
#	fileConn<-file("reconstructed.txt")
#	writeLines(body, fileConn)
#	close(fileConn)
	
	
	close(con)
	

	
	
	return(body)
	
	
	
}


