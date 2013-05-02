library(RCurl)
library(stringr)
library(SPARQL)



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
ddocks_restore <- function(con, targetfile){
	
	if (file.exists(targetfile)) return()
	
	# read data template from connection
    tmplt <- paste0(readLines(con), collapse = "\n")
    close(con)
	
	#
	# declare variables / set defaults
	#
    tokenOpen <- "<"
    tokenClose = ">"
    nsmap <- new.env(hash = T, parent = emptyenv())
    idsbyns <- new.env(hash = T, parent = emptyenv())		# maps from namespace to a list of all data reference IDs of that namespace
    id2values <- new.env(hash = T, parent = emptyenv())
	
	
	# split template into header and body
    tmp <- strsplit(tmplt, "(?<=\\Q</ddocks-header>\\E)\\s*\\n", perl = TRUE)
    header <- tmp[[1]][1]
    body <- tmp[[1]][2]
	
	# remove comments from header
    header <- gsub("#.*\\n", "", header, perl = TRUE)
    
	# declarations for open and close tokens
	res <- str_match(header, "@tokens[[:blank:]]([[:graph:]]+)[[:blank:]]([[:graph:]]+)")
    if (!is.na(res[, 1])) {
        tokenOpen <- res[, 2]
        tokenClose <- res[, 3]
        #print(paste("tokens: ", tokenOpen, tokenClose, sep = " ", collapse = NULL))
    }
	
	# namespace declarations
    res <- str_match(header, "@namespace[[:blank:]]([[:graph:]]+)[[:blank:]]([[:graph:]]+)[[:blank:]]([[:graph:]]+)")
    if (!is.na(res[, 1])) {
        ns <- res[, 2]
        uriprefix <- res[, 3]
        urlendpoint <- res[, 4]
        nsmap[[ns]] <- res[, 3:4]
        #print(paste("namespace: ", ns, paste(nsmap[[ns]], collapse = "    "), sep = " ", collapse = NULL))
    }
	
	# use regular expression to extract all IDs from the body
    regex <- sprintf("\\Q%s\\E([[:graph:]]+)\\Q%s\\E", tokenOpen, tokenClose)
    matches <- str_extract_all(body, regex)
    for (match in matches[[1]]) {
	
        id <- str_match(match, regex)[[2]]
        
		# todo: what about full URIs, without namespace prefix?
		ns <- str_match(id, "([[:graph:]]+)\\:")[2]
		
		# organize IDs by namespace
        idsbyns[[ns]] <- c(idsbyns[[ns]], id)
    }
	
	
	#
	# collect all data references in the template
	# organize them by repository, so we can generate one query per repository to request the values
	#
    for (ns in ls(idsbyns)) {
 
		#print(sprintf("processing ids for ns: %s", ns))
        
		# current endpoint is
		endpoint <- nsmap[[ns]][2]
        #print(sprintf("Pulling values from endpoint: %s", endpoint))
        
		# process all IDs of the current namespace
		tmpIDs <- idsbyns[[ns]]
		
		# how many IDs to pull in one request? (some triple stores have problems dealing with large queries)
        idsPerCall <- 30
        for (index in seq(1, length(tmpIDs), by = idsPerCall)) {
		
			# init query
            queryBuffer <- list(
				"PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>", 
                sprintf("PREFIX %s:<%s>", ns, nsmap[[ns]][1]), 
                "select * where {"
			)
			
			# query the next <idsPerCall> values
            for (i in index:min(index + idsPerCall - 1, length(tmpIDs))) {
				
				# dynamically add rows for every id to be queried
                queryBuffer <- c(queryBuffer, sprintf("\t%s rdf:value ?val%d .", tmpIDs[i], i))
            }
			
			# close query and convert the list (string buffer) to a string
            queryBuffer <- c(queryBuffer, "}")
            q <- paste(queryBuffer, collapse = "\n", sep = "")

			# run this query against the sparql endpoint for the given namespace
            d <- SPARQL(url = endpoint, q, extra = list(resultFormat = "xml"), ns = c("demoArchive", "http://demoarchive.demo/", "rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
            res <- d$results

			# bind results in id2values map
            for (vname in colnames(res)) {

				i <- as.integer(str_match(vname, "\\d+"))
                id <- tmpIDs[[i]]
                
				val <- res[vname]
                id2values[[id]] <- val
                
				#
				# replace all occurrences of this ID with its value
				#
				body <- gsub(sprintf("%s%s%s", tokenOpen, id, tokenClose), val, body, fixed = TRUE)
            }
        }
    }
	
	# write target file	
	fileConn<-file(targetfile)
	writeLines(body, fileConn)
	close(fileConn)
	
	# return data set as string maybe? better ideas anyone? file maybe?
    return()
}


