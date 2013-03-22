debug: {
    if (!is.null(extra)) {
        extrastr <- paste("&", sapply(seq(1, length(extra)), 
            function(i) {
                paste(names(extra)[i], "=", URLencode(extra[[i]]), 
                  sep = "")
            }), collapse = "&", sep = "")
    }
    else {
        extrastr <- ""
    }
    tf <- tempfile()
    if (query != "") {
        if (param == "") {
            param <- "query"
        }
        if (format == "xml") {
            tf <- do.call(getURL, append(list(url = paste(url, 
                "?", param, "=", gsub("\\+", "%2B", URLencode(query, 
                  reserved = TRUE)), extrastr, sep = ""), httpheader = c(Accept = "application/sparql-results+xml")), 
                curl_args))
            DOM <- do.call(xmlParse, append(list(tf), parser_args))
            if (length(getNodeSet(DOM, "//s:result[1]", namespaces = sparqlns)) == 
                0) {
                rm(DOM)
                df <- data.frame(c())
            }
            else {
                attrs <- unlist(xpathApply(DOM, paste("//s:head/s:variable", 
                  sep = ""), namespaces = sparqlns, quote(xmlGetAttr(<pointer: 0x0000000000326490>, 
                  "name"))))
                ns2 <- noBrackets(ns)
                res <- get_attr(attrs, DOM, ns2)
                if (dim(res)[[1]] == 1 & dim(res)[[2]] > 1) {
                  df <- data.frame(unlist(res))
                  names(df) <- attrs[1]
                }
                else {
                  df <- data.frame(res)
                }
                rm(res)
                rm(DOM)
                n = names(df)
                for (r in 1:length(n)) {
                  name <- n[r]
                  df[name] <- as.vector(unlist(df[name]))
                }
            }
        }
        else if (format == "csv") {
            tf <- do.call(getURL, append(list(url = paste(url, 
                "?", param, "=", gsub("\\+", "%2B", URLencode(query, 
                  reserved = TRUE)), extrastr, sep = "")), curl_args))
            df <- do.call(readCSVstring, append(list(tf, blank.lines.skip = TRUE, 
                strip.white = TRUE), parser_args))
            if (!is.null(ns)) 
                df <- dropNS(df, ns)
        }
        else if (format == "tsv") {
            tf <- do.call(getURL, append(list(url = paste(url, 
                "?", param, "=", gsub("\\+", "%2B", URLencode(query, 
                  reserved = TRUE)), extrastr, sep = "")), curl_args))
            df <- do.call(readTSVstring, append(list(tf, blank.lines.skip = TRUE, 
                strip.white = TRUE), parser_args))
            if (!is.null(ns)) 
                df <- dropNS(df, ns)
        }
        else {
            cat("unknown format \"", format, "\"\n\n", sep = "")
            return(list(results = NULL, namespaces = ns))
        }
        list(results = df, namespaces = ns)
    }
    else if (update != "") {
        if (param == "") {
            param <- "update"
        }
        extra[[param]] <- update
        do.call(postForm, append(list(url, .params = extra), 
            curl_args))
    }
}
