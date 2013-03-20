#
# test case - reconstruct demo data set 
#


rk <- readLines("samples/koenkerzeileis09/rk.raw")
n <- length(rk)/2

## process information on model dimension
mdim <- strsplit(rk[(1:n) * 2], "\t")
neq <- sapply(mdim, length)
mdim <- sapply(strsplit(unlist(mdim), ","), rbind)

## process information on publication
info <- rep(rk[(1:n) * 2 - 1], neq)
info <- sapply(strsplit(info, "\t"), rbind)

## combine and turn to data.frame
rk <- data.frame(id = rep(1:n, neq), neq = rep(neq, neq),
		nreg = as.numeric(mdim[1,]), nobs = as.numeric(mdim[2,]),
		author = info[1,], journal = info[2,], year = as.numeric(info[3,]),
		page = as.numeric(gsub("s", "", info[4,])),  subject = factor(info[5,],
				levels = c("d", "h", "g", "u", "w", "b", "m", "c")))
levels(rk$subject) <- c("discrimination", "human capital",
		"general", "unionism", "women")[c(1:5, 1, 3, 3)]
rk$collection <- factor(rk$journal %in% c("ir", "jpe", "jhr", "ilrr",
				"aer", "ej", "restat"), levels = c(TRUE, FALSE), labels = c("no", "yes"))

## write as CSV
write.table(rk, file = "rk.csv", sep = ",", quote = FALSE)














#
# read data template
#


# parse header
# find namespaces and repository information



#
# pull values from repositories and reconstruct data set
#



#
# test whether reconstructed dataset equals original one
#


