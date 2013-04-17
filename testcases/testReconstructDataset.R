#
# test case - reconstruct demo data set 
#
library(ddocks)

#
# test for data set 1
#

dsname  = "data.dj"

# use local template file
#path = sprintf("samples/koenkerzeileis09/%s.ddocks", dsname)
#con  = file(path, open = "r")

# use remote template file
uri  = sprintf("http://mawifo.zbw.eu/_static/%s.ddocks", dsname)
con  = url(uri)

# restore data set
ds <- ddocks_restore(con)

# compare original with restored
urlOriginal = "http://www.econ.uiuc.edu/~roger/research/repro/data.dj"
orig <- paste0(readLines(url(urlOriginal)), collapse="\n")

print(sprintf("[%s] original = restored? %s", dsname, (ds==orig)))

#fileConn<-file("tmp-data.txt")
#writeLines(ds, fileConn)
#close(fileConn)


#
# test for data set 2
#

dsname  = "rk.raw"

# use local template file
#path = sprintf("samples/koenkerzeileis09/%s.ddocks", dsname)
#con  = file(path, open = "r")

# use remote template file
uri  = sprintf("http://mawifo.zbw.eu/_static/%s.ddocks", dsname)
con  = url(uri)

# restore data set
ds <- ddocks_restore(con)

# compare original with restored
urlOriginal = "http://www.econ.uiuc.edu/~roger/research/repro/rk.raw"
orig <- paste0(readLines(url(urlOriginal)), collapse="\n")

print(sprintf("[%s] original = restored? %s", dsname, (ds==orig)))





#fileConn<-file("tmp-data.txt")
#writeLines(ds, fileConn)
#close(fileConn)






