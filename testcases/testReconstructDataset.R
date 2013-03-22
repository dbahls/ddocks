#
# test case - reconstruct demo data set 
#
library(ddocks)


#dsname  = "data.dj"
dsname  = "rk.raw"


print("reconstruct data set by template")
ds = ddocks_get(file(sprintf("samples/koenkerzeileis09/generated/%s.ddocks", dsname), open = "r"))

fileConn<-file("tmp-data.txt")
writeLines(ds, fileConn)
close(fileConn)




print("read original data set")
fileName <- sprintf("samples/koenkerzeileis09/%s", dsname)
orig = readChar(fileName, file.info(fileName)$size)


# matches?
# false, because of missing new line char in last line... 
print(sprintf("both files match? %s", orig==ds))

cat(ds)
#cat(orig)

print("")