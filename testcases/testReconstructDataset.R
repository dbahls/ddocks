#
# test case - reconstruct demo data set 
#
library(ddocks)



print("reconstruct data set by template")
ds = ddocks_get(file("samples/koenkerzeileis09/generated/data.dj.ddocks", open = "r"))


print("read original data set")
fileName <- "samples/koenkerzeileis09/data.dj"
orig = readChar(fileName, file.info(fileName)$size)


# matches?
# false, because of missing new line char in last line... 
print(sprintf("both files match? %s", orig==ds))

cat(ds)
#cat(orig)

print("")