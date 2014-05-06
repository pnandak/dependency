library(plyr)
setwd("~/Documents/Spring_2014/Math_Research")
employes <- read.csv('employees.csv')
emp <- arrange(employes, desc(Department))
emp$Department[156] <- "ENA East Power"
departmentNames <- unique(emp$Department)
countPerDept <- apply(emp, 2, count)$Department
colors <- heat.colors(length(departmentNames))
emp <- mutate(emp, color = colors[match(Department, departmentNames)], Name = as.character(Name))
nameCol <- cbind(emp$Name, emp$color)
x <- unlist(nameCol)
names <- x[1:(length(x)/2)]
colrs <- x[((length(x)/2)+1):length(x)]
s <- paste('"', names, '" : "', colrs,'"', sep="")
JSON <- paste('{', paste(s, collapse=","), '}', sep="")
#whoops, actually need to make it csv:
colnames(nameCol) <- c("name", "color")
write.table(nameCol, file="people.csv", sep=",", quote=FALSE, row.names = FALSE, col.names = TRUE)

filelist <- file.info(list.files("combmats"), include.dirs=TRUE)
files <- rownames(filelist)
files <- files[41:78]
setwd("~/Documents/Spring_2014/Math_Research/combmats")
matrix <- read.table(files[1], quote="\"")
for (i in 2:length(files)) {
  file <- read.table(files[i], quote="\"")
  matrix <- matrix + file
}
colnames(matrix) <- employes$Name
rownames(matrix) <- employes$Name
setwd("~/Documents/Spring_2014/Math_Research")
emp <- read.csv('employees.csv'); nameDept <- emp[, c(3,6)];

departmentSubset <- function(nameDept, departmentName) {
  return (subset(nameDept, Department == departmentName)$Name)
}

employeeToDept <- function(employee) {
  # in: employee name
  # out: department of employee
  return (subset(nameDept, Name == employee)$Department)
}

interdeptZeros <- function(CM, departmentSubset) {
  # in: vector whose entries are employees in the same department
  # out: CM, where [i,j] is set to zero if i and j are in the subset
  for (i in departmentSubset) {
    for (j in departmentSubset) {
      CM[i,j] <- 0
      CM[j,i] <- 0
    }
  }
  return (CM)
}
scaleDown <- function(CM, threshold) {
  for (i in 1:nrow(CM)) {
    for (j in 1:ncol(CM)) {
      if (CM[i,j] < threshold) {
        CM[i,j] <- 0
      }
    }
  }
  return (CM)
}
grandDaddy <- function(CM) {
  # makes zero CM[i,j] for all i,j pairs in the same department
  for (i in unique(nameDept$Department)) {
    deptSubset <- departmentSubset(nameDept, i)
    CM <- interdeptZeros(CM,deptSubset)
  }
  CM <- scaleDown(CM, 20)
  return (CM)
}
removeZeroVectors <- function(CM) {
  # in: some square matrix
  # out: that square matrix where row and column i are removed
  #       if either row i or column i is all zero
  zeroRows <- rownames(data.frame(which(apply(CM, 1, sum) == 0)))
  zeroCols <- rownames(data.frame(which(apply(CM, 2, sum) == 0)))
  alltogether <- unique(c(zeroRows, zeroCols))
  nums <- which(rownames(CM) %in% alltogether)
  return(CM[-nums, -nums])
}


##matrix with NO inter-departmental connections
matrix <- grandDaddy(matrix)
matrix <- removeZeroVectors(matrix)
m <- apply(matrix, 1, function(x) paste( x, collapse=","))
mat <- paste(paste('[', m, ']', sep=""), collapse=",")
matrixJSON <- paste('[', mat, ']', sep="")

fileConn<-file("matrix.json")
writeLines(matrixJSON, fileConn)  # for the nodes array for the JSON graph
close(fileConn)


a <- evcent(matrix) #dana davis!
b <- a/(max(a) + 0.1) + 0.1
employes <- read.csv('employees.csv')
departmentNames <- unique(emp$Department)
countPerDept <- apply(emp, 2, count)$Department
colors <- heat.colors(length(departmentNames))
emp <- mutate(emp, color = colors[match(Department, departmentNames)], Name = as.character(Name))
nameCol <- cbind(emp$Name, emp$color)
x <- unlist(nameCol)
names <- x[1:(length(x)/2)]
colrs <- x[((length(x)/2)+1):length(x)]
s <- paste('"', names, '" : "', colrs,'"', sep="")
JSON <- paste('{', paste(s, collapse=","), '}', sep="")
#whoops, actually need to make it csv:
colnames(nameCol) <- c("name", "color")

a <- match(rownames(matrix), as.character(nameCol$name))
b <- na.omit(a[1:156])
namesColors <- nameCol[b,]
colnames(namesColors) <- c("name", "color")


c <- evcent(matrix) 
d <- c/(max(c) + 0.4) + 0.1
cbind(namesColors, d) -> namesColors
namesColors <- namesColors[, c(1,3,2)]
colnames(namesColors) = c("name", "population", "color")
View(namesColors) #!!! works 
write.table(namesColors, file="peoples.csv", sep=",", quote=FALSE, row.names = FALSE, col.names = TRUE)


dependences <- function(col, date) {
  dependers <- which(data[col,] > 0)
  dependerNames <- employes$Name[dependers]
  dependerNames <- as.vector(paste('"', dependerNames, '"', sep=""))
  return (paste('{ "name": "',employes$Name[col],'" , "connections" : [', paste(dependerNames, collapse=", "), ']}', sep=""))
}
thisWorks <- as.vector(sapply(1:nrow(matrix), function(x, y) dependences(x, y), y = matrix)) #Look! No for loop!!
jsonOut <- paste(thisWorks, collapse=", ")






