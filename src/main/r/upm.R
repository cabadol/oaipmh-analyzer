###################################
## UPM Data Provider:
###################################
## [1] sourceASuri       publisherAStext   rightsASuri      
## [4] descriptionAStext descriptionASdate identifierASdate 
## [7] coverageASuri     publisherASdate   formatASdate     
##[10] identifierAStext  identifierASuri   languageASdate   
##[13] subjectAStext     rightsASdate      languageASuri    
##[16] languageAStext    subjectASdate     formatASuri      
##[19] contributorASuri  contributorASdate creatorASuri     
##[22] rightsAStext      contributorAStext subjectASuri     
##[25] relationASuri     creatorASdate     dateAStext       
##[28] coverageASdate    sourceAStext      dateASdate       
##[31] publisherASuri    published         sourceASdate     
##[34] relationAStext    coverageAStext    typeAStext       
##[37] titleASuri        formatAStext      creatorAStext    
##[40] dateASuri         titleASdate       descriptionASuri 
##[43] titleAStext       typeASdate        relationASdate   
##[46] typeASuri        
###################################


# Read directory
directory <- "/Users/cbadenes/Projects/oaipmh-analyzer/target/oaipmh-analyzer-1.0.0-SNAPSHOT/providers_900"
filenames <- list.files(directory, pattern = "*.*", full.names = TRUE)

# Initialize vectors of dataframe
resume <- NULL
servers <- NULL
totalPublications <- NULL
oldestPublications <- NULL
newestPublications <- NULL

# Initialize list of down servers
DOWN_SERVERS <- NULL

# Loop over files
n <- length(filenames)
print("Iteration:")
for (i in 1:n){
  
  cat(sprintf("\"%s\", ", i))
  
  # Read file path
  fileName <- filenames[i]
  #fileName <- "/Users/cbadenes/Projects/oaipmh-analyzer/target/oaipmh-analyzer-1.0.0-SNAPSHOT/providers/cdfmemlrucv_oai_alejandria_biz.csv"
  
  # if empty continue
  if (file.info(fileName)$size == 0){
    DOWN_SERVERS <- c(DOWN_SERVERS,basename(fileName))
    next
  }
    
  
  
  
  
  total <- 0
  oldest <- NA
  newest <- NA
  tryCatch({
    
    # Load file
    fileData <- read.csv(file=fileName,head=TRUE,sep=",")
    
    # Sort columns
    publications <- fileData[,order(names(fileData))]
    
    # Statistics for each type of data
    #summary(publications)
    
    # Number of publications
    total <- nrow(publications)
    
    # Convert publications to date
    publishDates <- publications[,"published"]
    publishDates <- publishDates[publishDates != "0"]
    publishDates <- publishDates[publishDates != "1"]
    publishDates <- publishDates[publishDates != ""]
    publishDates <- publishDates[publishDates != " "]
    published <- as.POSIXct(publishDates, format= "%Y-%m-%dT%H:%M:%OSZ")
    
    # min/max value of publications
    oldest <- as.numeric(format(min(published, na.rm=TRUE),"%Y"))
    newest <- as.numeric(format(max(published, na.rm=TRUE),"%Y"))
    
    
  },warning = function(w) {
    warning("warning message")
  }, error = function(e) {
    warning("error message")
  }, finally = {
   
    # create row
    servers <- c(servers,basename(fileName))
    totalPublications <- c(totalPublications,total)
    oldestPublications <- c(oldestPublications,oldest)
    newestPublications <- c(newestPublications, newest)
    #row = c(basename(fileName),total,oldest,newest)
    
    # insert in dataframe
    #rbind(resume,row)->resume
  })
  
  # number of publications by year
  #in2011 <- length(grep("2011",publications$published))
  
  # Boxplot of Contributors
  #ct1 <- publications[publications$contributorAStext,]
  #boxplot(ct1,xlab="Contributor",ylab="Publications")  
  
}

# Create data frame
resume <- data.frame(server=servers, total=totalPublications, oldest=oldestPublications, newest=newestPublications)

# sort by total
resumeSorted <- resume[ order(resume[,2]), ]

# Simple Bar Plot
barplot(resumeSorted[,"total"], main="Publication Distribution", xlab="Number of Publications")

# Simple Pie Chart
pie(resumeSorted[,"total"], labels = resumeSorted[,"total"], main="Pie Chart of Publications")

# Some statistics
summary(resume[,c("total","oldest","newest")])

# Server statistics
print("Servers:")
NUM_SERVERS=length(filenames)
NUM_UP_SERVERS=nrow(resume)
RATE_UP_SERVERS=ceiling((NUM_UP_SERVERS*100)/NUM_SERVERS)
NUM_DOWN_SERVERS=length(filenames) -nrow(resume)
RATE_DOWN_SERVERS=floor((NUM_DOWN_SERVERS*100)/NUM_SERVERS )

cat(sprintf("   - Analyzed: %s", NUM_SERVERS))
cat(sprintf("   - Up: %s (%s)", NUM_UP_SERVERS, RATE_UP_SERVERS))
cat(sprintf("   - Down: %s (%s)", NUM_DOWN_SERVERS, RATE_DOWN_SERVERS))

# Publication statistics
print("Publications:")
NUM_PUBLICATIONS=sum(resume[,"total"])
NUM_PUBLICATIONS_BY_SERVER=NUM_PUBLICATIONS/NUM_UP_SERVERS

cat(sprintf("   - Total: %s", NUM_PUBLICATIONS))
cat(sprintf("   - Average by server: %s", ceiling(NUM_PUBLICATIONS_BY_SERVER)))

# Estimations
print("Estimations:")
EST_NUM_SERVERS=2592
EST_NUM_UP_SERVERS=(RATE_UP_SERVERS/100)*EST_NUM_SERVERS
EST_NUM_DOWN_SERVERS=(RATE_DOWN_SERVERS/100)*EST_NUM_SERVERS
EST_PUBLICATIONS=NUM_PUBLICATIONS_BY_SERVER*EST_NUM_UP_SERVERS
EST_PUBLICATION_SIZE=10
EST_SIZE=EST_PUBLICATIONS*EST_PUBLICATION_SIZE

cat(sprintf("   - Servers: %s", EST_NUM_SERVERS))
cat(sprintf("   - Up Servers: %s", EST_NUM_UP_SERVERS))
cat(sprintf("   - Down Servers: %s", EST_NUM_DOWN_SERVERS))
cat(sprintf("   - Publications: %s", EST_PUBLICATIONS))
cat(sprintf("   - Size: %sMB", EST_SIZE))

  
# Write CSV in R
write.csv(resume[,c("server","total","oldest","newest")], file = paste(directory,"/Resume.csv",sep=""), na="", fileEncoding = "UTF-8")
