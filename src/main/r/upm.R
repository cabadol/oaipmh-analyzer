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
library(sqldf)

# Read directory
directory <- "/Users/cbadenes/Temp/oaipmh-analyzer-1.0.0-SNAPSHOT/providers_test1"
fn <- paste(directory,"/","_data.csv",sep="")
if (file.exists(fn)) file.remove(fn)
filenames <- list.files(directory, pattern = "*.*", full.names = TRUE)

# Initialize vectors of dataframe
resume <- NULL
servers <- NULL
totalPublications <- NULL
oldestPublications <- NULL
newestPublications <- NULL
sourcesAsUri <- NULL
sourcesAsTxt <- NULL
sourcesAsDate <- NULL
publishersAsUri <- NULL
publishersAsTxt <- NULL
publishersAsDate <- NULL
rightsAsUri <- NULL
rightsAsTxt <- NULL
rightsAsDate <- NULL
descriptionsAsUri <- NULL
descriptionsAsTxt <- NULL
descriptionsAsDate <- NULL
identifiersAsUri <- NULL
identifiersAsTxt <- NULL
identifiersAsDate <- NULL
coveragesAsUri <- NULL
coveragesAsTxt <- NULL
coveragesAsDate <- NULL
formatsAsUri <- NULL
formatsAsTxt <- NULL
formatsAsDate <- NULL
languagesAsUri <- NULL
languagesAsTxt <- NULL
languagesAsDate <- NULL
subjectsAsUri <- NULL
subjectsAsTxt <- NULL
subjectsAsDate <- NULL
contributorsAsUri <- NULL
contributorsAsTxt <- NULL
contributorsAsDate <- NULL
creatorsAsUri <- NULL
creatorsAsTxt <- NULL
creatorsAsDate <- NULL
relationsAsUri <- NULL
relationsAsTxt <- NULL
relationsAsDate <- NULL
datesAsUri <- NULL
datesAsTxt <- NULL
datesAsDate <- NULL
titlesAsUri <- NULL
titlesAsTxt <- NULL
titlesAsDate <- NULL
descriptionsAsUri <- NULL
descriptionsAsTxt <- NULL
descriptionsAsDate <- NULL
typesAsUri <- NULL
typesAsTxt <- NULL
typesAsDate <- NULL
sourcesNever <- NULL
sourcesOne <- NULL
sourcesMore <- NULL
publishersNever <- NULL
publishersOne <- NULL
publishersMore <- NULL
rightsNever <- NULL
rightsOne <- NULL
rightsMore <- NULL
descriptionsNever <- NULL
descriptionsOne <- NULL
descriptionsMore <- NULL
identifiersNever <- NULL
identifiersOne <- NULL
identifiersMore <- NULL
coveragesNever <- NULL
coveragesOne <- NULL
coveragesMore <- NULL
formatsNever <- NULL
formatsOne <- NULL
formatsMore <- NULL
languagesNever <- NULL
languagesOne <- NULL
languagesMore <- NULL
subjectsNever <- NULL
subjectsOne <- NULL
subjectsMore <- NULL
contributorsNever <- NULL
contributorsOne <- NULL
contributorsMore <- NULL
creatorsNever <- NULL
creatorsOne <- NULL
creatorsMore <- NULL
relationsNever <- NULL
relationsOne <- NULL
relationsMore <- NULL
datesNever <- NULL
datesOne <- NULL
datesMore <- NULL
titlesNever <- NULL
titlesOne <- NULL
titlesMore <- NULL
descriptionsNever <- NULL
descriptionsOne <- NULL
descriptionsMore <- NULL
typesNever <- NULL
typesOne <- NULL
typesMore <- NULL

# Initialize list of down servers
DOWN_SERVERS <- NULL

# Loop over files
n <- length(filenames)
print("Iteration:")
for (i in 1:n){
  
  cat(sprintf("\"%s\", ", i))
  
  # Read file path
  fileName <- filenames[i]
  
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
    
    # fields
    fieldsCounter <- colSums(Filter(is.numeric, publications),na.rm = TRUE)
    
    # term times column
    sourcesNever <- c(sourcesNever,sqldf("select count(*) from publications where sourceASuri == 0 and sourceAStext == 0 and sourceASdate == 0") * 100 / total)
    sourcesOne <- c(sourcesOne,sqldf("select count(*) from publications where sourceASuri == 1 or sourceAStext == 1 or sourceASdate == 1") * 100 / total)
    sourcesMore <- c(sourcesMore,sqldf("select count(*) from publications where sourceASuri > 1 or sourceAStext > 1 or sourceASdate > 1") * 100 / total)
    publishersNever <- c(publishersNever,sqldf("select count(*) from publications where publisherASuri == 0 and publisherAStext == 0 and publisherASdate == 0") * 100 / total)
    publishersOne <- c(publishersOne,sqldf("select count(*) from publications where publisherASuri == 1 or publisherAStext == 1 or publisherASdate == 1") * 100 / total)
    publishersMore <- c(publishersMore,sqldf("select count(*) from publications where publisherASuri > 1 or publisherAStext > 1 or publisherASdate > 1") * 100 / total)
    rightsNever <- c(rightsNever,sqldf("select count(*) from publications where rightsASuri == 0 and rightsAStext == 0 and rightsASdate == 0") * 100 / total)
    rightsOne <- c(rightsOne,sqldf("select count(*) from publications where rightsASuri == 1 or rightsAStext == 1 or rightsASdate == 1") * 100 / total)
    rightsMore <- c(rightsMore,sqldf("select count(*) from publications where rightsASuri > 1 or rightsAStext > 1 or rightsASdate > 1") * 100 / total)
    descriptionsNever <- c(descriptionsNever,sqldf("select count(*) from publications where descriptionASuri == 0 and descriptionAStext == 0 and descriptionASdate == 0") * 100 / total)
    descriptionsOne <- c(descriptionsOne,sqldf("select count(*) from publications where descriptionASuri == 1 or descriptionAStext == 1 or descriptionASdate == 1") * 100 / total)
    descriptionsMore <- c(descriptionsMore,sqldf("select count(*) from publications where descriptionASuri > 1 or descriptionAStext > 1 or descriptionASdate > 1") * 100 / total)
    identifiersNever <- c(identifiersNever,sqldf("select count(*) from publications where identifierASuri == 0 and identifierAStext == 0 and identifierASdate == 0") * 100 / total)
    identifiersOne <- c(identifiersOne,sqldf("select count(*) from publications where identifierASuri == 1 or identifierAStext == 1 or identifierASdate == 1") * 100 / total)
    identifiersMore <- c(identifiersMore,sqldf("select count(*) from publications where identifierASuri > 1 or identifierAStext > 1 or identifierASdate > 1") * 100 / total)
    coveragesNever <- c(coveragesNever,sqldf("select count(*) from publications where coverageASuri == 0 and coverageAStext == 0 and coverageASdate == 0") * 100 / total)
    coveragesOne <- c(coveragesOne,sqldf("select count(*) from publications where coverageASuri == 1 or coverageAStext == 1 or coverageASdate == 1") * 100 / total)
    coveragesMore <- c(coveragesMore,sqldf("select count(*) from publications where coverageASuri > 1 or coverageAStext > 1 or coverageASdate > 1") * 100 / total)
    formatsNever <- c(formatsNever,sqldf("select count(*) from publications where formatASuri == 0 and formatAStext == 0 and formatASdate == 0") * 100 / total)
    formatsOne <- c(formatsOne,sqldf("select count(*) from publications where formatASuri == 1 or formatAStext == 1 or formatASdate == 1") * 100 / total)
    formatsMore <- c(formatsMore,sqldf("select count(*) from publications where formatASuri > 1 or formatAStext > 1 or formatASdate > 1") * 100 / total)
    languagesNever <- c(languagesNever,sqldf("select count(*) from publications where languageASuri == 0 and languageAStext == 0 and languageASdate == 0") * 100 / total)
    languagesOne <- c(languagesOne,sqldf("select count(*) from publications where languageASuri == 1 or languageAStext == 1 or languageASdate == 1") * 100 / total)
    languagesMore <- c(languagesMore,sqldf("select count(*) from publications where languageASuri > 1 or languageAStext > 1 or languageASdate > 1") * 100 / total)
    subjectsNever <- c(subjectsNever,sqldf("select count(*) from publications where subjectASuri == 0 and subjectAStext == 0 and subjectASdate == 0") * 100 / total)
    subjectsOne <- c(subjectsOne,sqldf("select count(*) from publications where subjectASuri == 1 or subjectAStext == 1 or subjectASdate == 1") * 100 / total)
    subjectsMore <- c(subjectsMore,sqldf("select count(*) from publications where subjectASuri > 1 or subjectAStext > 1 or subjectASdate > 1") * 100 / total)
    contributorsNever <- c(contributorsNever,sqldf("select count(*) from publications where contributorASuri == 0 and contributorAStext == 0 and contributorASdate == 0") * 100 / total)
    contributorsOne <- c(contributorsOne,sqldf("select count(*) from publications where contributorASuri == 1 or contributorAStext == 1 or contributorASdate == 1") * 100 / total)
    contributorsMore <- c(contributorsMore,sqldf("select count(*) from publications where contributorASuri > 1 or contributorAStext > 1 or contributorASdate > 1") * 100 / total)
    creatorsNever <- c(creatorsNever,sqldf("select count(*) from publications where creatorASuri == 0 and creatorAStext == 0 and creatorASdate == 0") * 100 / total)
    creatorsOne <- c(creatorsOne,sqldf("select count(*) from publications where creatorASuri == 1 or creatorAStext == 1 or creatorASdate == 1") * 100 / total)
    creatorsMore <- c(creatorsMore,sqldf("select count(*) from publications where creatorASuri > 1 or creatorAStext > 1 or creatorASdate > 1") * 100 / total)
    relationsNever <- c(relationsNever,sqldf("select count(*) from publications where relationASuri == 0 and relationAStext == 0 and relationASdate == 0") * 100 / total)
    relationsOne <- c(relationsOne,sqldf("select count(*) from publications where relationASuri == 1 or relationAStext == 1 or relationASdate == 1") * 100 / total)
    relationsMore <- c(relationsMore,sqldf("select count(*) from publications where relationASuri > 1 or relationAStext > 1 or relationASdate > 1") * 100 / total)
    datesNever <- c(datesNever,sqldf("select count(*) from publications where dateASuri == 0 and dateAStext == 0 and dateASdate == 0") * 100 / total)
    datesOne <- c(datesOne,sqldf("select count(*) from publications where dateASuri == 1 or dateAStext == 1 or dateASdate == 1") * 100 / total)
    datesMore <- c(datesMore,sqldf("select count(*) from publications where dateASuri > 1 or dateAStext > 1 or dateASdate > 1") * 100 / total)
    titlesNever <- c(titlesNever,sqldf("select count(*) from publications where titleASuri == 0 and titleAStext == 0 and titleASdate == 0") * 100 / total)
    titlesOne <- c(titlesOne,sqldf("select count(*) from publications where titleASuri == 1 or titleAStext == 1 or titleASdate == 1") * 100 / total)
    titlesMore <- c(titlesMore,sqldf("select count(*) from publications where titleASuri > 1 or titleAStext > 1 or titleASdate > 1") * 100 / total)
    typesNever <- c(typesNever,sqldf("select count(*) from publications where typeASuri == 0 and typeAStext == 0 and typeASdate == 0") * 100 / total)
    typesOne <- c(typesOne,sqldf("select count(*) from publications where typeASuri == 1 or typeAStext == 1 or typeASdate == 1") * 100 / total)
    typesMore <- c(typesMore,sqldf("select count(*) from publications where typeASuri > 1 or typeAStext > 1 or typeASdate > 1") * 100 / total)
    
  },warning = function(w) {
    warning("warning message")
  }, error = function(e) {
    warning("error message")
  }, finally = {
   
    # create a new row for each provider
    # server column
    servers <- c(servers,basename(fileName))
    # total column
    totalPublications <- c(totalPublications,total)
    # oldest column
    oldestPublications <- c(oldestPublications,oldest)
    # newest column
    newestPublications <- c(newestPublications, newest)
    
    # encoding terms column
    sourcesAsUri <- c(sourcesAsUri,fieldsCounter["sourceASuri"])
    sourcesAsTxt <- c(sourcesAsTxt,fieldsCounter["sourceAStext"])
    sourcesAsDate <- c(sourcesAsDate,fieldsCounter["sourceASdate"])
    publishersAsUri <- c(publishersAsUri,fieldsCounter["publisherASuri"])
    publishersAsTxt <- c(publishersAsTxt,fieldsCounter["publisherAStext"])
    publishersAsDate <- c(publishersAsDate,fieldsCounter["publisherASdate"])
    rightsAsUri <- c(rightsAsUri,fieldsCounter["rightsASuri"])
    rightsAsTxt <- c(rightsAsTxt,fieldsCounter["rightsAStext"])
    rightsAsDate <- c(rightsAsDate,fieldsCounter["rightsASdate"])
    descriptionsAsUri <- c(descriptionsAsUri,fieldsCounter["descriptionASuri"])
    descriptionsAsTxt <- c(descriptionsAsTxt,fieldsCounter["descriptionAStext"])
    descriptionsAsDate <- c(descriptionsAsDate,fieldsCounter["descriptionASdate"])
    identifiersAsUri <- c(identifiersAsUri,fieldsCounter["identifierASuri"])
    identifiersAsTxt <- c(identifiersAsTxt,fieldsCounter["identifierAStext"])
    identifiersAsDate <- c(identifiersAsDate,fieldsCounter["identifierASdate"])
    coveragesAsUri <- c(coveragesAsUri,fieldsCounter["coverageASuri"])
    coveragesAsTxt <- c(coveragesAsTxt,fieldsCounter["coverageAStext"])
    coveragesAsDate <- c(coveragesAsDate,fieldsCounter["coverageASdate"])
    formatsAsUri <- c(formatsAsUri,fieldsCounter["formatASuri"])
    formatsAsTxt <- c(formatsAsTxt,fieldsCounter["formatAStext"])
    formatsAsDate <- c(formatsAsDate,fieldsCounter["formatASdate"])
    languagesAsUri <- c(languagesAsUri,fieldsCounter["languageASuri"])
    languagesAsTxt <- c(languagesAsTxt,fieldsCounter["languageAStext"])
    languagesAsDate <- c(languagesAsDate,fieldsCounter["languageASdate"])
    subjectsAsUri <- c(subjectsAsUri,fieldsCounter["subjectASuri"])
    subjectsAsTxt <- c(subjectsAsTxt,fieldsCounter["subjectAStext"])
    subjectsAsDate <- c(subjectsAsDate,fieldsCounter["subjectASdate"])
    contributorsAsUri <- c(contributorsAsUri,fieldsCounter["contributorASuri"])
    contributorsAsTxt <- c(contributorsAsTxt,fieldsCounter["contributorAStext"])
    contributorsAsDate <- c(contributorsAsDate,fieldsCounter["contributorASdate"])
    creatorsAsUri <- c(creatorsAsUri,fieldsCounter["creatorASuri"])
    creatorsAsTxt <- c(creatorsAsTxt,fieldsCounter["creatorAStext"])
    creatorsAsDate <- c(creatorsAsDate,fieldsCounter["creatorASdate"])
    relationsAsUri <- c(relationsAsUri,fieldsCounter["relationASuri"])
    relationsAsTxt <- c(relationsAsTxt,fieldsCounter["relationAStext"])
    relationsAsDate <- c(relationsAsDate,fieldsCounter["relationASdate"])
    datesAsUri <- c(datesAsTxt,fieldsCounter["dateASuri"])
    datesAsTxt <- c(datesAsTxt,fieldsCounter["dateAStext"])
    datesAsDate <- c(datesAsDate,fieldsCounter["dateASdate"])
    titlesAsUri <- c(titlesAsUri,fieldsCounter["titleASuri"])
    titlesAsTxt <- c(titlesAsTxt,fieldsCounter["titleAStext"])
    titlesAsDate <- c(titlesAsDate,fieldsCounter["titleASdate"])
    typesAsUri <- c(typesAsUri,fieldsCounter["typeASuri"])
    typesAsTxt <- c(typesAsTxt,fieldsCounter["typeAStext"])
    typesAsDate <- c(typesAsDate,fieldsCounter["typeASdate"])
    
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
resume <- data.frame(server=servers, total=totalPublications, oldest=oldestPublications, newest=newestPublications,sourcesAsUri,sourcesAsTxt,sourcesAsDate,publishersAsUri,publishersAsTxt,publishersAsDate,rightsAsUri,rightsAsTxt,rightsAsDate,descriptionsAsUri,descriptionsAsTxt,descriptionsAsDate,identifiersAsUri,identifiersAsTxt,identifiersAsDate,coveragesAsUri,coveragesAsTxt,coveragesAsDate,formatsAsUri,formatsAsTxt,formatsAsDate,languagesAsUri,languagesAsTxt,languagesAsDate,subjectsAsUri,subjectsAsTxt,subjectsAsDate,contributorsAsUri,contributorsAsTxt,contributorsAsDate,creatorsAsUri,creatorsAsTxt,creatorsAsDate,relationsAsUri,relationsAsTxt,relationsAsDate,datesAsUri,datesAsTxt,datesAsDate,titlesAsUri,titlesAsTxt,titlesAsDate,typesAsUri,typesAsTxt,typesAsDate)
resumeTimes <- cbind(server=servers, total=totalPublications, sourcesNever,sourcesOne,sourcesMore,publishersNever,publishersOne,publishersMore,rightsNever,rightsOne,rightsMore,descriptionsNever,descriptionsOne,descriptionsMore,identifiersNever,identifiersOne,identifiersMore,coveragesNever,coveragesOne,coveragesMore,formatsNever,formatsOne,formatsMore,languagesNever,languagesOne,languagesMore,subjectsNever,subjectsOne,subjectsMore,contributorsNever,contributorsOne,contributorsMore,creatorsNever,creatorsOne,creatorsMore,relationsNever,relationsOne,relationsMore,datesNever,datesOne,datesMore,titlesNever,titlesOne,titlesMore,descriptionsNever,descriptionsOne,descriptionsMore,typesNever,typesOne,typesMore)
resumeTimesTable <- data.frame(resumeTimes)
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
write.csv(resume[,c("server","total","oldest","newest")], file = paste(directory,"/_data.csv",sep=""), na="", fileEncoding = "UTF-8")

# statistics by terms usage

TERMS <- c("sources","publishers","rights","descriptions","identifiers","coverages","formats","languages","subjects","contributors","creators","relations","dates","titles","types")
WT <- NULL
TD <- NULL
TT <- NULL
TTD <- NULL
TU <- NULL
TUD <- NULL
TUT <- NULL
TUTD <- NULL
NEVERT <- NULL
ONET <- NULL
MORET <- NULL

for (term in TERMS){
  asUri <- paste(term,"AsUri",sep="")
  asText <- paste(term,"AsTxt",sep="")
  asDate <- paste(term,"AsDate",sep="")
  neverCount <- paste(term,"Never",sep="")
  oneCount <- paste(term,"One",sep="")
  moreCount <- paste(term,"More",sep="")
  WT <- c(WT,sqldf(paste("select count(*) from resume where ",asUri," == 0 and ",asText," == 0 and ",asDate," == 0",sep="")) * 100 / NUM_UP_SERVERS)
  TD <- c(TD,sqldf(paste("select count(*) from resume where ",asUri," == 0 and ",asText," == 0 and ",asDate," > 0",sep="")) * 100 / NUM_UP_SERVERS)
  TT <- c(TT,( sqldf(paste("select count(*) from resume where ",asUri," == 0 and ",asText," == 0 and ",asDate," == 0",sep="")) + sqldf(paste("select count(*) from resume where ",asUri," == 0 and ",asText," > 0 and ",asDate," == 0",sep="")) )* 100 / NUM_UP_SERVERS)
  TTD <- c(TTD,sqldf(paste("select count(*) from resume where ",asUri," == 0 and ",asText," > 0 and ",asDate," > 0",sep="")) * 100 / NUM_UP_SERVERS)
  TU <- c(TU,sqldf(paste("select count(*) from resume where ",asUri," > 0 and ",asText," == 0 and ",asDate," == 0",sep="")) * 100 / NUM_UP_SERVERS)
  TUD <- c(TUD,sqldf(paste("select count(*) from resume where ",asUri," > 0 and ",asText," == 0 and ",asDate," > 0",sep="")) * 100 / NUM_UP_SERVERS)
  TUT <- c(TUT,sqldf(paste("select count(*) from resume where ",asUri," > 0 and ",asText," > 0 and ",asDate," == 0",sep="")) * 100 / NUM_UP_SERVERS)
  TUTD <- c(TUTD,sqldf(paste("select count(*) from resume where ",asUri," > 0 and ",asText," > 0 and ",asDate," > 0",sep="")) * 100 / NUM_UP_SERVERS)
  NEVERT <- c(NEVERT,mean(as.numeric(resumeTimesTable[,neverCount])))
  ONET <- c(ONET,mean(as.numeric(resumeTimesTable[,oneCount])))
  MORET <- c(MORET,mean(as.numeric(resumeTimesTable[,moreCount])))
}

# Encoding scheme data.frame from sql results
termsEnc <- data.frame(cbind(TD,TT,TTD,TU,TUD,TUT,TUTD), row.names=TERMS)

# Frequency terms data.frame
termsFreq <- data.frame(never=NEVERT,one=ONET,more=MORET, row.names=TERMS)

# Term Frequency BarPlot
barplot(t(termsFreq), 
        main="Term Frequency", 
        col=c("indianred1","palegreen3","steelblue"), 
        ylab = "Rate of Providers",
        beside = T, 
        las=3, 
        names.arg=row.names(termsFreq), 
        cex.axis=1.0, cex.names=0.8, cex.main=1.5,
        legend = c("never","one","more"),
        args.legend = list(x = "topright", bty = "n", inset=c(-0.05, -0.15)))

# Term Encoding BarPlot
barplot(t(termsEnc), 
        main="Term Encoding", 
        col=c("chocolate1","steelblue","wheat4","yellow","tomato","palegreen3","snow4"), 
        ylab = "Rate of Providers",
        las=3, 
        names.arg=row.names(termsEnc), 
        cex.axis=1.0, cex.names=0.8, cex.main=1.5,
        legend = c("D","T","TD","U","UD","UT","UTD"),
        args.legend = list(x = "topright", bty = "n", inset=c(-0.08, -0.23)))

