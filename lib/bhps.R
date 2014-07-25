

# -------------------- define paths ------------------------------ #

pathOriginalData <- "~/Documents/Datasets/BHPS2009/stata8/" # path of original panel data files, ending with "/"
pathWorking <- "../data/" # path where new files will be stored

# -------------------- study characteristics -------------------- #


firstYearOfStudy <- 1991
lastYearOfStudy <- 2008

# A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  
# 91 92 93 94 95 96 97 98 99 00 01 02 03 04 05 06 07 08
# 1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 

startingWave <- 1

masterFile <- "ppfad.dta"
idName <- "persnr"
hID <- "hhnr" # Needs wave prefix
charsToSub <- "\\$"

# -------------------- list of waves and files -------------------- #

gsoep_prefixes <- c(letters[1:7],letters[7:26],paste("b",letters[1:2],sep=""))
gsoep_years <- c(1984:1990,1990:2011)
gsoep_ind_files <- paste(gsoep_prefixes,"p.dta",sep="")
gsoep_ind_files[8] <- "gpost.dta"
gsoep_h_files <- paste(gsoep_prefixes, "h.dta", sep="")
gsoep_h_files[8] <- "ghost.dta"
gsoep_eq_files <- paste(gsoep_prefixes, "pequiv.dta", sep="")
gsoep_eq_files[8] <- NA
gsoep_files <- as.data.frame(cbind(gsoep_years,gsoep_ind_files,gsoep_h_files,gsoep_eq_files), stringsAsFactors=FALSE)
gsoep_files$gsoep_years <- as.numeric(gsoep_files$gsoep_years)



## Function to get any longitudinal individual-level variable
## Provide a matrix with each line specifying a wave and a variable name
## Provides either a wide or long file with respondent ID, and all requested waves of a variable.
## Sample Year is provided with long files
## IMPORTANT: Length of 'variables' must match gsoep_files, even if some entries are missing
get_variable <- function(variables, newName, wide=FALSE, convertFactors=FALSE) {
    master <- read.dta(paste(pathOriginalData, masterFile, sep=""))
    master <- data.frame(master[,c(idName)], stringsAsFactors=FALSE)
    names(master) <- idName
    for (i in 1:length(variables)) {
        if (!is.na(variables[i])) {
            newDataFileName <- paste(pathOriginalData, gsoep_files[i,2], sep="")
            newVariableName <- paste(newName, gsoep_files[i,1], sep="-")
            data <- read.dta(newDataFileName,convert.factors=convertFactors)
            data <- data[,c(idName, tolower(variables[i]))]
            names(data)[2] <- newVariableName
            master <- merge(master, data, by = idName, all.x=TRUE)
            if (i==8) {
                master[,newVariableName] <- master[,paste(newVariableName, "x", sep=".")]
                master[which(is.na(master[,newVariableName])),newVariableName] <-
                    master[which(is.na(master[,newVariableName])),paste(newVariableName, "y", sep=".")]
                drops <- c(paste(newVariableName, c("x","y"), sep="."))
                master <- master[,!names(master) %in% drops]
            }
        }
    }
    if (wide==FALSE) {
        master <- melt(master, id.vars=idName)
        master$wave <- substring(master$variable,nchar(newName)+2,nchar(newName)+5)
        master$wave <- as.numeric(master$wave)
        master$variable <- substring(master$variable, 1, nchar(newName))
        form <- as.formula(paste(idName, " + wave ~ variable", sep=""))
        master <- dcast(master, form, value.var="value")
    }
    return(master)
}

iRecode <- function (x, rstring) { recode (x, rstring) }

# -------------------- find first year in study -------------------- #


    
# -------------------- identify those who changed status -------------------- #
# This function takes a long file with id, wave, and a status variable
# Can be used to find first wave in target or non-target status
firstStatus <- function(data, statusVariable, targetStatus, newName=NA) {
    select <- data[which(data[,statusVariable]==targetStatus),]
    min <- aggregate(select[,"wave"], by = list(select[,idName]), min)
    if(is.na(newName)) newName <- paste(statusVariable, "Min", sep="")
    names(min) <- c(idName, newName)
    return(min)
}

lastStatus <- function(data, statusVariable, targetStatus, newName=NA) {
    select <- data[which(data[,statusVariable]==targetStatus),]
    min <- aggregate(select[,"wave"], by = list(select[,idName]), max)
    if(is.na(newName)) newName <- paste(statusVariable, "Max", sep="")
    names(min) <- c(idName, newName)
    return(min)
}

