##############################################################
# Get Variables for Analysis
##############################################################

## Life Satisfaction
lsVarNames <- paste(ls_waves, "LFSATO", sep="")
ls <- get_variable(lsVarNames, "ls", bhpsFiles=ls_bhps_files)
lsRecodeString <- "-10:-1=NA"
ls$ls <- iRecode(ls$ls, lsRecodeString)
cache('ls')

## Disability
# 2003 wave only asked of proxies; therefore, only use to 2001
dis_waves <- c("a","b","c","d","e","f","g","h","i","j","k")
dis_bhps_files <- (bhps_files[c(1:11,13),])
disVarNames <- paste(dis_waves, "hldsbl", sep="")
dis <- get_variable(disVarNames, "dis", bhpsFiles=dis_bhps_files)
disRecodeString <- "-10:-1=NA"
dis$dis <- iRecode(dis$dis, disRecodeString)
dis[which(!is.na(dis$dis)),"disDummy"] <- 0
dis[which(dis$dis==1),"disDummy"] <- 1
cache('dis')

## Disability from Work Status Variables
disJobVarNames <- paste(bhps_prefixes, "jbstat", sep="")
disJob <- get_variable(disJobVarNames, "disJob")
disJobRecodeString <- "-10:-1=NA"
disJob$disJob <- iRecode(disJob$disJob, disJobRecodeString)
disJob[which(!is.na(disJob$disJob)),"disJobDummy"] <- 0
disJob[which(disJob$wave==1991&disJob$disJob==7),"disJobDummy"] <- 1
disJob[which(disJob$wave>1991&disJob$disJob==8),"disJobDummy"] <- 1
cache('disJob')

