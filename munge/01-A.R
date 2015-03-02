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
disWide <- dcast(dis, pid ~ wave)
cache('disWide')

## Disability from Work Status Variables
disJobVarNames <- paste(bhps_prefixes, "jbstat", sep="")
disJob <- get_variable(disJobVarNames, "disJob")
disJobRecodeString <- "-10:-1=NA"
disJob$disJob <- iRecode(disJob$disJob, disJobRecodeString)
disJob[which(!is.na(disJob$disJob)),"disJobDummy"] <- 0
disJob[which(disJob$wave==1991&disJob$disJob==7),"disJobDummy"] <- 1
disJob[which(disJob$wave>1991&disJob$disJob==8),"disJobDummy"] <- 1
cache('disJob')

# -------------------- find people who became disabled -------------------- #
notDisabledYear <- firstStatus(dis, "disDummy", 0, "notDisMin")
disabledYear <- firstStatus(dis, "disDummy", 1, "disMin")
lastDisStatus <- lastStatus(dis, "disDummy", 0, "notDisMax")
disabilityCombined <- merge(notDisabledYear, disabledYear, by=idName)
disabilityCombined <- merge(disabilityCombined, lastDisStatus, by=idName)


# select those who got disabled
gotDisabled <- disabilityCombined[which(disabilityCombined[,"notDisMin"]<
                                        disabilityCombined[,"disMin"]),]
cache('gotDisabled')


# select those who got and stayed disabled
stayedDisabled <- disabilityCombined[which(disabilityCombined[, "notDisMin"] <
                                        disabilityCombined[, "disMin"] &
                                        disabilityCombined[, "notDisMax"] <
                                        disabilityCombined[, "disMin"]),]
cache('stayedDisabled')

# ---- find people who became disabled using work status variable ----- #
notDisabledYear2 <- firstStatus(disJob, "disJobDummy", 0, "notDisMin2")
disabledYear2 <- firstStatus(disJob, "disJobDummy", 1, "disMin2")
lastDisStatus2 <- lastStatus(disJob, "disJobDummy", 0, "notDisMax2")
disabilityCombined2 <- merge(notDisabledYear2, disabledYear2, by=idName)
disabilityCombined2 <- merge(disabilityCombined2, lastDisStatus2, by=idName)


# select those who got disabled
gotDisabled2 <- disabilityCombined2[which(disabilityCombined2[,"notDisMin2"]<
                                        disabilityCombined2[,"disMin2"]),]
cache('gotDisabled2')


# select those who got and stayed disabled
stayedDisabled2 <- disabilityCombined2[which(disabilityCombined2[, "notDisMin2"] <
                                        disabilityCombined2[, "disMin2"] &
                                        disabilityCombined2[, "notDisMax2"] <
                                        disabilityCombined2[, "disMin2"]),]
cache('stayedDisabled2')


# -------------------- wide file for those who got disabled -------------------- #
gotDisWide <- merge(disWide, gotDisabled, by = idName)

# -------------------- set up transitions with TraMineR -------------------- #
#gotDisWide$seq <- seqconc(gotDisWide[,2:27], void='')
dis.seq <- seqdef(gotDisWide, 2:12, labels = c("disabled","notDisabled"), id=gotDisWide$pid)
#gotDisWide$dis.seq <- dis.seq
transition <- seqetm(dis.seq, method="transition")
#transition <- transition[1:2,1:2]
dis.tse <- seqformat(gotDisWide, 2:12, from="STS", to = "TSE", tevent=transition, id="pid")

########################################################################
# Number of transitions
# TraMineR doesn't handle missing data well, so we have to compare two strategies to
# identify transitions that do not occur around a missing occasion
########################################################################


# create dss, the simplified representation of the sequence
dis.dss <- seqdss(dis.seq)
gotDisWide$dis.dss <- dis.dss

# Identify number of transitions from dss (this should be accurate)
gotDisWide$nTransitions <- seqlength(gotDisWide$dis.dss)-1

# Identify number of distinct states from tse data (this will miss transitions with missing data)
dis.agg <- aggregate(dis.tse$id, by=list(dis.tse$id), FUN=length)
names(dis.agg) <- c(idName, "dis.agg")
dis.agg$pid <- as.integer(dis.agg$pid)
gotDisWide <- merge(gotDisWide, dis.agg, by = idName, sort=TRUE)

# Find duration of spells
dis.dur <- seqdur(dis.seq)
dis.dur.df <- data.frame(dis.dur)
dis.dur.df$pid <- row.names(dis.dur.df)


gotDisWide <- merge(gotDisWide, dis.dur.df, by="pid")
cache('gotDisWide')



