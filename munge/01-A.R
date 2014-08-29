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



##############################################################
# Create final data files for analyses
##############################################################

# Start just with old disability question

disData <- merge(ls, dis, by=c(idName, "wave"))

# Aggregate disability status to find percentage of years disabled
disDataAgg <- aggregate(disData$disDummy, by=list(disData$pid), FUN=mean, na.rm=TRUE)
names(disDataAgg) <- c("pid", "percentDis")
disData <- merge(disData, disDataAgg, by="pid")
disData$disDummy.C <- disData$disDummy-disData$percentDis


#--------------------------------------------------------------------------------------------#
# First analysis. Simple model with all participants examining effect of disability #
#--------------------------------------------------------------------------------------------#


# Baseline Model
baseline <- lmer(ls ~ 1 + (1 | pid), data=disData)

# Model 1 -- Basic MLM without centering
model1a <- lmer(ls ~ disDummy + (1 + disDummy | pid), data=disData) 
test <- lmer(ls ~ disDummy + (1 + disDummy | pid), data=disData,
             control=lmerControl(optimizer="Nelder_Mead"))

# Model 2 -- Centered disability + level 2 percentage: Equivalent to fixed effects
model2a <- lmer(ls ~ disDummy.C + percentDis + (1 + disDummy.C | pid), data=disData)
test2 <- lmer(ls ~ disDummy.C + percentDis + (1 + disDummy.C | pid), data=disData,
              control=lmerControl(optimizer="Nelder_Mead"))

summary(model1a)
summary(model2a)

# Models get warnings about lack of convergence. The following is another test that can be run;
# if values are lower than .001, things are okay.
relgrad <- with(model2a@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

#--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------#
# Replicate old model; only select those who start disabled and stay disabled; stay in study
# for at least three years of disability
#--------------------------------------------------------------------------------------------#

select <- gotDisWide[which(gotDisWide$dis.dss$ST3=="%"&gotDisWide$DUR2>=3),c("pid", "notDisMin", "disMin", "DUR1", "DUR2")]
replication <- merge(disData, select, by="pid")
replication <- replication[order(replication$pid, replication$wave),]

replication$year <- as.numeric(replication$wave)
replication$linear <- 0
replication[which(replication$year>replication$disMin), "linear"] <-
    replication[which(replication$year>replication$disMin), "year"] -
    replication[which(replication$year>replication$disMin), "disMin"]

replication$yearBefore <- 0
replication[which(replication$year==replication$disMin-1),'yearBefore'] <- 1

# Baseline Model
baseline <- lmer(ls ~ 1 + (1 | pid), data=replication)

# Model 1 -- Basic MLM without centering
model1 <- lmer(ls ~ yearBefore + disDummy + (1 + disDummy | pid), data=replication)

# Model 2 -- Centered disability + level 2 percentage: Equivalent to fixed effects
model2 <- lmer(ls ~ yearBefore + disDummy.C + percentDis + (1 + disDummy.C | pid), data=replication)

summary(model1)
summary(model2)

# Model3 -- Basic MLM without centering plus linear
model3 <- lmer(ls ~ yearBefore + disDummy + linear + (1 + disDummy + linear | pid), data=replication)
summary(model3)

# Model4 -- MLM with centering
# Create new average linear variable
disDataAgg2 <- aggregate(replication$linear, by=list(replication$pid), FUN=mean, na.rm=TRUE)
names(disDataAgg2) <- c("pid", "meanLinear")
replication <- merge(replication, disDataAgg2, by="pid")
replication$linear.C <- replication$linear-replication$meanLinear

model4 <- lmer(ls ~ yearBefore + disDummy.C + linear.C + percentDis + meanLinear + (1 + disDummy.C + linear.C | pid), data=replication)
summary(model4)


# Model5 -- check for effect of general linear decline
replication$adjLS <- replication$ls-.03*(replication$year-replication$notDisMin)
model5 <- lmer(adjLS ~ disDummy.C + linear.C + percentDis + meanLinear + (1 + disDummy.C + linear.C | pid), data=replication)
summary(model5)

                                        #--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------#
# Compare those who stayed disabled for three years (from above) to those who 
# recovered.                                        #--------------------------------------------------------------------------------------------#

select2 <- gotDisWide[which(gotDisWide$dis.dss$ST3=="0"&gotDisWide$dis.dss$ST4=="%"),c("pid", "notDisMin", "disMin", "DUR1", "DUR2", "DUR3")]
comparison <- merge(disData, select2, by="pid")
comparison <- comparison[order(comparison$pid, comparison$wave),]

comparison$year <- as.numeric(comparison$wave)
comparison$linear <- 0
comparison$spell1End <- comparison$disMin+comparison$DUR2-1
comparison[which(comparison$year>comparison$disMin&comparison$year<=comparison$spell1End), "linear"] <-
    comparison[which(comparison$year>comparison$disMin&comparison$year<=comparison$spell1End), "year"] -
    comparison[which(comparison$year>comparison$disMin&comparison$year<=comparison$spell1End), "disMin"]

comparison$yearBefore <- 0
comparison[which(comparison$year==comparison$disMin-1),'yearBefore'] <- 1


# Model 1 -- Basic MLM without centering
model1c <- lmer(ls ~ yearBefore + disDummy + (1 + disDummy | pid), data=comparison)
summary(model1c)

# Model 2 -- Centered disability + level 2 percentage: Equivalent to fixed effects
model2c <- lmer(ls ~ yearBefore + disDummy.C + percentDis + (1 + disDummy.C | pid), data=comparison)
summary(model2c)


# Model3 -- Basic MLM without centering plus linear
model3c <- lmer(ls ~ yearBefore + disDummy + linear + (1 + disDummy + linear | pid), data=comparison)
summary(model3c)

# Model4 -- MLM with centering
# Create new average linear variable
disDataAgg3 <- aggregate(comparison$linear, by=list(comparison$pid), FUN=mean, na.rm=TRUE)
names(disDataAgg3) <- c("pid", "meanLinear")
comparison <- merge(comparison, disDataAgg3, by="pid")
comparison$linear.C <- comparison$linear-comparison$meanLinear

model4 <- lmer(ls ~ disDummy.C + linear.C + percentDis + meanLinear + (1 + disDummy.C + linear.C | pid), data=comparison)
summary(model4)


# Model5 -- check for effect of general linear decline
replication$adjLS <- replication$ls-.03*(replication$year-replication$notDisMin)
model5 <- lmer(adjLS ~ disDummy.C + linear.C + percentDis + meanLinear + (1 + disDummy.C + linear.C | pid), data=replication)
summary(model5)


#--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#
