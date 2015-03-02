##############################################################
# Create final data files for analyses & Run Analyses
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




#--------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------#
