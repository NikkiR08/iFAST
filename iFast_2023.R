library(dplyr) #Load the package dplyr
library(ggplot2) #Load the package ggplot2
library(reshape2)
library(cowplot)
library(scales)
library(diagram)
library(data.table)

####****PARAMETERS****#####

params <- read.csv("ifast_parameters.csv")
params <- as.data.table(params)

#  Start by defining states
state.names<-c("A.clinsusUTI","B.empiricUTIR","X.empiricUTIS",
               "C.personRcultureS","D.personRcultureR","E.personScultureS",
               "F.personScultureR",
               "G.ICU","H.BSI","I.death","J.recovery") 
state.names

cohort.size <- 1000

### COSTS #################### 


### recurring health state costs
c.dmcA <- 0  ## Direct medical costs associated with state A
c.dmcB <- params[parameter=="cost.empiricuti",value]
c.dmcX <- params[parameter=="cost.empiricuti",value]  ##Direct medical costs associated with state 
c.dmcC <- params[parameter=="cost.cultureuti",value]   ## Direct medical costs associated with state C
c.dmcD <- params[parameter=="cost.cultureuti",value]   ##Direct medical costs associated with state D
c.dmcE <- params[parameter=="cost.cultureuti",value]  ## Direct medical costs associated with state E
c.dmcF <- params[parameter=="cost.cultureuti",value]   ##Direct medical costs associated with state F
c.dmcG <- params[parameter=="cost.icu",value] 
c.dmcH <- params[parameter=="cost.bsi",value] 
c.dmcI <- 0
c.dmcJ <- 0

c.dmc <- c(c.dmcA, c.dmcB, c.dmcX,c.dmcC, c.dmcD, c.dmcE,
           c.dmcF, c.dmcG, c.dmcH,c.dmcI, c.dmcJ)

### one off costs - not ideal for generally modelling
### but as no discounting will multiply all cohort
### by 1 off test as assumed everyone gets tested - then add at end
c.standardtest.U <- params[parameter=="cost.standardtest.U",value]
c.ifast.U <-params[parameter=="cost.ifast.U",value]

## A vector storing the direct costs associated with each state
## the order is important as these will be multiplied according to 
## matrix multiplication 
## (e.g. first value in dmc - direct medical cost of A - will be multiplied by 
## first value in a matrix that represents number of cases in state A) 

### OTHER PARAMETERS #######
dr.c <- params[parameter=="dr.cost",value]   ## Annual discount rate - costs (%)
dr.o <- params[parameter=="dr.outcomes",value]   ## Annual discount rate - benefits (%) 

#  Set the total number of cycles for the model to run
cycles <- params[parameter=="n.cycles",value]

#  This shows the probability of transitioning from one state to another 
n.states <- length(state.names)

#  Seed the starting states of the model (cycle 0)
seed <- c(cohort.size,rep(0,(n.states-1))) ## i.e. everyone starts in State A

### transition probabilities###
p.BSI <- params[parameter=="p.uti2bsi",value]
p.BSI.death <- params[parameter=="p.bsi2die",value]
p.death <-  params[parameter=="p.die",value]
p.BSI.icu <- params[parameter=="p.bsi2icu",value]
p.icu <-  params[parameter=="p.icu",value]
p.icu2die <- params[parameter=="p.icu2die",value]

t.current <-   params[parameter=="t.current",value]
t.ifast <-   params[parameter=="t.ifast",value]

### time dependent vector for testing & uti & bsi transitions
p.testresultcurrent <-  c(rep(0,(t.current-1)),1,rep(0,(cycles-t.current)))
p.testresultifast <-  c(rep(0,(t.ifast-1)),1,rep(0,(cycles-t.ifast)))

p.uti2recovery_R <- params[parameter=="p.uti2recovery_R",value]
p.uti2recovery_S <- params[parameter=="p.uti2recovery_S",value]

p.bsi2recovery <- params[parameter=="p.bsi2recovery",value]
p.icu2recovery <- params[parameter=="p.icu2recovery",value]

prevalence <- params[parameter=="r.prev",value]
sensitivity <- params[parameter=="sensitivity",value]
specificity <- params[parameter=="specificity",value]
  
### if want to make antibiotic culture sensitivity would use this fomula
### but would also have to change the transition probabilities in tm
# personRcultureR <- prevalence * sensitivity
# personRcultureS <- prevalence * (1 - sensitivity) 
# personScultureR <- (1 - prevalence) * (1-specificity) 
# personScultureS <- (1 - prevalence) * specificity 

personRcultureR <- sensitivity
personRcultureS <- (1 - sensitivity) 
personScultureR <- (1-specificity) 
personScultureS <- specificity 

#### create transition matrix first to capture time dependencies
### also makes it less likely to make mistake as can assign spefic transitions
tm.current <- array(data=0,dim=c(n.states, n.states, cycles),
                dimnames= list(state.names, state.names, 1:cycles))

tm.current["A.clinsusUTI","B.empiricUTIR",] <- prevalence
tm.current["A.clinsusUTI","X.empiricUTIS",] <- 1-prevalence

tm.current["B.empiricUTIR","C.personRcultureS",] <- p.testresultcurrent*personRcultureS
tm.current["B.empiricUTIR","D.personRcultureR",] <- p.testresultcurrent*personRcultureR
# tm.current["B.empiricUTIR","E.personScultureS",] <- p.testresultcurrent*personScultureS
# tm.current["B.empiricUTIR","F.personScultureR",] <- p.testresultcurrent*personScultureR
tm.current["B.empiricUTIR","G.ICU",]  <- p.icu
tm.current["B.empiricUTIR","H.BSI",] <- p.BSI
tm.current["B.empiricUTIR","I.death",] <- p.death
tm.current["B.empiricUTIR","J.recovery",] <- p.uti2recovery_R
tm.current["B.empiricUTIR","B.empiricUTIR",] <- 1-(p.icu+p.BSI+p.death+
                                                     p.uti2recovery_R)
## !!! note only transition to culture on t.culture
tm.current["B.empiricUTIR","G.ICU",t.current]  <- 0
tm.current["B.empiricUTIR","H.BSI",t.current] <- 0
tm.current["B.empiricUTIR","I.death",t.current] <- 0
tm.current["B.empiricUTIR","J.recovery",t.current] <-0 
tm.current["B.empiricUTIR","B.empiricUTIR",t.current] <-0 

tm.current["X.empiricUTIS","E.personScultureS",] <- p.testresultcurrent*personScultureS
tm.current["X.empiricUTIS","F.personScultureR",] <- p.testresultcurrent*personScultureR
tm.current["X.empiricUTIS","G.ICU",]  <- p.icu
tm.current["X.empiricUTIS","H.BSI",] <- p.BSI
tm.current["X.empiricUTIS","I.death",] <- p.death
tm.current["X.empiricUTIS","J.recovery",] <- p.uti2recovery_S
tm.current["X.empiricUTIS","X.empiricUTIS",] <- 1-(p.icu+p.BSI+p.death+
                                                     p.uti2recovery_S)
## !!! note only transition to culture on t.culture
tm.current["X.empiricUTIS","G.ICU",t.current]  <- 0
tm.current["X.empiricUTIS","H.BSI",t.current] <- 0
tm.current["X.empiricUTIS","I.death",t.current] <- 0
tm.current["X.empiricUTIS","J.recovery",t.current] <-0 
tm.current["X.empiricUTIS","X.empiricUTIS",t.current] <-0 

tm.current["C.personRcultureS","G.ICU",] <- p.icu
tm.current["C.personRcultureS","H.BSI",] <- p.BSI
tm.current["C.personRcultureS","I.death",] <- p.death
tm.current["C.personRcultureS","J.recovery",] <- p.uti2recovery_R ### !!! this might need to change 
tm.current["C.personRcultureS","C.personRcultureS",] <- 1-(p.icu+p.BSI+p.death+
                                                             p.uti2recovery_R)
tm.current["D.personRcultureR","G.ICU",] <- p.icu
tm.current["D.personRcultureR","H.BSI",] <- p.BSI
tm.current["D.personRcultureR","I.death",] <- p.death
tm.current["D.personRcultureR","J.recovery",] <- p.uti2recovery_S ### !!! this might need to change 
tm.current["D.personRcultureR","D.personRcultureR",] <- 1-(p.icu+p.BSI+p.death+
                                                             p.uti2recovery_S)

tm.current["E.personScultureS","G.ICU",] <- p.icu
tm.current["E.personScultureS","H.BSI",] <- p.BSI
tm.current["E.personScultureS","I.death",] <- p.death
tm.current["E.personScultureS","J.recovery",] <- p.uti2recovery_S ### !!! this might need to change 
tm.current["E.personScultureS","E.personScultureS",] <- 1-(p.icu+p.BSI+p.death+
                                                             p.uti2recovery_S)

tm.current["F.personScultureR","G.ICU",] <- p.icu
tm.current["F.personScultureR","H.BSI",] <- p.BSI
tm.current["F.personScultureR","I.death",] <- p.death
tm.current["F.personScultureR","J.recovery",] <- p.uti2recovery_S ### !!! this might need to change 
tm.current["F.personScultureR","F.personScultureR",] <- 1-(p.icu+p.BSI+p.death+
                                                             p.uti2recovery_S)

tm.current["G.ICU","I.death",] <- p.icu2die
tm.current["G.ICU","J.recovery",] <- p.icu2recovery
tm.current["G.ICU","G.ICU",] <- 1-(p.icu2die+p.icu2recovery)

tm.current["H.BSI","I.death",] <- p.BSI.death
tm.current["H.BSI","J.recovery",] <- p.bsi2recovery
tm.current["H.BSI","G.ICU",] <- p.bsi2icu
tm.current["H.BSI","H.BSI",] <- 1-(p.BSI.death+p.bsi2recovery)

tm.current["I.death","I.death",] <- 1
tm.current["J.recovery","J.recovery",] <- 1

### testing adds to 1
rowSums(tm.current[,,48])
rowSums(tm.current[,,1])

#  Create a trace for the UTI arm
#  This captures the number of people in each state at any one time
trace.current <- matrix(data=NA, nrow=cycles, ncol=n.states) ## the length of the matrix is equivalent to the number of cycles
colnames(trace.current) <- state.names

# Note this does not include any cost/effect of anything occurring before cycle 1
trace.current[1,] <- seed%*%tm.current[,,1] 

## Let's see what the first few rows of the Markov trace looks like:
head(trace.current) ## the head() function returns the first 6 rows of a matrix (or data.frame)

for (i in 2:cycles) {   ### we want to get the estimates for cycle2 (row 3) to cycle20 (row 21)
  trace.current[i,] <- trace.current[i-1,] %*% tm.current[,,i]

}

rownames(trace.current) <- paste("cycle", 1:cycles, sep = "_") ## assigning the rownames to highlight each row is 1 cycle run of the markov model
rowSums(trace.current) ## check that they sum to 1, if not something has gone wrong in your calculations

tracedata <- as.data.frame(trace.current)
tracedata$cycle <- 1: cycles
tracedata$BSI <- tracedata$H.BSI
tracedata$stillinhospital <- cohort.size-(tracedata$I.death+tracedata$J.recovery)
df2 <- select(tracedata,cycle,stillinhospital,BSI,J.recovery, I.death)
colnames(df2) <- c("cycle","still_in_hospital","BSI", "recovery", "death")
df2 <- melt(df2 ,  id.vars = 'cycle', variable.name = 'variable')

p1<- ggplot(df2, aes(cycle,value)) + 
  geom_line(aes(colour = variable),size=0.5) + xlab("hours") + ylab("proportion") +
  geom_point(aes(colour = variable),size=0.3)+
  scale_color_brewer(name = "state", palette = "Set2") + scale_y_continuous(limits = c(0, 1),expand = c(0,0)) +
  scale_x_continuous(limits = c(0, cycles),expand = c(0,0)) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()
p1

##### HOURS and TYPE of antibiotic use per patient #####

inappropriate <- tracedata$B.empiricUTIR+tracedata$C.personRcultureS+
  tracedata$F.personScultureR
appropriate <- tracedata$X.empiricUTIS+tracedata$D.personRcultureR+
  tracedata$E.personScultureS
ab_use <- data.frame(inappropriate, appropriate)
ab_use$cycles <- 1:cycles
ab_use <- melt(ab_use ,  id.vars = 'cycles', variable.name = 'variable')

save(ab_use, file="outputs/ab_use_current.RData")

##### QUALITY ADJUSTED LIFE HOURS LOST #####
###!! important to note this model looks at decrements not total QALYs
QALH.uti <- params[parameter=="q.uti",value]
QALH.bsi <- params[parameter=="q.bsi",value]
QALH.icu <- params[parameter=="q.icu",value]
### !!! note need to update cost and qalh matrices by hand if change
## name or order of states as currently stands
QALH <-c(rep(QALH.uti,7),QALH.icu, QALH.bsi,0,1)##!!! got to here 
## these are for days in hospital. not including deaths. will add those on later.
lh_current <- trace.current %*% QALH 

undisc.lh_current <- colSums(lh_current) ## calculating the total LHs from UTI arm
undisc.ly_hosp_current <- undisc.lh_current/(24*365)
### no discounted needed as all within 1 year

deaths_current <- trace.current[cycles,"I.death"] ## take the last value
disc.ly_death_current <- deaths_current*params[parameter=="qaly.death", value]
  
 qaly.lost <- disc.ly_death_current+undisc.ly_hosp_current
#### COST CALCULATIONS #####
 
## undiscounted:
cost_orig <- trace.current %*% c.dmc 
 cost_time <- as.data.frame(cost_orig)
 cost_time$cycles <- 1:cycles
 colnames(cost_time)<- c("cost","cycles")
 cost_time$cumsumcost <- cumsum(cost_time$cost) 
 ### !!! note cost_time is just the recurrent costs not inc upfront costs at time of testing
hospital_cost <-sum(cost_orig)

upfront_testingcost <- cohort.size*c.standardtest.U
undisc.cost<-hospital_cost+upfront_testingcost ## calculating the total cost of the UTI arm
#undisc.cost 
### no discounting needed as all one year

#### fill results
results <- data.frame(excess_cost=c(0,0),
                      excess_qalyloss = c(0,0))
rownames(results) <- c("current","iFAST")

results["current","excess_cost"] <- undisc.cost
results["current","excess_qalyloss"] <- -qaly.lost ##negative for loss

####****iFAST ARM***** ##### 

tm.ifast <- array(data=0,dim=c(n.states, n.states, cycles),
                    dimnames= list(state.names, state.names, 1:cycles))

tm.ifast["A.clinsusUTI","B.empiricUTIR",] <- prevalence
tm.ifast["A.clinsusUTI","X.empiricUTIS",] <- 1-prevalence

tm.ifast["B.empiricUTIR","C.personRcultureS",] <- p.testresultifast*personRcultureS
tm.ifast["B.empiricUTIR","D.personRcultureR",] <- p.testresultifast*personRcultureR
tm.ifast["B.empiricUTIR","G.ICU",]  <- p.icu
tm.ifast["B.empiricUTIR","H.BSI",] <- p.BSI
tm.ifast["B.empiricUTIR","I.death",] <- p.death
tm.ifast["B.empiricUTIR","J.recovery",] <- p.uti2recovery_R
tm.ifast["B.empiricUTIR","B.empiricUTIR",] <- 1-(p.icu+p.BSI+p.death+
                                                     p.uti2recovery_R)
## !!! note only transition to culture on t.culture
tm.ifast["B.empiricUTIR","G.ICU",t.ifast]  <- 0
tm.ifast["B.empiricUTIR","H.BSI",t.ifast] <- 0
tm.ifast["B.empiricUTIR","I.death",t.ifast] <- 0
tm.ifast["B.empiricUTIR","J.recovery",t.ifast] <-0 
tm.ifast["B.empiricUTIR","B.empiricUTIR",t.ifast] <-0 

tm.ifast["X.empiricUTIS","E.personScultureS",] <-p.testresultifast*personScultureS
tm.ifast["X.empiricUTIS","F.personScultureR",] <- p.testresultifast*personScultureR
tm.ifast["X.empiricUTIS","G.ICU",]  <- p.icu
tm.ifast["X.empiricUTIS","H.BSI",] <- p.BSI
tm.ifast["X.empiricUTIS","I.death",] <- p.death
tm.ifast["X.empiricUTIS","J.recovery",] <- p.uti2recovery_S
tm.ifast["X.empiricUTIS","X.empiricUTIS",] <- 1-(p.icu+p.BSI+p.death+
                                                     p.uti2recovery_S)
## !!! note only transition to culture on t.culture
tm.ifast["X.empiricUTIS","G.ICU",t.ifast]  <- 0
tm.ifast["X.empiricUTIS","H.BSI",t.ifast] <- 0
tm.ifast["X.empiricUTIS","I.death",t.ifast] <- 0
tm.ifast["X.empiricUTIS","J.recovery",t.ifast] <-0 
tm.ifast["X.empiricUTIS","X.empiricUTIS",t.ifast] <-0 

tm.ifast["C.personRcultureS","G.ICU",] <- p.icu
tm.ifast["C.personRcultureS","H.BSI",] <- p.BSI
tm.ifast["C.personRcultureS","I.death",] <- p.death
tm.ifast["C.personRcultureS","J.recovery",] <- p.uti2recovery_R ### !!! this might need to change 
tm.ifast["C.personRcultureS","C.personRcultureS",] <- 1-(p.icu+p.BSI+p.death+
                                                             p.uti2recovery_R)
tm.ifast["D.personRcultureR","G.ICU",] <- p.icu
tm.ifast["D.personRcultureR","H.BSI",] <- p.BSI
tm.ifast["D.personRcultureR","I.death",] <- p.death
tm.ifast["D.personRcultureR","J.recovery",] <- p.uti2recovery_S ### !!! this might need to change 
tm.ifast["D.personRcultureR","D.personRcultureR",] <- 1-(p.icu+p.BSI+p.death+
                                                             p.uti2recovery_S)

tm.ifast["E.personScultureS","G.ICU",] <- p.icu
tm.ifast["E.personScultureS","H.BSI",] <- p.BSI
tm.ifast["E.personScultureS","I.death",] <- p.death
tm.ifast["E.personScultureS","J.recovery",] <- p.uti2recovery_S ### !!! this might need to change 
tm.ifast["E.personScultureS","E.personScultureS",] <- 1-(p.icu+p.BSI+p.death+
                                                             p.uti2recovery_S)

tm.ifast["F.personScultureR","G.ICU",] <- p.icu
tm.ifast["F.personScultureR","H.BSI",] <- p.BSI
tm.ifast["F.personScultureR","I.death",] <- p.death
tm.ifast["F.personScultureR","J.recovery",] <- p.uti2recovery_S ### !!! this might need to change 
tm.ifast["F.personScultureR","F.personScultureR",] <- 1-(p.icu+p.BSI+p.death+
                                                             p.uti2recovery_S)

tm.ifast["G.ICU","I.death",] <- p.icu2die
tm.ifast["G.ICU","J.recovery",] <- p.icu2recovery
tm.ifast["G.ICU","G.ICU",] <- 1-(p.icu2die+p.icu2recovery)

tm.ifast["H.BSI","I.death",] <- p.BSI.death
tm.ifast["H.BSI","J.recovery",] <- p.bsi2recovery
tm.ifast["H.BSI","H.BSI",] <- 1-(p.BSI.death+p.bsi2recovery)

tm.ifast["I.death","I.death",] <- 1
tm.ifast["J.recovery","J.recovery",] <- 1

### testing adds to 1
rowSums(tm.ifast[,,48])
rowSums(tm.ifast[,,1])

#  Create a trace for the UTI arm
#  This captures the number of people in each state at any one time
trace.ifast <- matrix(data=NA, nrow=cycles, ncol=n.states) ## the length of the matrix is equivalent to the number of cycles
colnames(trace.ifast) <- state.names

# Note this does not include any cost/effect of anything occurring before cycle 1
trace.ifast[1,] <- seed%*%tm.ifast[,,1] 

## Let's see what the first few rows of the Markov trace looks like:
head(trace.ifast) ## the head() function returns the first 6 rows of a matrix (or data.frame)

for (i in 2:cycles) {   ### we want to get the estimates for cycle2 (row 3) to cycle20 (row 21)
  trace.ifast[i,] <- trace.ifast[i-1,] %*% tm.ifast[,,i]
  
}

rownames(trace.ifast) <- paste("cycle", 1:cycles, sep = "_") ## assigning the rownames to highlight each row is 1 cycle run of the markov model
rowSums(trace.ifast) ## check that they sum to 1, if not something has gone wrong in your calculations

tracedata.ifast <- as.data.frame(trace.ifast)
tracedata.ifast$cycle <- 1: cycles
tracedata.ifast$BSI <- tracedata.ifast$H.BSI
tracedata.ifast$stillinhospital <- cohort.size-(tracedata.ifast$I.death+
                                                  tracedata.ifast$J.recovery)
df2 <- select(tracedata.ifast,cycle,stillinhospital,BSI,J.recovery, I.death)
colnames(df2) <- c("cycle","still_in_hospital","BSI", "recovery", "death")
df2 <- melt(df2 ,  id.vars = 'cycle', variable.name = 'variable')

p1<- ggplot(df2, aes(cycle,value)) + 
  geom_line(aes(colour = variable),size=0.5) + xlab("hours") + ylab("proportion") +
  geom_point(aes(colour = variable),size=0.3)+
  scale_color_brewer(name = "state", palette = "Set2") + scale_y_continuous(limits = c(0, 1),expand = c(0,0)) +
  scale_x_continuous(limits = c(0, cycles),expand = c(0,0)) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()
p1

##### HOURS and TYPE of antibiotic use per patient #####

inappropriate <- tracedata.ifast$B.empiricUTIR+tracedata.ifast$C.personRcultureS+
  tracedata.ifast$F.personScultureR
appropriate <- tracedata.ifast$X.empiricUTIS+tracedata.ifast$D.personRcultureR+
  tracedata.ifast$E.personScultureS
ab_use_ifast <- data.frame(inappropriate, appropriate)
ab_use_ifast$cycles <- 1:cycles
ab_use_ifast <- melt(ab_use_ifast ,  id.vars = 'cycles', variable.name = 'variable')

save(ab_use, file="outputs/ab_use_ifast.RData")

##### QUALITY ADJUSTED LIFE HOURS LOST #####
###!! important to note this model looks at decrements not total QALYs
QALH.uti <- params[parameter=="q.uti",value]
QALH.bsi <- params[parameter=="q.bsi",value]
QALH.icu <- params[parameter=="q.icu",value]
### !!! note need to update cost and qalh matrices by hand if change
## name or order of states as currently stands
QALH <-c(rep(QALH.uti,7),QALH.icu, QALH.bsi,0,1)##!!! got to here 
## these are for days in hospital. not including deaths. will add those on later.
lh_ifast <- trace.ifast %*% QALH 

undisc.lh_ifast <- colSums(lh_ifast) ## calculating the total LHs from UTI arm
undisc.ly_hosp_ifast <- undisc.lh_ifast/(24*365)
### no discounted needed as all within 1 year

deaths_ifast <- trace.ifast[cycles,"I.death"] ## take the last value
disc.ly_death_ifast <- deaths_ifast*params[parameter=="qaly.death", value]

qaly.lost_ifast <- disc.ly_death_ifast+undisc.ly_hosp_ifast

#### COST CALCULATIONA####
# undiscounted:
cost_ifast <- trace.ifast %*% c.dmc 
cost_time_ifast <- as.data.frame(cost_ifast)
cost_time_ifast$cycles <- 1:cycles
colnames(cost_time_ifast)<- c("cost","cycles")
cost_time_ifast$cumsumcost <- cumsum(cost_time_ifast$cost) 
### !!! note cost_time is just the recurrent costs not inc upfront costs at time of testing
hospital_cost_ifast <-sum(cost_ifast)

upfront_testingcost_ifast <- cohort.size*c.ifast.U
undisc.cost_ifast<-hospital_cost_ifast+upfront_testingcost_ifast ## calculating the total cost of the UTI arm
#undisc.cost 
### no discounting needed as all one year

#### fill results
results["iFAST","excess_cost"] <- undisc.cost_ifast
results["iFAST","excess_qalyloss"] <- -qaly.lost_ifast

#######**** Results *****#####
#  Cost-effectiveness results
### output table
results[2,"IncrementalCost"] <- results[2,"excess_cost"]-results[1,"excess_cost"]

results[2,"IncrementalQALYQain"] <- (results[2,"excess_qalyloss"]-
                                  results[1,"excess_qalyloss"])

results[2,"icer"] <- results[2,"IncrementalCost"]/results[2,"IncrementalQALYQain"]
results




