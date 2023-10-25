library(dplyr) #Load the package dplyr
library(ggplot2) #Load the package ggplot2
library(reshape2)
library(data.table)


####****PARAMETERS****#####
WTP <- 20000

model <- function(params){######### copied into function from here ################
  
  #### converting to hourly discharge -
  #### p = 1 – [(1 − p) ^(1/n)]
  params[parameter=="p.uti2discharge_other",value := 1-((1-value)^(1/24))]
  params[parameter=="p.bsi2discharge",value := 1-((1-value)^(1/24))]
  
  ### calculate qaly decrements
  q.ruti <- params[parameter=="q.ruti",value]
  q.suti<- params[parameter=="q.suti",value]
  q.nouti<- params[parameter=="q.nouti",value]
  
  q.rutidec <- q.nouti - q.ruti
  q.sutidec <- q.nouti - q.suti
  
  q.bsi <- params[parameter=="q.bsi.dec",value]
  
  q.ruti.H <- q.rutidec/(365*24)
  q.suti.H <- q.sutidec/(365*24)
  q.bsi.H <- q.bsi/(365*24)
  
  ### removed for now
  # ## if q.bsi less than q.ruti set to the same value 
  # if (q.bsi.H < q.ruti.H){
  #   q.bsi.H <- q.ruti.H
  # }
  
  #### setting up state names
  state.names<-c("clinsusUTI","empiricUTIR","empiricUTIS",
                 "personRcultureS","personRcultureR","personScultureS",
                 "personScultureR",
                 "BSI","death","recovery") 
  state.names
  
  #  Set the total number of cycles for the model to run
  cycles <- params[parameter=="n.cycles",value]
  
  #  This shows the probability of transitioning from one state to another 
  n.states <- length(state.names)
  
  
  # # #### defining scenarios for test runs
  # abx <- "base"
  # age <- "1664"
  # sex <- "f"
  # uti <- "uncomplicated"
  
  CEA_model <- function(age, sex, abx, uti){
    
    relevant_dt <- params[(uti_group==uti|uti_group=="all")&
                            (age_group==age|age_group=="all") &    ## weird read-in of age column 
                            (sex_group==sex|sex_group=="all") & 
                            (abx_scenario==abx|abx_scenario=="all")]
    
    #  Seed the starting states of the model (cycle 0)
    cohort.size <- relevant_dt[parameter=="cohort",value]
    
    seed <- c(cohort.size,rep(0,(n.states-1))) ## i.e. everyone starts in State A
    
    
    #### getting the correct transition probabilities out of uti from data 
    R_inapp <- relevant_dt[parameter=="r.prev", value]* 
      relevant_dt[parameter=="prop.inap",value] 
    RS_other <- 1-R_inapp
    
    uti_probdis <- relevant_dt[parameter=="p.uti2discharge_other",value]
    
    new_r_inapp <- uti_probdis /(R_inapp+(RS_other*relevant_dt[parameter=="r.multip.los",value]))
    new_RS_other <- new_r_inapp*relevant_dt[parameter=="r.multip.los",value]
    
    p.uti2discharge_R_inappropriate <- new_r_inapp
    p.uti2discharge_other <-new_RS_other
    
    ### adjusting mortality impacts
    uti_propmort <- relevant_dt[parameter=="prop.uti2die",value]
    
    new_RS_other.m <- uti_propmort /(RS_other+(R_inapp*relevant_dt[parameter=="r.multip.mort",value]))
    new_r_inapp.m <- new_RS_other.m*relevant_dt[parameter=="r.multip.mort",value]
    
    p.uti2die_R <- p.uti2discharge_R_inappropriate*new_r_inapp.m 
    p.uti2well_R <- p.uti2discharge_R_inappropriate*(1-new_r_inapp.m)
    p.uti2die <- p.uti2discharge_other*new_RS_other.m
    p.uti2well <- p.uti2discharge_other*(1-new_RS_other.m)
    
    ### UNIT COSTS #################### 
    ### recurring health state costs
    c.none <- 0  ## where there are no costs 
    
    c.uti <- (relevant_dt[parameter=="cost.bed.icu", value]*    ## weighted average bedday cost
                relevant_dt[parameter=="prop.uti.icu",value])+
      (relevant_dt[parameter=="cost.bed.general", value]*
         (1-relevant_dt[parameter=="prop.uti.icu",value]))
    
    c.bsi <- (relevant_dt[parameter=="cost.bed.icu", value]*
                relevant_dt[parameter=="prop.bsi.icu",value])+
      (relevant_dt[parameter=="cost.bed.general", value]*
         (1-relevant_dt[parameter=="prop.bsi.icu",value]))
    
    ### storing all costs in a vector
    c.dmc <- c(c.none, c.uti, c.uti,c.uti, c.uti, c.uti,
               c.uti, c.bsi,c.none, c.none)
    
    check1 <- length(c.dmc) == n.states ## check all states have a cost
    
    ### one off costs - not ideal for generally modelling
    ### but as no discounting will multiply all cohort/state totals at end
    ### by 1 off test as assumed everyone gets tested - then add at end
    c.standardtest.U <- relevant_dt[parameter=="cost.standardtest.U",value]
    c.ifast.U <-relevant_dt[parameter=="cost.ifast.U",value]
    
    
    c.empiric <- relevant_dt[parameter=="cost.empiricuti.U", value]
    c.2ndline <- relevant_dt[parameter=="cost.2ndline.U", value]
    c.empiric.pre.c <- relevant_dt[parameter=="cost.empiricuti.current", value]
    c.empiric.pre.i <- relevant_dt[parameter=="cost.empiricuti.ifast", value]
    
    ### transition probabilities####
    ### non age/sex dependent
    p.uti2bsi <- relevant_dt[parameter=="p.uti2bsi",value]
    t.current <-   relevant_dt[parameter=="t.current",value]
    t.ifast <-   relevant_dt[parameter=="t.ifast",value]
    
    ### time dependent vector for testing & uti & bsi transitions
    p.testresultcurrent <-  c(rep(0,(t.current-1)),1,rep(0,(cycles-t.current)))
    p.testresultifast <-  c(rep(0,(t.ifast-1)),1,rep(0,(cycles-t.ifast)))
    
    
    p.bsi2discharge <- relevant_dt[age_group==age &    ## weird read-in of age column 
                                     sex==sex & parameter=="p.bsi2discharge",value] 
    prop.bsi2die <-  relevant_dt[age_group==age &    ## weird read-in of age column 
                                   sex==sex & parameter=="prop.bsi2die",value] 
    p.bsi2die <- p.bsi2discharge*prop.bsi2die
    p.bsi2well <- p.bsi2discharge*(1-prop.bsi2die)
    
    testing_info <- relevant_dt[abx_scenario==abx & uti_group==uti &
                                  sex==sex & age_group==age]
    
    prevalence <- testing_info[parameter=="r.prev",value]
    sensitivity.i <- testing_info[parameter=="sensitivity.i",value]
    specificity.i <- testing_info[parameter=="specificity.i",value]
    
    sensitivity.c <- testing_info[parameter=="sensitivity.c",value]
    specificity.c <- testing_info[parameter=="specificity.c",value]
    
    personRcultureR.i <- sensitivity.i
    personRcultureS.i <- (1 - sensitivity.i)
    personScultureR.i <- (1-specificity.i)
    personScultureS.i <- specificity.i
    
    personRcultureR.c <- sensitivity.c
    personRcultureS.c <- (1 - sensitivity.c)
    personScultureR.c <- (1-specificity.c)
    personScultureS.c <- specificity.c
    
    #### transition matrices ####
    
    ### **** Current Arm***** #####
    
    #### create transition matrix first to capture time dependencies
    ### also makes it less likely to make mistake as can assign spefic transitions
    tm.current <- array(data=0,dim=c(n.states, n.states, cycles),
                        dimnames= list(state.names, state.names, 1:cycles))
    
    tm.current["clinsusUTI","empiricUTIR",] <- prevalence
    tm.current["clinsusUTI","empiricUTIS",] <- 1-prevalence
    
    tm.current["empiricUTIR","personRcultureS",] <- p.testresultcurrent*personRcultureS.c
    tm.current["empiricUTIR","personRcultureR",] <- p.testresultcurrent*personRcultureR.c
    tm.current["empiricUTIR","BSI",] <- p.uti2bsi
    tm.current["empiricUTIR","death",] <- p.uti2die_R
    tm.current["empiricUTIR","recovery",] <- p.uti2well_R
    tm.current["empiricUTIR","empiricUTIR",] <- 1-(p.uti2bsi+p.uti2die_R+
                                                     p.uti2well_R+
                                                     (p.testresultcurrent*personRcultureS.c)+
                                                     ( p.testresultcurrent*personRcultureR.c))
    ## !!! note only transition to culture on t.culture
    tm.current["empiricUTIR","BSI",t.current] <- 0
    tm.current["empiricUTIR","death",t.current] <- 0
    tm.current["empiricUTIR","recovery",t.current] <-0 
    tm.current["empiricUTIR","empiricUTIR",t.current] <-0 
    
    tm.current["empiricUTIS","personScultureS",] <- p.testresultcurrent*personScultureS.c
    tm.current["empiricUTIS","personScultureR",] <- p.testresultcurrent*personScultureR.c
    tm.current["empiricUTIS","BSI",] <- p.uti2bsi
    tm.current["empiricUTIS","death",] <- p.uti2die
    tm.current["empiricUTIS","recovery",] <- p.uti2well
    tm.current["empiricUTIS","empiricUTIS",] <- 1-(p.uti2bsi+p.uti2die+
                                                     p.uti2well+
                                                     (p.testresultcurrent*personScultureS.c)+
                                                     (p.testresultcurrent*personScultureR.c))
    ## !!! note only transition to culture on t.culture
    tm.current["empiricUTIS","BSI",t.current] <- 0
    tm.current["empiricUTIS","death",t.current] <- 0
    tm.current["empiricUTIS","recovery",t.current] <-0 
    tm.current["empiricUTIS","empiricUTIS",t.current] <-0 
    
    tm.current["personRcultureS","BSI",] <- p.uti2bsi
    tm.current["personRcultureS","death",] <- p.uti2die_R
    tm.current["personRcultureS","recovery",] <- p.uti2well_R ## person kept on wrong empiric therapy
    tm.current["personRcultureS","personRcultureS",] <- 1-(p.uti2bsi+p.uti2die_R+
                                                             p.uti2well_R)
    tm.current["personRcultureR","BSI",] <- p.uti2bsi
    tm.current["personRcultureR","death",] <- p.uti2die
    tm.current["personRcultureR","recovery",] <- p.uti2well
    tm.current["personRcultureR","personRcultureR",] <- 1-(p.uti2bsi+p.uti2die+
                                                             p.uti2well)
    tm.current["personScultureS","BSI",] <- p.uti2bsi
    tm.current["personScultureS","death",] <- p.uti2die
    tm.current["personScultureS","recovery",] <- p.uti2well
    tm.current["personScultureS","personScultureS",] <- 1-(p.uti2bsi+p.uti2die+
                                                             p.uti2well)
    tm.current["personScultureR","BSI",] <- p.uti2bsi
    tm.current["personScultureR","death",] <- p.uti2die
    tm.current["personScultureR","recovery",] <- p.uti2well
    tm.current["personScultureR","personScultureR",] <- 1-(p.uti2bsi+p.uti2die+
                                                             p.uti2well)
    tm.current["BSI","death",] <- p.bsi2die
    tm.current["BSI","recovery",] <- p.bsi2well
    tm.current["BSI","BSI",] <- 1-(p.bsi2well+p.bsi2die)
    
    tm.current["death","death",] <- 1
    tm.current["recovery","recovery",] <- 1
    
    ### testing adds to 1
    # rowSums(tm.current[,,49])
    check2 <- sum(tm.current)==(cycles*n.states)
    
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
    check3 <- (sum(trace.current))-(cohort.size*cycles) ### why is this not 0? !!come back to 
    
    ##### QUALITY ADJUSTED LIFE HOURS LOST #####
    ###!! important to note this model looks at decrements not total QALYs
    ### !!! note need to update cost and qalh matrices by hand if change
    ## name or order of states as currently stands
    QALH <-c(0,q.ruti.H,q.suti.H,q.ruti.H,q.ruti.H,q.suti.H,q.suti.H,q.bsi.H,0,0)
    ## these are for days in hospital. not including deaths. will add those on later.
    QALH_current <- trace.current %*% QALH 
    
    undisc.QALH_current <- colSums(QALH_current) ## calculating the total LHs from UTI arm
    undisc.QALY_hosp_current <- undisc.QALH_current/(24*365)
    ### no discounted needed as all within 1 year
    
    deaths_current <- trace.current[cycles,"death"] ## take the last value
    disc.QALY_death_current <- deaths_current*relevant_dt[parameter=="qaly.death", value]
    
    qaly.lost_current <- disc.QALY_death_current+undisc.QALY_hosp_current
    
    #### COST CALCULATIONS #####
    
    ## undiscounted:
    cost_orig <- trace.current %*% c.dmc 
    cost_time <- as.data.frame(cost_orig)
    cost_time$cycles <- 1:cycles
    colnames(cost_time)<- c("cost","cycles")
    cost_time$cumsumcost <- cumsum(cost_time$cost) 
    ### !!! note cost_time is just the recurrent costs not inc upfront costs at time of testing
    hospital_cost_rec <-sum(cost_orig)
    
    upfront_testingcost <- cohort.size*c.standardtest.U
    
    ### easier as all happening in 1 year so easier no discounting
    ## probably a more efficient way of embedding into model but for now use results and multiply
    n.empiric <- trace.current[t.current,"personScultureS"]+
      trace.current[t.current,"personRcultureS"]+
      trace.current[t.current,"BSI"] +trace.current[t.current,"death"]+
      trace.current[t.current,"recovery"]
    n.switch <- trace.current[t.current,"personScultureR"]+
      trace.current[t.current,"personRcultureR"]
    
    ## checks
    check4 <-  (n.empiric+n.switch) == cohort.size
    ### seemingly a e-13 difference ? come back to !!
    ## quick fix for now
    n.empiric <- cohort.size - n.switch
    
    c.switch <- (c.empiric.pre.c +c.2ndline)
    
    hosp_empiric <- n.empiric*c.empiric
    hosp_switch <- n.switch*c.switch
    
    undisc.cost_current<-hospital_cost_rec + upfront_testingcost +
      hosp_empiric+hosp_switch
    ## calculating the total cost of the UTI arm
    #undisc.cost 
    ### no discounting needed as all one year
    
    #### fill results
    results <- data.frame(excess_cost=c(0,0),
                          excess_qalyloss = c(0,0))
    rownames(results) <- c("current","iFAST")
    
    results["current","excess_cost"] <- undisc.cost_current
    results["current","excess_qalyloss"] <- qaly.lost_current ##negative if put qalys
    
    ####****iFAST ARM***** ##### 
    
    #### create transition matrix first to capture time dependencies
    ### also makes it less likely to make mistake as can assign spefic transitions
    tm.ifast <- array(data=0,dim=c(n.states, n.states, cycles),
                      dimnames= list(state.names, state.names, 1:cycles))
    
    tm.ifast["clinsusUTI","empiricUTIR",] <- prevalence
    tm.ifast["clinsusUTI","empiricUTIS",] <- 1-prevalence
    
    tm.ifast["empiricUTIR","personRcultureS",] <- p.testresultifast*personRcultureS.i
    tm.ifast["empiricUTIR","personRcultureR",] <- p.testresultifast*personRcultureR.i
    
    tm.ifast["empiricUTIR","BSI",] <- p.uti2bsi
    tm.ifast["empiricUTIR","death",] <- p.uti2die_R
    tm.ifast["empiricUTIR","recovery",] <- p.uti2well_R
    tm.ifast["empiricUTIR","empiricUTIR",] <- 1-(p.uti2bsi+p.uti2die_R+
                                                   p.uti2well_R+
                                                   (p.testresultifast*personRcultureS.i)+
                                                   (p.testresultifast*personRcultureR.i))
    ## !!! note only transition to culture on t.culture
    tm.ifast["empiricUTIR","BSI",t.ifast] <- 0
    tm.ifast["empiricUTIR","death",t.ifast] <- 0
    tm.ifast["empiricUTIR","recovery",t.ifast] <-0 
    tm.ifast["empiricUTIR","empiricUTIR",t.ifast] <-0 
    
    tm.ifast["empiricUTIS","personScultureS",] <- p.testresultifast*personScultureS.i
    tm.ifast["empiricUTIS","personScultureR",] <- p.testresultifast*personScultureR.i
    tm.ifast["empiricUTIS","BSI",] <- p.uti2bsi
    tm.ifast["empiricUTIS","death",] <- p.uti2die
    tm.ifast["empiricUTIS","recovery",] <- p.uti2well
    tm.ifast["empiricUTIS","empiricUTIS",] <- 1-(p.uti2bsi+p.uti2die+
                                                   p.uti2well+
                                                   (p.testresultifast*personScultureS.i)+
                                                   (p.testresultifast*personScultureR.i))
    ## !!! note only transition to culture on t.culture
    tm.ifast["empiricUTIS","BSI",t.ifast] <- 0
    tm.ifast["empiricUTIS","death",t.ifast] <- 0
    tm.ifast["empiricUTIS","recovery",t.ifast] <-0 
    tm.ifast["empiricUTIS","empiricUTIS",t.ifast] <-0 
    
    tm.ifast["personRcultureS","BSI",] <- p.uti2bsi
    tm.ifast["personRcultureS","death",] <- p.uti2die_R
    tm.ifast["personRcultureS","recovery",] <- p.uti2well_R ## person kept on wrong empiric therapy
    tm.ifast["personRcultureS","personRcultureS",] <- 1-(p.uti2bsi+p.uti2die_R+
                                                           p.uti2well_R)
    tm.ifast["personRcultureR","BSI",] <- p.uti2bsi
    tm.ifast["personRcultureR","death",] <- p.uti2die
    tm.ifast["personRcultureR","recovery",] <- p.uti2well
    tm.ifast["personRcultureR","personRcultureR",] <- 1-(p.uti2bsi+p.uti2die+
                                                           p.uti2well)
    tm.ifast["personScultureS","BSI",] <- p.uti2bsi
    tm.ifast["personScultureS","death",] <- p.uti2die
    tm.ifast["personScultureS","recovery",] <- p.uti2well
    tm.ifast["personScultureS","personScultureS",] <- 1-(p.uti2bsi+p.uti2die+
                                                           p.uti2well)
    tm.ifast["personScultureR","BSI",] <- p.uti2bsi
    tm.ifast["personScultureR","death",] <- p.uti2die
    tm.ifast["personScultureR","recovery",] <- p.uti2well
    tm.ifast["personScultureR","personScultureR",] <- 1-(p.uti2bsi+p.uti2die+
                                                           p.uti2well)
    tm.ifast["BSI","death",] <- p.bsi2die
    tm.ifast["BSI","recovery",] <- p.bsi2well
    tm.ifast["BSI","BSI",] <- 1-(p.bsi2well+p.bsi2die)
    
    tm.ifast["death","death",] <- 1
    tm.ifast["recovery","recovery",] <- 1
    
    ### testing adds to 1
    # rowSums(tm.ifast[,,2])
    # rowSums(tm.ifast[,,1])
    check5 <- sum(tm.ifast)==(cycles*n.states)
    
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
    
    
    ##### QUALITY ADJUSTED LIFE HOURS LOST #####
    
    ## these are for days in hospital. not including deaths. will add those on later.
    QALH_ifast <- trace.ifast %*% QALH 
    
    undisc.QALH_ifast <- colSums(QALH_ifast) ## calculating the total LHs from UTI arm
    undisc.QALY_hosp_ifast <- undisc.QALH_ifast/(24*365)
    ### no discounted needed as all within 1 year
    
    deaths_ifast <- trace.ifast[cycles,"death"] ## take the last value
    disc.QALY_death_ifast <- deaths_ifast*relevant_dt[age_group==age &    ## weird read-in of age column 
                                                        sex==sex & parameter=="qaly.death", value]
    
    qaly.lost_ifast <- disc.QALY_death_ifast+undisc.QALY_hosp_ifast
    #### COST CALCULATIONS #####
    
    ## undiscounted:
    cost_orig <- trace.ifast %*% c.dmc 
    cost_time <- as.data.frame(cost_orig)
    cost_time$cycles <- 1:cycles
    colnames(cost_time)<- c("cost","cycles")
    cost_time$cumsumcost <- cumsum(cost_time$cost) 
    ### !!! note cost_time is just the recurrent costs not inc upfront costs at time of testing
    hospital_cost_rec <-sum(cost_orig)
    
    upfront_testingcost <- cohort.size*c.ifast.U
    
    ### easier as all happening in 1 year so easier no discounting
    ## probably a more efficient way of embedding into model but for now use results and multiply
    n.empiric <- trace.ifast[t.ifast,"personScultureS"]+
      trace.ifast[t.ifast,"personRcultureS"]+
      trace.ifast[t.ifast,"BSI"] +trace.ifast[t.ifast,"death"]+
      trace.ifast[t.ifast,"recovery"]
    n.switch <- trace.ifast[t.ifast,"personScultureR"]+
      trace.ifast[t.ifast,"personRcultureR"]
    
    ## checks
    n.empiric+n.switch == cohort.size
    ### seemingly a e-13 rounding difference ? come back to !!
    ## quick fix for now
    n.empiric <- cohort.size - n.switch
    
    c.switch <- (c.empiric.pre.c +c.2ndline)
    
    hosp_empiric <- n.empiric*c.empiric
    hosp_switch <- n.switch*c.switch
    
    undisc.cost_ifast <-hospital_cost_rec+upfront_testingcost +
      hosp_empiric+hosp_switch
    ## calculating the total cost of the UTI arm
    #undisc.cost 
    ### no discounting needed as all one year
    
    #### fill results
    results["iFAST","excess_cost"] <- undisc.cost_ifast
    results["iFAST","excess_qalyloss"] <- qaly.lost_ifast ##negative if put qalys
    
    #######**** Results *****#####
    #  Cost-effectiveness results
    ### output table
    results["iFAST","IncrementalCost"] <- results["iFAST","excess_cost"]-results["current","excess_cost"]
    
    results["iFAST","IncrementalQALYGain"] <- (-results["iFAST","excess_qalyloss"])-
      (-results["current","excess_qalyloss"])
    
    results["iFAST","Net Monetary Benefit"] <- (results["iFAST","IncrementalQALYGain"]*WTP)-
      results["iFAST","IncrementalCost"]
    results
    results$scenario <- paste0(age," ",sex," ",abx," ",uti)
    
    ##### HOURS and TYPE of antibiotic use per patient #####
    
    #old code
   # inappropriate_current <- (trace.current[,"empiricUTIR"])+
  #    (trace.current[,"personRcultureS"])+
  #    (trace.current[,"personScultureR"])
  #  appropriate_current <- (trace.current[,"empiricUTIS"])+
  #    (trace.current[,"personRcultureR"])+
  #    (trace.current[,"personScultureS"])
    
    inappropriate_current <- (trace.current[,"empiricUTIR"])+
      (trace.current[,"personRcultureS"])
      appropriate_current <- (trace.current[,"empiricUTIS"])+
        (trace.current[,"personRcultureR"])+
        (trace.current[,"personScultureS"])
    appropriate_current_other <- (trace.current[,"empiricUTIS"])+
      (trace.current[,"personRcultureR"])+
      (trace.current[,"personScultureS"])+
      (trace.current[,"personScultureR"])
    
    ab_use_current <- data.frame(inappropriate_current, appropriate_current_other)
    ab_use_current$cycles <- 1:cycles
    ab_use_current <- melt(ab_use_current,  id.vars = 'cycles', variable.name = 'variable')
    ab_use_current$intervention <- "current"
    
   # inappropriate_ifast <- (trace.ifast[,"empiricUTIR"])+
  #    (trace.ifast[,"personRcultureS"])+
  #    (trace.ifast[,"personScultureR"])
  #  appropriate_ifast <- (trace.ifast[,"empiricUTIS"])+
  #    (trace.ifast[,"personRcultureR"])+
  #    (trace.ifast[,"personScultureS"])
    
    inappropriate_ifast <- (trace.ifast[,"empiricUTIR"])+
      (trace.ifast[,"personRcultureS"])
    appropriate_ifast <- (trace.ifast[,"empiricUTIS"])+
      (trace.ifast[,"personRcultureR"])+
      (trace.ifast[,"personScultureS"])
    appropriate_ifast_other <- (trace.ifast[,"empiricUTIS"])+
      (trace.ifast[,"personRcultureR"])+
      (trace.ifast[,"personScultureS"])+
      (trace.ifast[,"personScultureR"])
    
    ab_use_ifast <- data.frame(inappropriate_ifast, appropriate_ifast_other)
    ab_use_ifast$cycles <- 1:cycles
    ab_use_ifast <- melt(ab_use_ifast,  id.vars = 'cycles', variable.name = 'variable')
    ab_use_ifast$intervention <- "ifast"
    
    ab_use <- rbind(ab_use_current, ab_use_ifast)
    ab_use$scenario <- paste0(age," ",sex," ",abx," ",uti)
    
    ## rename appropriate/inappropriate now added in intervention column
    ab_use <- as.data.table(ab_use)
    ab_use[variable=="inappropriate_current"|variable=="inappropriate_ifast",
           variable:="inappropriate"]
    ab_use[variable=="appropriate_current_other"|variable=="appropriate_ifast_other",
           variable:="appropriate"]
    
    ##### hospital hours & bsi hours #####
    trace.current <- as.data.frame(trace.current)
    trace.current$cycle <- 1: cycles
    trace.current$stillinhospital <- cohort.size-(trace.current$death+
                                                    trace.current$recovery)
    hospital_outcomes_current <- select(trace.current,cycle,stillinhospital,BSI,recovery, death)
    colnames(hospital_outcomes_current) <- c("cycle","still_in_hospital","BSI", "recovery", "death")
    hospital_outcomes_current<- melt(hospital_outcomes_current,  id.vars = 'cycle', variable.name = 'variable')
    hospital_outcomes_current$intervention <- "current"
    
    trace.ifast <- as.data.frame(trace.ifast)
    trace.ifast$cycle <- 1: cycles
    trace.ifast$stillinhospital <- cohort.size-(trace.ifast$death+
                                                  trace.ifast$recovery)
    hospital_outcomes_ifast <- select(trace.ifast,cycle,stillinhospital,BSI,recovery, death)
    colnames(hospital_outcomes_ifast) <- c("cycle","still_in_hospital","BSI", "recovery", "death")
    hospital_outcomes_ifast<- melt(hospital_outcomes_ifast,  id.vars = 'cycle', variable.name = 'variable')
    hospital_outcomes_ifast$intervention <- "ifast"
    
    hospital_outcomes <- rbind(hospital_outcomes_current, hospital_outcomes_ifast)
    hospital_outcomes$scenario <- paste0(age," ",sex," ",abx," ",uti)
    
    model_outputs <- list(results, ab_use, hospital_outcomes)
    return(model_outputs)
  }
  
  
  ########## RUNNING FOR BASE CASE #################
  f1664unC <- CEA_model("1664","f","base", "uncomplicated")
  m1664unC <- CEA_model("1664","m","base", "uncomplicated")
  f65100unC <- CEA_model("65100","f","base", "uncomplicated")
  m65100unC <- CEA_model("65100","m","base", "uncomplicated")
  
  f1664C_IV <- CEA_model("1664","f","base", "complicated_IV")
  m1664C_IV <- CEA_model("1664","m","base", "complicated_IV")
  f65100C_IV <- CEA_model("65100","f","base", "complicated_IV")
  m65100C_IV <- CEA_model("65100","m","base", "complicated_IV")
  
  f1664C_oral <- CEA_model("1664","f","base", "complicated_oral")
  m1664C_oral <- CEA_model("1664","m","base", "complicated_oral")
  f65100C_oral <- CEA_model("65100","f","base", "complicated_oral")
  m65100C_oral <- CEA_model("65100","m","base", "complicated_oral")
  
  icer.outputs <- rbind(f1664unC[[1]],
                        m1664unC[[1]],
                        f65100unC[[1]],
                        m65100unC[[1]],
                        f1664C_IV[[1]],
                        m1664C_IV[[1]],
                        f65100C_IV[[1]],
                        m65100C_IV[[1]],
                        f1664C_oral[[1]],
                        m1664C_oral[[1]],
                        f65100C_oral[[1]],
                        m65100C_oral[[1]])

  icer.temp <- as.data.table(icer.outputs)
  icer.temp[is.na(IncrementalCost), intervention := "current"]
  icer.temp[!is.na(IncrementalCost), intervention := "iFAST"]
  icer.merged <- icer.temp[ , .(excess_cost  = sum(excess_cost),
            excess_qalyloss =sum(excess_qalyloss)), by = intervention] 
  icer.merged[, IncrementalCost := excess_cost - shift(excess_cost, fill = first(excess_cost))]
  icer.merged[, IncrementalQALYGain := -(excess_qalyloss - shift(excess_qalyloss, 
                                                               fill = first(excess_qalyloss)))]
  icer.merged[ , NetMonetaryBenefit := (IncrementalQALYGain*WTP)-IncrementalCost]

  ### big dataset with trace values
  trace.outputs <- rbind(f1664unC[[3]],
                         m1664unC[[3]],
                         f65100unC[[3]],
                         m65100unC[[3]],
                         f1664C_IV[[3]],
                         m1664C_IV[[3]],
                         f65100C_IV[[3]],
                         m65100C_IV[[3]],
                         f1664C_oral[[3]],
                         m1664C_oral[[3]],
                         f65100C_oral[[3]],
                         m65100C_oral[[3]])
  end.trace <- subset(trace.outputs,cycle==10000)
  
  end.trace <- as.data.table(end.trace)
  
  end.trace <- end.trace[ , .(value=sum(value)), by = c("intervention",
                                                        "variable")]
  
  ### death difference
  death.diff <- end.trace[variable=="death"&intervention=="current",value]-
    end.trace[variable=="death"&intervention=="ifast",value]
  
  ## for plots 
  hosp.merged <- as.data.table(trace.outputs)
  hosp.merged <- hosp.merged[ , .(value=sum(value)), by = c("cycle",
                                                            "intervention",
                                                            "variable")]
  
  ### big dataset with ab use values
  ab.outputs <- rbind(f1664unC[[2]],
                      m1664unC[[2]],
                      f65100unC[[2]],
                      m65100unC[[2]],
                      f1664C_IV[[2]],
                      m1664C_IV[[2]],
                      f65100C_IV[[2]],
                      m65100C_IV[[2]],
                      f1664C_oral[[2]],
                      m1664C_oral[[2]],
                      f65100C_oral[[2]],
                      m65100C_oral[[2]])
  
  ab.outputs <- as.data.table(ab.outputs)
  data.merged <- ab.outputs[ , .(value=sum(value)), by = c("cycles",
                                                           "intervention",
                                                           "variable")]
  
  data.merged[ , variable_combi := paste0(variable,"_",intervention)]
  
  #total antibiotic hours
  abhours <- data.frame(inappropriate_current = sum(data.merged$value[1:cycles]),
                        appropriate_current  = sum(data.merged$value[(cycles+1):(cycles*2)]),
                        inappropriate_ifast = sum(data.merged$value[((cycles*2)+1):(cycles*3)]),
                        appropriate_ifast  = sum(data.merged$value[((cycles*3)+1):(cycles*4)])
  )
  
  abdays <- abhours/24 #to give days
  
 

#### all outputs
outputs <- list(icer.outputs,
                icer.merged,
                trace.outputs,
                end.trace,
                death.diff, #5
                hosp.merged,
                ab.outputs,
                data.merged,
                abhours,
                abdays #10
                )

return(outputs)

}


model_trim <- function(params){######### copied into function from here ################
  
  #### converting to hourly discharge -
  #### p = 1 – [(1 − p) ^(1/n)]
  params[parameter=="p.uti2discharge_other",value := 1-((1-value)^(1/24))]
  params[parameter=="p.bsi2discharge",value := 1-((1-value)^(1/24))]
  
  ### calculate qaly decrements
  q.ruti <- params[parameter=="q.ruti",value]
  q.suti<- params[parameter=="q.suti",value]
  q.nouti<- params[parameter=="q.nouti",value]
  
  q.rutidec <- q.nouti - q.ruti
  q.sutidec <- q.nouti - q.suti
  
  q.bsi <- params[parameter=="q.bsi.dec",value]
  
  q.ruti.H <- q.rutidec/(365*24)
  q.suti.H <- q.sutidec/(365*24)
  q.bsi.H <- q.bsi/(365*24)
  
  ### removed for now
  # ## if q.bsi less than q.ruti set to the same value 
  # if (q.bsi.H < q.ruti.H){
  #   q.bsi.H <- q.ruti.H
  # }
  
  #### setting up state names
  state.names<-c("clinsusUTI","empiricUTIR","empiricUTIS",
                 "personRcultureS","personRcultureR","personScultureS",
                 "personScultureR",
                 "BSI","death","recovery") 
  state.names
  
  #  Set the total number of cycles for the model to run
  cycles <- params[parameter=="n.cycles",value]
  
  #  This shows the probability of transitioning from one state to another 
  n.states <- length(state.names)
  
  
  # # #### defining scenarios for test runs
  # abx <- "base"
  # age <- "1664"
  # sex <- "f"
  # uti <- "uncomplicated"
  
  CEA_model <- function(age, sex, abx, uti){
    
    relevant_dt <- params[(uti_group==uti|uti_group=="all")&
                            (age_group==age|age_group=="all") &    ## weird read-in of age column 
                            (sex_group==sex|sex_group=="all") & 
                            (abx_scenario==abx|abx_scenario=="all")]
    
    #  Seed the starting states of the model (cycle 0)
    cohort.size <- relevant_dt[parameter=="cohort",value]
    
    seed <- c(cohort.size,rep(0,(n.states-1))) ## i.e. everyone starts in State A
    
    
    #### getting the correct transition probabilities out of uti from data 
    R_inapp <- relevant_dt[parameter=="r.prev", value]* 
      relevant_dt[parameter=="prop.inap",value] 
    RS_other <- 1-R_inapp
    
    uti_probdis <- relevant_dt[parameter=="p.uti2discharge_other",value]
    
    new_r_inapp <- uti_probdis /(R_inapp+(RS_other*relevant_dt[parameter=="r.multip.los",value]))
    new_RS_other <- new_r_inapp*relevant_dt[parameter=="r.multip.los",value]
    
    p.uti2discharge_R_inappropriate <- new_r_inapp
    p.uti2discharge_other <-new_RS_other
    
    ### adjusting mortality impacts
    uti_propmort <- relevant_dt[parameter=="prop.uti2die",value]
    
    new_RS_other.m <- uti_propmort /(RS_other+(R_inapp*relevant_dt[parameter=="r.multip.mort",value]))
    new_r_inapp.m <- new_RS_other.m*relevant_dt[parameter=="r.multip.mort",value]
    
    p.uti2die_R <- p.uti2discharge_R_inappropriate*new_r_inapp.m 
    p.uti2well_R <- p.uti2discharge_R_inappropriate*(1-new_r_inapp.m)
    p.uti2die <- p.uti2discharge_other*new_RS_other.m
    p.uti2well <- p.uti2discharge_other*(1-new_RS_other.m)
    
    ### UNIT COSTS #################### 
    ### recurring health state costs
    c.none <- 0  ## where there are no costs 
    
    c.uti <- (relevant_dt[parameter=="cost.bed.icu", value]*    ## weighted average bedday cost
                relevant_dt[parameter=="prop.uti.icu",value])+
      (relevant_dt[parameter=="cost.bed.general", value]*
         (1-relevant_dt[parameter=="prop.uti.icu",value]))
    
    c.bsi <- (relevant_dt[parameter=="cost.bed.icu", value]*
                relevant_dt[parameter=="prop.bsi.icu",value])+
      (relevant_dt[parameter=="cost.bed.general", value]*
         (1-relevant_dt[parameter=="prop.bsi.icu",value]))
    
    ### storing all costs in a vector
    c.dmc <- c(c.none, c.uti, c.uti,c.uti, c.uti, c.uti,
               c.uti, c.bsi,c.none, c.none)
    
    check1 <- length(c.dmc) == n.states ## check all states have a cost
    
    ### one off costs - not ideal for generally modelling
    ### but as no discounting will multiply all cohort/state totals at end
    ### by 1 off test as assumed everyone gets tested - then add at end
    c.standardtest.U <- relevant_dt[parameter=="cost.standardtest.U",value]
    c.ifast.U <-relevant_dt[parameter=="cost.ifast.U",value]
    
    
    c.empiric <- relevant_dt[parameter=="cost.empiricuti.U", value]
    c.2ndline <- relevant_dt[parameter=="cost.2ndline.U", value]
    c.empiric.pre.c <- relevant_dt[parameter=="cost.empiricuti.current", value]
    c.empiric.pre.i <- relevant_dt[parameter=="cost.empiricuti.ifast", value]
    
    ### transition probabilities####
    ### non age/sex dependent
    p.uti2bsi <- relevant_dt[parameter=="p.uti2bsi",value]
    t.current <-   relevant_dt[parameter=="t.current",value]
    t.ifast <-   relevant_dt[parameter=="t.ifast",value]
    
    ### time dependent vector for testing & uti & bsi transitions
    p.testresultcurrent <-  c(rep(0,(t.current-1)),1,rep(0,(cycles-t.current)))
    p.testresultifast <-  c(rep(0,(t.ifast-1)),1,rep(0,(cycles-t.ifast)))
    
    
    p.bsi2discharge <- relevant_dt[age_group==age &    ## weird read-in of age column 
                                     sex==sex & parameter=="p.bsi2discharge",value] 
    prop.bsi2die <-  relevant_dt[age_group==age &    ## weird read-in of age column 
                                   sex==sex & parameter=="prop.bsi2die",value] 
    p.bsi2die <- p.bsi2discharge*prop.bsi2die
    p.bsi2well <- p.bsi2discharge*(1-prop.bsi2die)
    
    testing_info <- relevant_dt[abx_scenario==abx & uti_group==uti &
                                  sex==sex & age_group==age]
    
    prevalence <- testing_info[parameter=="r.prev",value]
    sensitivity.i <- testing_info[parameter=="sensitivity.i",value]
    specificity.i <- testing_info[parameter=="specificity.i",value]
    
    sensitivity.c <- testing_info[parameter=="sensitivity.c",value]
    specificity.c <- testing_info[parameter=="specificity.c",value]
    
    personRcultureR.i <- sensitivity.i
    personRcultureS.i <- (1 - sensitivity.i)
    personScultureR.i <- (1-specificity.i)
    personScultureS.i <- specificity.i
    
    personRcultureR.c <- sensitivity.c
    personRcultureS.c <- (1 - sensitivity.c)
    personScultureR.c <- (1-specificity.c)
    personScultureS.c <- specificity.c
    
    #### transition matrices ####
    
    ### **** Current Arm***** #####
    
    #### create transition matrix first to capture time dependencies
    ### also makes it less likely to make mistake as can assign spefic transitions
    tm.current <- array(data=0,dim=c(n.states, n.states, cycles),
                        dimnames= list(state.names, state.names, 1:cycles))
    
    tm.current["clinsusUTI","empiricUTIR",] <- prevalence
    tm.current["clinsusUTI","empiricUTIS",] <- 1-prevalence
    
    tm.current["empiricUTIR","personRcultureS",] <- p.testresultcurrent*personRcultureS.c
    tm.current["empiricUTIR","personRcultureR",] <- p.testresultcurrent*personRcultureR.c
    tm.current["empiricUTIR","BSI",] <- p.uti2bsi
    tm.current["empiricUTIR","death",] <- p.uti2die_R
    tm.current["empiricUTIR","recovery",] <- p.uti2well_R
    tm.current["empiricUTIR","empiricUTIR",] <- 1-(p.uti2bsi+p.uti2die_R+
                                                     p.uti2well_R+
                                                     (p.testresultcurrent*personRcultureS.c)+
                                                     ( p.testresultcurrent*personRcultureR.c))
    ## !!! note only transition to culture on t.culture
    tm.current["empiricUTIR","BSI",t.current] <- 0
    tm.current["empiricUTIR","death",t.current] <- 0
    tm.current["empiricUTIR","recovery",t.current] <-0 
    tm.current["empiricUTIR","empiricUTIR",t.current] <-0 
    
    tm.current["empiricUTIS","personScultureS",] <- p.testresultcurrent*personScultureS.c
    tm.current["empiricUTIS","personScultureR",] <- p.testresultcurrent*personScultureR.c
    tm.current["empiricUTIS","BSI",] <- p.uti2bsi
    tm.current["empiricUTIS","death",] <- p.uti2die
    tm.current["empiricUTIS","recovery",] <- p.uti2well
    tm.current["empiricUTIS","empiricUTIS",] <- 1-(p.uti2bsi+p.uti2die+
                                                     p.uti2well+
                                                     (p.testresultcurrent*personScultureS.c)+
                                                     (p.testresultcurrent*personScultureR.c))
    ## !!! note only transition to culture on t.culture
    tm.current["empiricUTIS","BSI",t.current] <- 0
    tm.current["empiricUTIS","death",t.current] <- 0
    tm.current["empiricUTIS","recovery",t.current] <-0 
    tm.current["empiricUTIS","empiricUTIS",t.current] <-0 
    
    tm.current["personRcultureS","BSI",] <- p.uti2bsi
    tm.current["personRcultureS","death",] <- p.uti2die_R
    tm.current["personRcultureS","recovery",] <- p.uti2well_R ## person kept on wrong empiric therapy
    tm.current["personRcultureS","personRcultureS",] <- 1-(p.uti2bsi+p.uti2die_R+
                                                             p.uti2well_R)
    tm.current["personRcultureR","BSI",] <- p.uti2bsi
    tm.current["personRcultureR","death",] <- p.uti2die
    tm.current["personRcultureR","recovery",] <- p.uti2well
    tm.current["personRcultureR","personRcultureR",] <- 1-(p.uti2bsi+p.uti2die+
                                                             p.uti2well)
    tm.current["personScultureS","BSI",] <- p.uti2bsi
    tm.current["personScultureS","death",] <- p.uti2die
    tm.current["personScultureS","recovery",] <- p.uti2well
    tm.current["personScultureS","personScultureS",] <- 1-(p.uti2bsi+p.uti2die+
                                                             p.uti2well)
    tm.current["personScultureR","BSI",] <- p.uti2bsi
    tm.current["personScultureR","death",] <- p.uti2die
    tm.current["personScultureR","recovery",] <- p.uti2well
    tm.current["personScultureR","personScultureR",] <- 1-(p.uti2bsi+p.uti2die+
                                                             p.uti2well)
    tm.current["BSI","death",] <- p.bsi2die
    tm.current["BSI","recovery",] <- p.bsi2well
    tm.current["BSI","BSI",] <- 1-(p.bsi2well+p.bsi2die)
    
    tm.current["death","death",] <- 1
    tm.current["recovery","recovery",] <- 1
    
    ### testing adds to 1
    # rowSums(tm.current[,,49])
    check2 <- sum(tm.current)==(cycles*n.states)
   # print(check2)
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
    check3 <- (sum(trace.current))-(cohort.size*cycles) ### why is this not 0? !!come back to 
    
    ##### QUALITY ADJUSTED LIFE HOURS LOST #####
    ###!! important to note this model looks at decrements not total QALYs
    ### !!! note need to update cost and qalh matrices by hand if change
    ## name or order of states as currently stands
    QALH <-c(0,q.ruti.H,q.suti.H,q.ruti.H,q.ruti.H,q.suti.H,q.suti.H,q.bsi.H,0,0)
    ## these are for days in hospital. not including deaths. will add those on later.
    QALH_current <- trace.current %*% QALH 
    
    undisc.QALH_current <- colSums(QALH_current) ## calculating the total LHs from UTI arm
    undisc.QALY_hosp_current <- undisc.QALH_current/(24*365)
    ### no discounted needed as all within 1 year
    
    deaths_current <- trace.current[cycles,"death"] ## take the last value
    disc.QALY_death_current <- deaths_current*relevant_dt[parameter=="qaly.death", value]
    
    qaly.lost_current <- disc.QALY_death_current+undisc.QALY_hosp_current
    
    #### COST CALCULATIONS #####
    
    ## undiscounted:
    cost_orig <- trace.current %*% c.dmc 
    cost_time <- as.data.frame(cost_orig)
    cost_time$cycles <- 1:cycles
    colnames(cost_time)<- c("cost","cycles")
    cost_time$cumsumcost <- cumsum(cost_time$cost) 
    ### !!! note cost_time is just the recurrent costs not inc upfront costs at time of testing
    hospital_cost_rec <-sum(cost_orig)
    
    upfront_testingcost <- cohort.size*c.standardtest.U
    
    ### easier as all happening in 1 year so easier no discounting
    ## probably a more efficient way of embedding into model but for now use results and multiply
    n.empiric <- trace.current[t.current,"personScultureS"]+
      trace.current[t.current,"personRcultureS"]+
      trace.current[t.current,"BSI"] +trace.current[t.current,"death"]+
      trace.current[t.current,"recovery"]
    n.switch <- trace.current[t.current,"personScultureR"]+
      trace.current[t.current,"personRcultureR"]
    
    ## checks
    check4 <-  (n.empiric+n.switch) == cohort.size
    ### seemingly a e-13 difference ? come back to !!
    ## quick fix for now
    n.empiric <- cohort.size - n.switch
    
    c.switch <- (c.empiric.pre.c +c.2ndline)
    
    hosp_empiric <- n.empiric*c.empiric
    hosp_switch <- n.switch*c.switch
    
    undisc.cost_current<-hospital_cost_rec + upfront_testingcost +
      hosp_empiric+hosp_switch
    ## calculating the total cost of the UTI arm
    #undisc.cost 
    ### no discounting needed as all one year
    
    #### fill results
    results <- data.frame(excess_cost=c(0,0),
                          excess_qalyloss = c(0,0))
    rownames(results) <- c("current","iFAST")
    
    results["current","excess_cost"] <- undisc.cost_current
    results["current","excess_qalyloss"] <- qaly.lost_current ##negative if put qalys
    
    ####****iFAST ARM***** ##### 
    
    #### create transition matrix first to capture time dependencies
    ### also makes it less likely to make mistake as can assign spefic transitions
    tm.ifast <- array(data=0,dim=c(n.states, n.states, cycles),
                      dimnames= list(state.names, state.names, 1:cycles))
    
    tm.ifast["clinsusUTI","empiricUTIR",] <- prevalence
    tm.ifast["clinsusUTI","empiricUTIS",] <- 1-prevalence
    
    tm.ifast["empiricUTIR","personRcultureS",] <- p.testresultifast*personRcultureS.i
    tm.ifast["empiricUTIR","personRcultureR",] <- p.testresultifast*personRcultureR.i
    
    tm.ifast["empiricUTIR","BSI",] <- p.uti2bsi
    tm.ifast["empiricUTIR","death",] <- p.uti2die_R
    tm.ifast["empiricUTIR","recovery",] <- p.uti2well_R
    tm.ifast["empiricUTIR","empiricUTIR",] <- 1-(p.uti2bsi+p.uti2die_R+
                                                   p.uti2well_R+
                                                   (p.testresultifast*personRcultureS.i)+
                                                   (p.testresultifast*personRcultureR.i))
    ## !!! note only transition to culture on t.culture
    tm.ifast["empiricUTIR","BSI",t.ifast] <- 0
    tm.ifast["empiricUTIR","death",t.ifast] <- 0
    tm.ifast["empiricUTIR","recovery",t.ifast] <-0 
    tm.ifast["empiricUTIR","empiricUTIR",t.ifast] <-0 
    
    tm.ifast["empiricUTIS","personScultureS",] <- p.testresultifast*personScultureS.i
    tm.ifast["empiricUTIS","personScultureR",] <- p.testresultifast*personScultureR.i
    tm.ifast["empiricUTIS","BSI",] <- p.uti2bsi
    tm.ifast["empiricUTIS","death",] <- p.uti2die
    tm.ifast["empiricUTIS","recovery",] <- p.uti2well
    tm.ifast["empiricUTIS","empiricUTIS",] <- 1-(p.uti2bsi+p.uti2die+
                                                   p.uti2well+
                                                   (p.testresultifast*personScultureS.i)+
                                                   (p.testresultifast*personScultureR.i))
    ## !!! note only transition to culture on t.culture
    tm.ifast["empiricUTIS","BSI",t.ifast] <- 0
    tm.ifast["empiricUTIS","death",t.ifast] <- 0
    tm.ifast["empiricUTIS","recovery",t.ifast] <-0 
    tm.ifast["empiricUTIS","empiricUTIS",t.ifast] <-0 
    
    tm.ifast["personRcultureS","BSI",] <- p.uti2bsi
    tm.ifast["personRcultureS","death",] <- p.uti2die_R
    tm.ifast["personRcultureS","recovery",] <- p.uti2well_R ## person kept on wrong empiric therapy
    tm.ifast["personRcultureS","personRcultureS",] <- 1-(p.uti2bsi+p.uti2die_R+
                                                           p.uti2well_R)
    tm.ifast["personRcultureR","BSI",] <- p.uti2bsi
    tm.ifast["personRcultureR","death",] <- p.uti2die
    tm.ifast["personRcultureR","recovery",] <- p.uti2well
    tm.ifast["personRcultureR","personRcultureR",] <- 1-(p.uti2bsi+p.uti2die+
                                                           p.uti2well)
    tm.ifast["personScultureS","BSI",] <- p.uti2bsi
    tm.ifast["personScultureS","death",] <- p.uti2die
    tm.ifast["personScultureS","recovery",] <- p.uti2well
    tm.ifast["personScultureS","personScultureS",] <- 1-(p.uti2bsi+p.uti2die+
                                                           p.uti2well)
    tm.ifast["personScultureR","BSI",] <- p.uti2bsi
    tm.ifast["personScultureR","death",] <- p.uti2die
    tm.ifast["personScultureR","recovery",] <- p.uti2well
    tm.ifast["personScultureR","personScultureR",] <- 1-(p.uti2bsi+p.uti2die+
                                                           p.uti2well)
    tm.ifast["BSI","death",] <- p.bsi2die
    tm.ifast["BSI","recovery",] <- p.bsi2well
    tm.ifast["BSI","BSI",] <- 1-(p.bsi2well+p.bsi2die)
    
    tm.ifast["death","death",] <- 1
    tm.ifast["recovery","recovery",] <- 1
    
    ### testing adds to 1
    # rowSums(tm.ifast[,,2])
    # rowSums(tm.ifast[,,1])
    check5 <- sum(tm.ifast)==(cycles*n.states)
    
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
    
    
    ##### QUALITY ADJUSTED LIFE HOURS LOST #####
    
    ## these are for days in hospital. not including deaths. will add those on later.
    QALH_ifast <- trace.ifast %*% QALH 
    
    undisc.QALH_ifast <- colSums(QALH_ifast) ## calculating the total LHs from UTI arm
    undisc.QALY_hosp_ifast <- undisc.QALH_ifast/(24*365)
    ### no discounted needed as all within 1 year
    
    deaths_ifast <- trace.ifast[cycles,"death"] ## take the last value
    disc.QALY_death_ifast <- deaths_ifast*relevant_dt[age_group==age &    ## weird read-in of age column 
                                                        sex==sex & parameter=="qaly.death", value]
    
    qaly.lost_ifast <- disc.QALY_death_ifast+undisc.QALY_hosp_ifast
    #### COST CALCULATIONS #####
    
    ## undiscounted:
    cost_orig <- trace.ifast %*% c.dmc 
    cost_time <- as.data.frame(cost_orig)
    cost_time$cycles <- 1:cycles
    colnames(cost_time)<- c("cost","cycles")
    cost_time$cumsumcost <- cumsum(cost_time$cost) 
    ### !!! note cost_time is just the recurrent costs not inc upfront costs at time of testing
    hospital_cost_rec <-sum(cost_orig)
    
    upfront_testingcost <- cohort.size*c.ifast.U
    
    ### easier as all happening in 1 year so easier no discounting
    ## probably a more efficient way of embedding into model but for now use results and multiply
    n.empiric <- trace.ifast[t.ifast,"personScultureS"]+
      trace.ifast[t.ifast,"personRcultureS"]+
      trace.ifast[t.ifast,"BSI"] +trace.ifast[t.ifast,"death"]+
      trace.ifast[t.ifast,"recovery"]
    n.switch <- trace.ifast[t.ifast,"personScultureR"]+
      trace.ifast[t.ifast,"personRcultureR"]
    
    ## checks
    n.empiric+n.switch == cohort.size
    ### seemingly a e-13 rounding difference ? come back to !!
    ## quick fix for now
    n.empiric <- cohort.size - n.switch
    
    c.switch <- (c.empiric.pre.c +c.2ndline)
    
    hosp_empiric <- n.empiric*c.empiric
    hosp_switch <- n.switch*c.switch
    
    undisc.cost_ifast <-hospital_cost_rec+upfront_testingcost +
      hosp_empiric+hosp_switch
    ## calculating the total cost of the UTI arm
    #undisc.cost 
    ### no discounting needed as all one year
    
    #### fill results
    results["iFAST","excess_cost"] <- undisc.cost_ifast
    results["iFAST","excess_qalyloss"] <- qaly.lost_ifast ##negative if put qalys
    
    #######**** Results *****#####
    #  Cost-effectiveness results
    ### output table
    results["iFAST","IncrementalCost"] <- results["iFAST","excess_cost"]-results["current","excess_cost"]
    
    results["iFAST","IncrementalQALYGain"] <- (-results["iFAST","excess_qalyloss"])-
      (-results["current","excess_qalyloss"])
    
    results["iFAST","Net Monetary Benefit"] <- (results["iFAST","IncrementalQALYGain"]*WTP)-
      results["iFAST","IncrementalCost"]
    results
    results$scenario <- paste0(age," ",sex," ",abx," ",uti)
    
    ##### HOURS and TYPE of antibiotic use per patient #####
    
    #old code
    # inappropriate_current <- (trace.current[,"empiricUTIR"])+
    #    (trace.current[,"personRcultureS"])+
    #    (trace.current[,"personScultureR"])
    #  appropriate_current <- (trace.current[,"empiricUTIS"])+
    #    (trace.current[,"personRcultureR"])+
    #    (trace.current[,"personScultureS"])
    
    inappropriate_current <- (trace.current[,"empiricUTIR"])+
      (trace.current[,"personRcultureS"])
    appropriate_current <- (trace.current[,"empiricUTIS"])+
      (trace.current[,"personRcultureR"])+
      (trace.current[,"personScultureS"])
    appropriate_current_other <- (trace.current[,"empiricUTIS"])+
      (trace.current[,"personRcultureR"])+
      (trace.current[,"personScultureS"])+
      (trace.current[,"personScultureR"])
    
    ab_use_current <- data.frame(inappropriate_current, appropriate_current_other)
    ab_use_current$cycles <- 1:cycles
    ab_use_current <- melt(ab_use_current,  id.vars = 'cycles', variable.name = 'variable')
    ab_use_current$intervention <- "current"
    
    # inappropriate_ifast <- (trace.ifast[,"empiricUTIR"])+
    #    (trace.ifast[,"personRcultureS"])+
    #    (trace.ifast[,"personScultureR"])
    #  appropriate_ifast <- (trace.ifast[,"empiricUTIS"])+
    #    (trace.ifast[,"personRcultureR"])+
    #    (trace.ifast[,"personScultureS"])
    
    inappropriate_ifast <- (trace.ifast[,"empiricUTIR"])+
      (trace.ifast[,"personRcultureS"])
    appropriate_ifast <- (trace.ifast[,"empiricUTIS"])+
      (trace.ifast[,"personRcultureR"])+
      (trace.ifast[,"personScultureS"])
    appropriate_ifast_other <- (trace.ifast[,"empiricUTIS"])+
      (trace.ifast[,"personRcultureR"])+
      (trace.ifast[,"personScultureS"])+
      (trace.ifast[,"personScultureR"])
    
    ab_use_ifast <- data.frame(inappropriate_ifast, appropriate_ifast_other)
    ab_use_ifast$cycles <- 1:cycles
    ab_use_ifast <- melt(ab_use_ifast,  id.vars = 'cycles', variable.name = 'variable')
    ab_use_ifast$intervention <- "ifast"
    
    ab_use <- rbind(ab_use_current, ab_use_ifast)
    ab_use$scenario <- paste0(age," ",sex," ",abx," ",uti)
    
    ## rename appropriate/inappropriate now added in intervention column
    ab_use <- as.data.table(ab_use)
    ab_use[variable=="inappropriate_current"|variable=="inappropriate_ifast",
           variable:="inappropriate"]
    ab_use[variable=="appropriate_current_other"|variable=="appropriate_ifast_other",
           variable:="appropriate"]
    
    ##### hospital hours & bsi hours #####
    trace.current <- as.data.frame(trace.current)
    trace.current$cycle <- 1: cycles
    trace.current$stillinhospital <- cohort.size-(trace.current$death+
                                                    trace.current$recovery)
    hospital_outcomes_current <- select(trace.current,cycle,stillinhospital,BSI,recovery, death)
    colnames(hospital_outcomes_current) <- c("cycle","still_in_hospital","BSI", "recovery", "death")
    hospital_outcomes_current<- melt(hospital_outcomes_current,  id.vars = 'cycle', variable.name = 'variable')
    hospital_outcomes_current$intervention <- "current"
    
    trace.ifast <- as.data.frame(trace.ifast)
    trace.ifast$cycle <- 1: cycles
    trace.ifast$stillinhospital <- cohort.size-(trace.ifast$death+
                                                  trace.ifast$recovery)
    hospital_outcomes_ifast <- select(trace.ifast,cycle,stillinhospital,BSI,recovery, death)
    colnames(hospital_outcomes_ifast) <- c("cycle","still_in_hospital","BSI", "recovery", "death")
    hospital_outcomes_ifast<- melt(hospital_outcomes_ifast,  id.vars = 'cycle', variable.name = 'variable')
    hospital_outcomes_ifast$intervention <- "ifast"
    
    hospital_outcomes <- rbind(hospital_outcomes_current, hospital_outcomes_ifast)
    hospital_outcomes$scenario <- paste0(age," ",sex," ",abx," ",uti)
    
    model_outputs <- list(results, ab_use, hospital_outcomes)
    return(model_outputs)
  }
  
  
  ########## RUNNING FOR BASE CASE #################
  f1664unC <- CEA_model("1664","f","low_trim", "uncomplicated")
  m1664unC <- CEA_model("1664","m","low_trim", "uncomplicated")
  f65100unC <- CEA_model("65100","f","low_trim", "uncomplicated")
  m65100unC <- CEA_model("65100","m","low_trim", "uncomplicated")
  
  f1664C_IV <- CEA_model("1664","f","low_trim", "complicated_IV")
  m1664C_IV <- CEA_model("1664","m","low_trim", "complicated_IV")
  f65100C_IV <- CEA_model("65100","f","low_trim", "complicated_IV")
  m65100C_IV <- CEA_model("65100","m","low_trim", "complicated_IV")
  
  f1664C_oral <- CEA_model("1664","f","low_trim", "complicated_oral")
  m1664C_oral <- CEA_model("1664","m","low_trim", "complicated_oral")
  f65100C_oral <- CEA_model("65100","f","low_trim", "complicated_oral")
  m65100C_oral <- CEA_model("65100","m","low_trim", "complicated_oral")
  
  icer.outputs <- rbind(f1664unC[[1]],
                        m1664unC[[1]],
                        f65100unC[[1]],
                        m65100unC[[1]],
                        f1664C_IV[[1]],
                        m1664C_IV[[1]],
                        f65100C_IV[[1]],
                        m65100C_IV[[1]],
                        f1664C_oral[[1]],
                        m1664C_oral[[1]],
                        f65100C_oral[[1]],
                        m65100C_oral[[1]])
  
  icer.temp <- as.data.table(icer.outputs)
  icer.temp[is.na(IncrementalCost), intervention := "current"]
  icer.temp[!is.na(IncrementalCost), intervention := "iFAST"]
  icer.merged <- icer.temp[ , .(excess_cost  = sum(excess_cost),
                                excess_qalyloss =sum(excess_qalyloss)), by = intervention] 
  icer.merged[, IncrementalCost := excess_cost - shift(excess_cost, fill = first(excess_cost))]
  icer.merged[, IncrementalQALYGain := -(excess_qalyloss - shift(excess_qalyloss, 
                                                                 fill = first(excess_qalyloss)))]
  icer.merged[ , NetMonetaryBenefit := (IncrementalQALYGain*WTP)-IncrementalCost]
  
  ### big dataset with trace values
  trace.outputs <- rbind(f1664unC[[3]],
                         m1664unC[[3]],
                         f65100unC[[3]],
                         m65100unC[[3]],
                         f1664C_IV[[3]],
                         m1664C_IV[[3]],
                         f65100C_IV[[3]],
                         m65100C_IV[[3]],
                         f1664C_oral[[3]],
                         m1664C_oral[[3]],
                         f65100C_oral[[3]],
                         m65100C_oral[[3]])
  end.trace <- subset(trace.outputs,cycle==10000)
  
  end.trace <- as.data.table(end.trace)
  
  end.trace <- end.trace[ , .(value=sum(value)), by = c("intervention",
                                                        "variable")]
  
  ### death difference
  death.diff <- end.trace[variable=="death"&intervention=="current",value]-
    end.trace[variable=="death"&intervention=="ifast",value]
  
  ## for plots 
  hosp.merged <- as.data.table(trace.outputs)
  hosp.merged <- hosp.merged[ , .(value=sum(value)), by = c("cycle",
                                                            "intervention",
                                                            "variable")]
  
  ### big dataset with ab use values
  ab.outputs <- rbind(f1664unC[[2]],
                      m1664unC[[2]],
                      f65100unC[[2]],
                      m65100unC[[2]],
                      f1664C_IV[[2]],
                      m1664C_IV[[2]],
                      f65100C_IV[[2]],
                      m65100C_IV[[2]],
                      f1664C_oral[[2]],
                      m1664C_oral[[2]],
                      f65100C_oral[[2]],
                      m65100C_oral[[2]])
  
  ab.outputs <- as.data.table(ab.outputs)
  data.merged <- ab.outputs[ , .(value=sum(value)), by = c("cycles",
                                                           "intervention",
                                                           "variable")]
  
  data.merged[ , variable_combi := paste0(variable,"_",intervention)]
  
  ##########
  #total antibiotic hours
  abhours <- data.frame(inappropriate_current = sum(data.merged$value[1:cycles]),
                        appropriate_current  = sum(data.merged$value[(cycles+1):(cycles*2)]),
                        inappropriate_ifast = sum(data.merged$value[((cycles*2)+1):(cycles*3)]),
                        appropriate_ifast  = sum(data.merged$value[((cycles*3)+1):(cycles*4)])
  )
  
  abdays <- abhours/24 #to give days
  
  
  #### all outputs
  outputs <- list(icer.outputs,
                  icer.merged,
                  trace.outputs,
                  end.trace,
                  death.diff,
                  hosp.merged,
                  ab.outputs,
                  data.merged,
                  abhours,
                  abdays
  )
  
  return(outputs)
  
}