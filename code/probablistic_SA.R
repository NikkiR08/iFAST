source("code/iFAST_2023_model_function.R")

# > unique(params$distribution)
# [1] NA           "uniform"    "beta"       "normal.se"  "gamma"     

####### probabilistic parameters ###########
params.all <- read.csv("inputs/ifast_inputparameters.csv")
params.all <- as.data.table(params.all)

params.all[distribution=="normal.se", scale := value ] ## so code will work later on

n.runs <- 1000
params.psa <- params.all[rep(params.all[, .I], n.runs )] ## create 1000 samples of each row
params.psa$run_id <- rep(1:n.runs, each=nrow(params.all))

temp.func.u <- function(shape,scale) { runif(1,shape,scale)}
temp.func.b <- function(shape,scale) { rbeta(1,shape,scale)}
temp.func.g <- function(shape,scale) { rgamma(1,shape,scale)}

## need to convert some into lognormal to stop <0 values

params.psa[ distribution=="lnormal.95", lnse := (log(value) - log(shape)) / (1.96*2)]
params.psa[ distribution=="lnormal.95", lnmu := log(value)]

temp.func.ln <- function(lnmu,lnse) { exp(rnorm(1, lnmu, lnse))}

params.psa[distribution=="uniform",value := mapply(temp.func.u, shape,scale)]
params.psa[distribution=="beta",value := mapply(temp.func.b, shape,scale)]
params.psa[distribution=="gamma",value := mapply(temp.func.g, shape,scale)]
params.psa[distribution=="lnormal.95",value := mapply(temp.func.ln,lnmu,lnse)]

### tested to make sure values were in bounds e.g. puti2bsi not >1
#### !! probably would want to add more code to check sum(transitions)>1 if had more time
psa.outputs <- list()

pb = txtProgressBar(min = 1, max = n.runs, initial = 0, style = 3)

for (i in 1:n.runs){
  setTxtProgressBar(pb,i)
  temp <- params.psa[run_id==i]
  temp.outputs <- model(temp)
  psa.outputs[[i]] <- temp.outputs[[2]]
}

psa.outputs.dt <- rbindlist(psa.outputs, use.names=TRUE, fill=TRUE, idcol=TRUE)

 write.csv(psa.outputs.dt, file="outputs/psa_outputs.csv")

# psa.outputs.dt<- read.csv("outputs/psa_outputs.csv") #can use this line if already generated
#### probablistic plots ####
options(scipen=10000)
plot.ce.plane <- function(results){
  ### FUNCTION: PLOTTING THE COST-EFFECTIVENESS PLANE (for one comparator, using incremetnal costs and outcomes)
  ### INPUTS: a results data frame that has the columns "inc.qalys" and "inc.cost"
  ### Need ggplot called
  ### OUTPUTS: cost-effectiveness plane plot
  require(ggplot2)
  xlabel = "Incremental QALYs"
  ylabel = "Incremental Cost"
  
  plot = ggplot(results) + 
    geom_point(shape = 21, size = 2, colour = "black", fill = NA, alpha = 0.5,
               aes(x=IncrementalQALYGain, y=IncrementalCost)) + 
    labs(x = xlabel, text = element_text(size=10)) + 
    labs (y = ylabel, text = element_text(size=10)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=12),
          plot.margin=unit(c(1.2,0.5,0,1.2),"cm"))+
    geom_vline(xintercept=0,linetype=3)+
    geom_hline(yintercept=0,linetype=3)+
  geom_abline(slope = 20000,
                intercept = 0,
                color="blue")+ theme_linedraw()
  
  return(plot)
  
}
plot.ce.plane(psa.outputs.dt)
ggsave("outputs/plot.ce.plane.png", width = 100, height = 100, units='mm',dpi=1000)

#### PLOTTING THE COST-EFFECTIVENESS ACCEPTABILITY CURVE #####

# Generate CEAC table
WTP.values <- seq(from = 0, to = 50000, by = 1000) ## use the seq() function to get a vector of specified numeric values

CEAC <- data.frame(WTP = WTP.values, 
                   current= rep(as.numeric(NA),length(WTP.values)),
                   ifast= rep(as.numeric(NA),length(WTP.values)))

simulation.results <- dcast(psa.outputs.dt, .id ~ intervention, 
                            value.var = c("excess_cost", "excess_qalyloss"))


### !!! remove NA values, need to investigate what is causing it
simulation.results <- simulation.results[!is.na(excess_cost_current) & !is.na(excess_cost_iFAST)]

pCE.3b <-function(WTP, simulation.results) {
  ## a function that estimates the probability of the new intervention
  # being cost-effective 
  # INPUTS: WTP = willingness to pay value (numeric)
  #         simulation.results = a data.frame output of PSA simulations which includes
  #         columns prefixed with "cost." and "qalys." for SP0, NP1 and NP2
  # OUTPUTS: A numeric value specifying the probability of cost-effectiveness across the 3 pathways given the inputs
  
  # Now calculate NMB for all three interventions
  nmb <- (-(simulation.results[,c("excess_qalyloss_current",
                          "excess_qalyloss_iFAST")])*WTP) - 
    (simulation.results[,c("excess_cost_current",
                          "excess_cost_iFAST")] )
  ## this time we want to choose the prothesis with the greatest net monetary benefit:
  max.nmb <- apply(nmb, 1, max) # selecting max value indication by row within nmb
  
  ## creating an indication of TRUE/FALSE as to whether each treatment column == that max value:
  CE <- nmb[1:nrow(simulation.results),] == max.nmb[1:nrow(simulation.results)] 
  probCE<- apply(CE, 2, mean) ## averaging over TRUE (=1) and FALE (=0) for each column
  
  return(probCE)
  
}


pb = txtProgressBar(min = 0, max = length(WTP.values), initial = 0, style = 3)

for (i in 1:length(WTP.values)) {
  setTxtProgressBar(pb,i)
  CEAC[i,"WTP"] <- WTP.values[i]
  CEAC[i,2:3]<- pCE.3b(WTP.values[i], simulation.results)
  
}


# Look at the results
head(CEAC)  
tail(CEAC)

CEAC.long <- melt(CEAC, id.vars = c("WTP"))
colnames(CEAC.long) <- c("WTP", "group", "pCE")
head(CEAC.long)

write.csv(CEAC.long, file="outputs/CEAC.long.csv")
#CEAC.long <- read.csv("outputs/CEAC.long.csv")
CEAC.long$Group<-CEAC.long$group
plot.ceac.all <- function(results){
  ## FUNCTION: Cost-effectiveness acceptability curve (CEAC) for multiple comparators
  ## INPUTS: a results data frame that has the columns "WTP", "group"& "pCE"
  ## OUTPUT: CEAC curve where the color of the CEAC differs for each comparator
  
  xlabel = "Willingness To Pay Threshold"
  ylabel = "Probability Cost-Effective"
  
  plot = ggplot(results) + geom_line(aes(x=WTP, y=pCE, color=Group), size=1) + 
    labs(x = xlabel, text = element_text(size=10)) + 
    labs(y = ylabel, text = element_text(size=10)) + theme_classic() +
    theme(legend.title = element_blank(), axis.title=element_text(face="bold"), 
          axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 3, l = 0)), 
          axis.title.y = element_text(margin = margin(t = 0, r = 7, b = 0, l = 3)), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          legend.key.width=unit(1.8,"line"), text = element_text(size=12)) + 
    scale_x_continuous(expand = c(0, 0.1)) + 
    scale_y_continuous(limits = c(0,1), breaks=seq(0,1,0.1), expand = c(0, 0))+ theme_linedraw()
  
  return(plot)
  
}

plot.ceac.all(CEAC.long)
ggsave("outputs/plot.ceac.all.png", width = 120, height = 100, units='mm',dpi=1000)
