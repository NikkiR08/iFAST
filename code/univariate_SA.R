########### univariate sensitivity analyses #########
###### converting parameters
 
source("code/iFAST_2023_model_function.R")

params.all <-read.csv("inputs/ifast_inputparameters.csv")
params.all <- as.data.table(params.all)

############ uniform parameters #################
### create variable to ID parameters where there are multiple e.g. across age/sex groups
#### just do for the base case scenario
params.all<-params.all[params.all$abx_scenario %in% c("all","base"),] #only base (remove low trim parameters)
params.all[ , parameter_specific := paste0(uti_group,"_",age_group,"_",
                                       sex_group,"_", parameter)]
params.all[ , ID := seq_len(.N), by = distribution] 

params.all.keep <- copy(params.all)

outputs.deterministic <- model(params.all)
outputs.deterministic <- outputs.deterministic[[2]]
outputs.deterministic$univariate_scenario <- "deterministic"
outputs.deterministic$parameter <- "Not Applicable"

n.temp.unif <- length(which(params.all.keep$distribution=="uniform"))

outputs.low <- list()
outputs.high <- list()

for (i in 1:n.temp.unif){
  temp <- copy(params.all.keep)
  temp[distribution=="uniform" & ID==i, value := shape]
  outputs.low.temp <- model(temp)

  outputs.low[[i]] <- outputs.low.temp[[2]]
  outputs.low[[i]]$parameter <- temp[distribution=="uniform" & ID==i, parameter_specific]
  outputs.low[[i]]$univariate_scenario <- "low"
  rm(temp)
  temp <- copy(params.all.keep)
  temp[distribution=="uniform" & ID==i, value := scale]
  outputs.high.temp <- model(temp)
  outputs.high[[i]] <- outputs.high.temp[[2]]
  outputs.high[[i]]$parameter <- temp[distribution=="uniform" & ID==i, parameter_specific]
  outputs.high[[i]]$univariate_scenario <- "high"
  rm(temp)
}


outputs.low <- rbindlist(outputs.low)
outputs.high <- rbindlist(outputs.high)
all.uniform <- rbind(outputs.low, outputs.high, outputs.deterministic)

write.csv(all.uniform, file="outputs/Univariate_Uniform.csv")
