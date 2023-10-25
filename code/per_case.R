########## DETERMINISTIC RUN OF MODEL ##########
source("code/iFAST_2023_model_function.R")

#### base case #######
params.all <- read.csv("inputs/ifast_inputparameters.csv")
params.all <- as.data.table(params.all)

params.all[parameter=="cohort", value:=1]

outputs.percase <- model(params.all)

### table for paper

percase.icer <- outputs.percase[[1]]
percase.icer <- percase.icer[,c("IncrementalCost","IncrementalQALYGain" , "Net Monetary Benefit", "scenario")]
percase.icer <- percase.icer[c(2,4,6,8,10,12,14,16,18,20,24),]

percase.icer <- percase.icer[order(percase.icer$`Net Monetary Benefit`, decreasing=TRUE),]
write.csv(percase.icer,file="outputs/percase_icer.csv")
