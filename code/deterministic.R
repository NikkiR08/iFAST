#setwd("~/Desktop/ifast project/ifast_gitlab1705")
########## DETERMINISTIC RUN OF MODEL ##########
source("code/iFAST_2023_model_function.R")

#### base case #######
params.all <- read.csv("inputs/ifast_inputparameters.csv")
params.all <- as.data.table(params.all)
#params.all[params.all$parameter %in% c("sensitivity.c","sensitivity.i","specificity.c","specificity.i"),]$value <- 1
outputs.deterministic <- model(params.all)
outputs.deterministic.table <- outputs.deterministic[[2]]
outputs.deterministic.table_large <- outputs.deterministic[[1]]
write.csv(outputs.deterministic.table ,"outputs/outputs.deterministic.table.csv")
write.csv(outputs.deterministic.table_large ,"outputs/outputs.deterministic.table_large.csv")
#days on antibiotics
abdays.table <- as.data.frame(matrix(c(outputs.deterministic[[10]][c(1,3)],outputs.deterministic[[10]][c(2,4)]),ncol=2))
abdays.table[1:2] <- lapply(abdays.table[1:2], as.numeric)
abdays.table <- rbind ( abdays.table, 100-(abdays.table[2, ] / abdays.table[1, ])*100)
colnames(abdays.table)<- c("Inappropriate_Prescribing_Days","Appropriate_Prescribing_Days")
rownames(abdays.table)<- c("Current","iFAST","Percentage_Decrease")
write.csv(round(abdays.table,1) ,"outputs/abdays.table_base.csv")

# per case scenario
params.all[parameter=="cohort", value:=1]
outputs.percase <- model(params.all)
percase.icer <- outputs.percase[[1]]
percase.icer <- percase.icer[,c("IncrementalCost","IncrementalQALYGain" , "Net Monetary Benefit", "scenario")]
percase.icer <- percase.icer[c(2,4,6,8,10,12,14,16,18,20,22,24),]
#percase.icer <- percase.icer[order(percase.icer$`Net Monetary Benefit`),]
percase.icer[,1:3]<- signif(percase.icer[,1:3],4)
write.csv(percase.icer,file="outputs/percase_icer.csv")


#### low trim resistance scenario #####
### note important to read in again as for data.table it will update params.all when running through the model function
params.all <- read.csv("inputs/ifast_inputparameters.csv")
params.all <- as.data.table(params.all)
outputs.deterministic.low_trim <- model_trim(params.all)
outputs.deterministic.table.low_trim <- outputs.deterministic.low_trim[[2]]
write.csv(outputs.deterministic.table.low_trim ,"outputs/outputs.deterministic.table.low_trim.csv")
#days on antibiotics for low trim
abdays.table_low_trim <- as.data.frame(matrix(c(outputs.deterministic.low_trim[[10]][c(1,3)],outputs.deterministic.low_trim[[10]][c(2,4)]),ncol=2))
abdays.table_low_trim[1:2] <- lapply(abdays.table_low_trim[1:2], as.numeric)
abdays.table_low_trim <- rbind ( abdays.table_low_trim, 100-(abdays.table_low_trim[2, ] / abdays.table_low_trim[1, ])*100)
colnames(abdays.table_low_trim)<- c("Inappropriate_Prescribing_Days","Appropriate_Prescribing_Days")
rownames(abdays.table_low_trim)<- c("Current","iFAST","Percentage_Decrease")
write.csv(round(abdays.table_low_trim,1) ,"outputs/abdays.table_low_trim.csv")
# per case scenario
params.all[parameter=="cohort", value:=1]
outputs.percase <- model_trim(params.all)
percase.icer.low_trim <- outputs.percase[[1]]
percase.icer.low_trim <- percase.icer.low_trim[,c("IncrementalCost","IncrementalQALYGain" , "Net Monetary Benefit", "scenario")]
percase.icer.low_trim <- percase.icer.low_trim[c(2,4,6,8,10,12,14,16,18,20,22,24),]
#percase.icer.low_trim <- percase.icer.low_trim[order(percase.icer.low_trim$`Net Monetary Benefit`),]
percase.icer.low_trim[,1:3]<- signif(percase.icer.low_trim[,1:3],4)
write.csv(percase.icer.low_trim,file="outputs/percase_icer_low_trim.csv")

#### 40% r.prev scenario #####
params.all <- read.csv("inputs/ifast_inputparameters.csv")
params.all <- as.data.table(params.all)
#current values for r.prev range from 0.05 to 0.13. Set all to 0.40
params.all[params.all$parameter=="r.prev",]$value <- 0.4
outputs.deterministic.high_prev <- model(params.all)
outputs.deterministic.table.high_prev <- outputs.deterministic.high_prev[[2]]
write.csv(outputs.deterministic.table.high_prev ,"outputs/outputs.deterministic.table.high_prev.csv")
#days on antibiotics for 40% r.prev
abdays.table_high_prev <- as.data.frame(matrix(c(outputs.deterministic.high_prev[[10]][c(1,3)],outputs.deterministic.high_prev[[10]][c(2,4)]),ncol=2))
abdays.table_high_prev[1:2] <- lapply(abdays.table_high_prev[1:2], as.numeric)
abdays.table_high_prev <- rbind ( abdays.table_high_prev, 100-(abdays.table_high_prev[2, ] / abdays.table_high_prev[1, ])*100)
colnames(abdays.table_high_prev)<- c("Inappropriate_Prescribing_Days","Appropriate_Prescribing_Days")
rownames(abdays.table_high_prev)<- c("Current","iFAST","Percentage_Decrease")
write.csv(round(abdays.table_high_prev,1) ,"outputs/abdays.table_high_prev.csv")
# per case scenario
params.all[parameter=="cohort", value:=1]
outputs.percase <- model(params.all)
percase.icer.high_prev <- outputs.percase[[1]]
percase.icer.high_prev <- percase.icer.high_prev[,c("IncrementalCost","IncrementalQALYGain" , "Net Monetary Benefit", "scenario")]
percase.icer.high_prev <- percase.icer.high_prev[c(2,4,6,8,10,12,14,16,18,20,22,24),]
#percase.icer.high_prev <- percase.icer.high_prev[order(percase.icer.high_prev$`Net Monetary Benefit`),]
percase.icer.high_prev[,1:3]<- signif(percase.icer.high_prev[,1:3],4)
write.csv(percase.icer.high_prev,file="outputs/percase_icer_high_prev.csv")

combined_percaseNMB <- data.frame(as.vector(percase.icer$scenario),
                                  round(percase.icer$`Net Monetary Benefit`,2),
                                  round(percase.icer.low_trim$`Net Monetary Benefit`,2),
                                  round(percase.icer.high_prev$`Net Monetary Benefit`,2))
colnames(combined_percaseNMB)<- c("scenario","NMB base","NMB low trim","NMB high prev")
write.csv(combined_percaseNMB,"outputs/combined_percaseNMB.csv")

#combined table for paper
outputs.combined.all3 <- rbind(outputs.deterministic.table[2,-1],
                                       outputs.deterministic.table.low_trim[2,-1],
                               outputs.deterministic.table.high_prev[2,-1])
outputs.combined.all3$scenario <- c("base","low_trim","high_prev")
write.csv(outputs.combined.all3,"outputs/outputs.combined.all3.csv")

source("code/ifast_plots.R")


#additional outputs for table 1
table_1 <- abdays.table
table_1$LOS <- c(x/24,y/24, 100-(y/x*100))
table_1$beddays <- c(current.t/24,ifast.t/24,100-(ifast.t/current.t*100) ) 
BSI_hours <- outputs.deterministic[[3]][outputs.deterministic[[3]]$variable=="BSI",]
summary_BSI_hours <- aggregate(value~scenario+intervention,BSI_hours,sum)
summary_BSI_hours$BSIhours.saved <- -c(rep(NA,12),summary_BSI_hours$value[13:24] - summary_BSI_hours$value[1:12] )
summary_BSI_hours$BSIhours.saved <- summary_BSI_hours$BSIhours.saved /24
t1_BSI <- subset(summary_BSI_hours, intervention=="current")
current.t_BSI <- sum(t1_BSI$value, na.rm=TRUE)
t2_BSI <- subset(summary_BSI_hours, intervention=="ifast")
ifast.t_BSI <- sum(t2_BSI$value, na.rm=TRUE)
table_1$BSIdays <- c(current.t_BSI/24,ifast.t_BSI/24,100-(ifast.t_BSI/current.t_BSI*100) ) 
table_1<-rbind(table_1,table_1[1,] - table_1[2,])
rownames(table_1)[4]<-"Total_Decrease"
write.csv(table_1,"outputs/table_1.csv")


source("code/univariate_SA.R") #will take some time
source("code/tornado_plots.R") #to be run after generating Univariate_Uniform.csv
source("code/probablistic_SA.R") #will take some time
source("code/heat_map.R")
