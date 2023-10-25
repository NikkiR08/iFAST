########### heat maps #########
source("code/iFAST_2023_model_function.R")

params.all <- read.csv("inputs/ifast_inputparameters.csv")
params.all <- as.data.table(params.all)

############ uniform parameters #################
### create variable to ID parameters where there are multiple e.g. across age/sex groups
#### just do for the base case scenario

params.all[ , ID := seq_len(.N), by = distribution] 
params.all.keep <- copy(params.all)
outputs.deterministic <- model(params.all)
outputs.deterministic <- outputs.deterministic[[2]]
outputs.deterministic$scenario <- "deterministic"
#outputs.deterministic$parameter <- "Not Applicable"

outputs.heat <- list()
#slices=10
slices_by = 2
slices1_min <- 2 #how granular do we want the heat maps for t.ifast
slices1_max <- 30
slices2_min <- 1 #min for cost.ifast.U
slices2_max <- 50

slices1_vec <- round(seq(slices1_min,slices1_max,by=slices_by),0)
slices2_vec <- round(seq(slices2_min,slices2_max,by=slices_by),0)

counter <- 0
for (i in 1:length(slices1_vec)){
  for (j in 1:length(slices2_vec)){
    counter <- counter + 1
   # print(counter)
  temp <- copy(params.all.keep)
  temp[parameter=="t.ifast"]$value <- slices1_vec[i]
  temp[parameter=="cost.ifast.U"]$value <- slices2_vec[j]
  print(c(temp[parameter=="t.ifast"]$value,temp[parameter=="cost.ifast.U"]$value)) #to keep track of model code
  outputs.slices.temp <- model(temp)
  outputs.heat[[counter]] <- outputs.slices.temp[[2]]
  outputs.heat[[counter]]$t.ifast <- slices1_vec[i]
  outputs.heat[[counter]]$cost.ifast.U <- slices2_vec[j]
  rm(temp)
  }
}
outputs.heat <- rbindlist(outputs.heat)
all.heat <- rbind(outputs.heat, outputs.deterministic,fill=TRUE)

write.csv(all.heat, file="outputs/all.heat.csv")
#all.heat<- read.csv("outputs/all.heat.csv")
all.heat <- all.heat[seq(from=2,to=nrow(all.heat),by=2),]
all.heat$t.ifast <- all.heat$t.ifast - 1 #adjust for way it is coded

p1_A<- ggplot(all.heat, aes(t.ifast, cost.ifast.U, fill= IncrementalCost)) + 
  geom_tile()+
  labs(x="Turnaround Time for BICT", y='Cost of BICT') +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+ 
  scale_fill_gradient(low = "blue", high = "orange")+
  labs(
       fill = "Incremental Cost")

p1_B<- ggplot(all.heat, aes(t.ifast, cost.ifast.U, fill= IncrementalQALYGain)) + 
  geom_tile()+
  labs(x="Turnaround Time for BICT", y='Cost of BICT') +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+ 
  scale_fill_gradient(low = "orange", high = "blue",na.value = "white")+
  labs(fill = "Incremental QALY Gain")

p1_C<- ggplot(all.heat, aes(t.ifast, cost.ifast.U, fill= NetMonetaryBenefit)) + 
  geom_tile()+
  labs(x="Turnaround Time for BICT", y='Cost of BICT') +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+ 
  scale_fill_gradient2(low = "orange", high = "blue",na.value = "white")+
  labs(fill = "Net Monetary Benefit")+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

require("ggpubr")
outputs_comb <- ggarrange(p1_A, p1_B 
                          , p1_C,nrow=3,align = "v",
                          labels = c("A","B","C"))
outputs_comb
options(scipen=10000000)

ggsave("outputs/all_heat.png", width = 150, height = 150, units='mm',dpi=1000)

