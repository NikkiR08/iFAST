#source("code/deterministic.R")

data.merged_temp <- outputs.deterministic[[8]]
ggplot(data.merged_temp, aes(cycles,value)) + 
  geom_line(aes(colour = variable_combi),size=0.5) + xlab("Hours") + ylab("Patients on Antibiotic") +
  geom_point(aes(colour = variable_combi),size=0.3)+
  scale_color_brewer(name = "type", palette = "Set2") + scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 60),expand = c(0,0)) + #temporarily set to x=60 to see whats going on
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  facet_wrap(. ~ variable_combi, scales="free_y",ncol=1)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_60hours.png", width = 150, height = 100, units='mm',dpi=1000)

ifast.outputs <- outputs.deterministic[[1]]
ifast.outputs$intervention <- rep(c("current","iFAST"))
ifast.outputs$scenario_2 <- c(rep("Uncomplicated",8),rep("Complicated, IV",8),rep("Complicated, Oral",8))
ifast.outputs$scenario_3 <- c(rep("16-64 females",2),rep("16-64 males",2),
                             rep("65-100 females",2),rep("65-100 males",2))
ifast.outputs$Net_Monetary_Benefit <- ifast.outputs$`Net Monetary Benefit`
ifast.outputs$intervention_scenario_2 <-
  paste(ifast.outputs$intervention, ifast.outputs$scenario_2, sep = ", ")

#comparing intervention, and compl/uncompl split (very similar)
p1_excess_cost<- ggplot(ifast.outputs, 
                             aes(x=scenario,y=excess_cost)) + 
  geom_col(aes(fill = intervention_scenario_2),size=0.5,position="dodge") + xlab("Scenario") + ylab("Excess Cost") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_3,ncol=4,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_excess_cost.png", width = 200, height = 100, units='mm',dpi=1000)

p1_incremental_cost<- ggplot(ifast.outputs[seq(from=2,to=nrow(ifast.outputs),by=2),], 
                             aes(x=scenario,y=IncrementalCost)) + 
  geom_col(aes(fill = scenario_2),size=0.5) + xlab("Scenario") + ylab("Incremental Cost") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_3,ncol=4,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_incremental_cost.png", width = 150, height = 100, units='mm',dpi=1000)


p1_incremental_QALY_gain<- ggplot(ifast.outputs[seq(from=2,to=nrow(ifast.outputs),by=2),], 
                             aes(x=scenario,y=IncrementalQALYGain)) + 
  geom_col(aes(fill = scenario_2),size=0.5) + xlab("Scenario") + ylab("Incremental QALY Gain") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_3,ncol=4,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_incremental_QALY_gain.png", width = 150, height = 100, units='mm',dpi=1000)

p1_Net_Monetary_Benefit<- ggplot(ifast.outputs[seq(from=2,to=nrow(ifast.outputs),by=2),], 
                                  aes(x=scenario,y=Net_Monetary_Benefit)) + 
  geom_col(aes(fill = scenario_2),size=0.5) + xlab("Scenario") + ylab("Net Monetary Benefit") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_3,ncol=4,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_Net_Monetary_Benefit.png", width = 150, height = 100, units='mm',dpi=1000)



#print(p1_incremental_cost)
#print(p1_incremental_QALY_gain)
#print(p1_Net_Monetary_Benefit)

require("ggpubr")

outputs_comb <- ggarrange(p1_incremental_cost, p1_incremental_QALY_gain 
                            , p1_Net_Monetary_Benefit,nrow=3,  common.legend = TRUE,align = "v",
                          labels = c("A","B","C"))
outputs_comb
ggsave("outputs/outputs_comb.png", width = 250, height = 200, units='mm',dpi=1000)


#Comparing the base to low_trim scenario and high prev scenario

#comparing intervention, and compl/uncompl split (very similar)
ifast.outputs_2 <- outputs.deterministic.low_trim[[1]]
ifast.outputs_2$intervention <- rep(c("current","iFAST"))
ifast.outputs_2$scenario_2 <- c(rep("Uncomplicated",8),rep("Complicated, IV",8),rep("Complicated, Oral",8))
ifast.outputs_2$scenario_3 <- c(rep("16-64 females",2),rep("16-64 males",2),
                               rep("65-100 females",2),rep("65-100 males",2))
ifast.outputs_2$Net_Monetary_Benefit <- ifast.outputs_2$`Net Monetary Benefit`
ifast.outputs_2$intervention_scenario_2 <-
  paste(ifast.outputs_2$intervention, ifast.outputs_2$scenario_2, sep = ", ")

ifast.outputs_3 <- outputs.deterministic.high_prev[[1]]
ifast.outputs_3$intervention <- rep(c("current","iFAST"))
ifast.outputs_3$scenario_2 <- c(rep("Uncomplicated",8),rep("Complicated, IV",8),rep("Complicated, Oral",8))
ifast.outputs_3$scenario_3 <- c(rep("16-64 females",2),rep("16-64 males",2),
                                rep("65-100 females",2),rep("65-100 males",2))
ifast.outputs_3$Net_Monetary_Benefit <- ifast.outputs_3$`Net Monetary Benefit`
ifast.outputs_3$intervention_scenario_2 <-
  paste(ifast.outputs_3$intervention, ifast.outputs_3$scenario_2, sep = ", ")

ifast.outputs_3$scenario<-gsub("base", "high_prev", ifast.outputs_3$scenario)

temp1<-ifast.outputs[seq(from=2,to=nrow(ifast.outputs),by=2),]
temp1$scenario_4 <- "Base Scenario"
temp2<- ifast.outputs_2[seq(from=2,to=nrow(ifast.outputs_2),by=2),]
temp2$scenario_4 <- "Low Trimethoprim Scenario"
temp3<- ifast.outputs_3[seq(from=2,to=nrow(ifast.outputs_3),by=2),]
temp3$scenario_4 <- "High Prevalence Scenario"

ifast.outputs_combined_base_trim <- rbind(temp1,temp2)
ifast.outputs_combined <- rbind(temp1,temp2,temp3)

####base + low trim plots
p1_incremental_cost_combined_base_trim<- ggplot(ifast.outputs_combined_base_trim, 
                                                aes(x=scenario,y=IncrementalCost)) + 
  geom_col(aes(fill = scenario_4),size=0.5) + xlab("Scenario") + ylab("Incremental Cost") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_2+scenario_3,ncol=6,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_incremental_cost_combined_base_trim.png", width = 250, height = 100, units='mm',dpi=1000)


p1_incremental_QALY_gain_combined_base_trim<- ggplot(ifast.outputs_combined_base_trim,
                                                     aes(x=scenario,y=IncrementalQALYGain)) + 
  geom_col(aes(fill = scenario_4),size=0.5) + xlab("Scenario") + ylab("Incremental QALY Gain") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_2+scenario_3,ncol=6,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_incremental_QALY_gain_combined_base_trim.png", width = 250, height = 100, units='mm',dpi=1000)

p1_Net_Monetary_Benefit_combined_base_trim<- ggplot(ifast.outputs_combined_base_trim,
                                                    aes(x=scenario,y=Net_Monetary_Benefit)) + 
  geom_col(aes(fill = scenario_4),size=0.5) + xlab("Scenario") + ylab("Net Monetary Benefit") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_2+scenario_3,ncol=6,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_Net_Monetary_Benefit_combined_base_trim.png", width = 250, height = 100, units='mm',dpi=1000)


#print(p1_incremental_cost_combined_base_trim)
#print(p1_incremental_QALY_gain_combined_base_trim)
options(scipen=999)
print(p1_Net_Monetary_Benefit_combined_base_trim)

outputs_comb_combined_base_trim <- ggarrange(p1_incremental_cost_combined_base_trim, p1_incremental_QALY_gain_combined_base_trim
                                             , p1_Net_Monetary_Benefit_combined_base_trim,nrow=3,  common.legend = TRUE,align = "v",
                                             labels = c("A","B","C"))
outputs_comb_combined_base_trim
ggsave("outputs/outputs_comb_combined_base_trim.png", width = 250, height = 200, units='mm',dpi=1000)

### base+low_trim+high_prev

p1_incremental_cost_combined<- ggplot(ifast.outputs_combined, 
                                      aes(x=scenario,y=IncrementalCost)) + 
  geom_col(aes(fill = scenario_4),size=0.5) + xlab("Scenario") + ylab("Incremental Cost") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_2+scenario_3,ncol=6,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_incremental_cost_combined.png", width = 250, height = 100, units='mm',dpi=1000)


p1_incremental_QALY_gain_combined<- ggplot(ifast.outputs_combined,
                                           aes(x=scenario,y=IncrementalQALYGain)) + 
  geom_col(aes(fill = scenario_4),size=0.5) + xlab("Scenario") + ylab("Incremental QALY Gain") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_2+scenario_3,ncol=6,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_incremental_QALY_gain_combined.png", width = 250, height = 100, units='mm',dpi=1000)

p1_Net_Monetary_Benefit_combined<- ggplot(ifast.outputs_combined,
                                          aes(x=scenario,y=Net_Monetary_Benefit)) + 
  geom_col(aes(fill = scenario_4),size=0.5) + xlab("Scenario") + ylab("Net Monetary Benefit") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_2+scenario_3,ncol=6,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_Net_Monetary_Benefit_combined.png", width = 250, height = 100, units='mm',dpi=1000)


#print(p1_incremental_cost_combined)
#print(p1_incremental_QALY_gain_combined)
options(scipen=999)
print(p1_Net_Monetary_Benefit_combined)

outputs_comb_combined <- ggarrange(p1_incremental_cost_combined, p1_incremental_QALY_gain_combined
                                   , p1_Net_Monetary_Benefit_combined,nrow=3,  common.legend = TRUE,align = "v",
                                   labels = c("A","B","C"))
outputs_comb_combined
ggsave("outputs/outputs_comb_combined.png", width = 250, height = 200, units='mm',dpi=1000)


##alternative version to outputs_comb_combined.png
p1_incremental_cost_combined_alt<- ggplot(ifast.outputs_combined, aes(x=scenario,y=IncrementalCost)) + 
  geom_col(aes(fill = scenario_2),size=0.5) + xlab("Scenario") + ylab("Incremental Cost") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_4+scenario_3,nrow=3,scales = "free")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))

p1_incremental_QALY_gain_combined_alt<- ggplot(ifast.outputs_combined,
                                               aes(x=scenario,y=IncrementalQALYGain)) + 
  geom_col(aes(fill = scenario_2),size=0.5) + xlab("Scenario") + ylab("Incremental QALY Gain") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_4+scenario_3,nrow=3,scales = "free")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))

p1_Net_Monetary_Benefit_combined_alt<- ggplot(ifast.outputs_combined,
                                              aes(x=scenario,y=Net_Monetary_Benefit)) + 
  geom_col(aes(fill = scenario_2),size=0.5) + xlab("Scenario") + ylab("Net Monetary Benefit") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_4+scenario_3,nrow=3,scales = "free")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))


#print(p1_incremental_cost_combined)
#print(p1_incremental_QALY_gain_combined)
options(scipen=999)

outputs_comb_combined_alt <- ggarrange(p1_incremental_cost_combined_alt, 
                                       p1_incremental_QALY_gain_combined_alt,
                                       p1_Net_Monetary_Benefit_combined_alt,nrow=3,  
                                       common.legend = TRUE,align = "v", 
                                       labels = c("A","B","C"))

ggsave("outputs/outputs_comb_combined_alt.png", width = 250, height = 300, units='mm',dpi=1000)



### bed hours saved
#take the outputs.deterministic file and rejig
still.in.hospital <- outputs.deterministic[[3]][outputs.deterministic[[3]]$variable=="still_in_hospital",]
cohortsize <- outputs.deterministic[[3]][outputs.deterministic[[3]]$cycle==1,] #this takes the initial numbers in each cohort
cohortsize<-cohortsize[cohortsize$variable=="still_in_hospital" &cohortsize$intervention=="current" ,][,c(3,5)]
colnames(cohortsize)[1] <- "size"
summary.bedhours <- aggregate(value~scenario+intervention,still.in.hospital,sum)
summary.bedhours$bedhours.saved <- -c(rep(NA,12),summary.bedhours$value[13:24] - summary.bedhours$value[1:12] )
#### !!! for now just adding in - sign here but really should flip round the above to get non-negative
### as "bed days saved" :
summary.bedhours$beddays.saved <- summary.bedhours$bedhours.saved /24
summary.bedhours$scenario_2 <- rep(c("Complicated, IV","Complicated, Oral","Uncomplicated"),8)
summary.bedhours$scenario_3 <- c(rep("16-64 females",3),rep("16-64 males",3),
                                 rep("65-100 females",3),rep("65-100 males",3))

summary.bedhours<-merge(summary.bedhours,cohortsize,by="scenario")
#summary.bedhours$LOS <- summary.bedhours$value/ summary.bedhours$size
write.csv(summary.bedhours,"outputs/summary.bedhours.csv")
sum(summary.bedhours$beddays.saved, na.rm=TRUE)

## av length of stay
total.n <- 246395
  t1 <- subset(summary.bedhours, intervention=="current")
current.t <- sum(t1$value, na.rm=TRUE)
x <- current.t/total.n

t2 <- subset(summary.bedhours, intervention=="ifast")
ifast.t <- sum(t2$value, na.rm=TRUE)
y <- ifast.t/total.n
x-y

#bed hours
ggplot(summary.bedhours, 
       aes(x=scenario,y=bedhours.saved)) + 
  geom_col(aes(fill = scenario_2),size=0.5) + xlab("Scenario") + ylab("Bed Hours Saved") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_3,ncol=4,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/summary.bedhours.png", width = 200, height = 100, units='mm',dpi=1000)

#bed days
ggplot(summary.bedhours, 
       aes(x=scenario,y=beddays.saved)) + 
  geom_col(aes(fill = scenario_2),size=0.5) + xlab("Scenario") + ylab("Bed Days Saved") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_3,ncol=4,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/summary.beddays.png", width = 200, height = 100, units='mm',dpi=1000)


#per case
percase.icer$scenario_2 <- c(rep("Uncomplicated",4),rep("Complicated, IV",4),rep("Complicated, Oral",4))
percase.icer$scenario_3 <- c("65-100 females", "16-64 males", "65-100 males", "16-64 females",
                              "16-64 males", "65-100 females", "65-100 males", "16-64 females",
                              "16-64 males", "65-100 females", "65-100 males", "16-64 females"
                              )
percase.icer$Net_Monetary_Benefit <- percase.icer$`Net Monetary Benefit`

p1_percase_IncrementalCost<- ggplot(percase.icer, 
                               aes(x=scenario,y=IncrementalCost)) + 
  geom_col(aes(fill = scenario_2),size=0.5) + xlab("Scenario") + ylab("Incremental Cost") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_3,ncol=4,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_percase.png", width = 150, height = 100, units='mm',dpi=1000)

p1_percase_IncrementalQALYGain<- ggplot(percase.icer, 
                    aes(x=scenario,y=IncrementalQALYGain)) + 
  geom_col(aes(fill = scenario_2),size=0.5) + xlab("Scenario") + ylab("Incremental QALY Gain") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_3,ncol=4,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_percase_IncrementalQALYGain.png", width = 150, height = 100, units='mm',dpi=1000)

p1_percase_Net_Monetary_Benefit<- ggplot(percase.icer, 
                    aes(x=scenario,y=`Net Monetary Benefit`)) + 
  geom_col(aes(fill = scenario_2),size=0.5) + xlab("Scenario") + ylab("Net Monetary Benefit") +
  scale_color_brewer(name = "state", palette = "Set2") +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_wrap(. ~ scenario_3,ncol=4,scales = "free_x")+ scale_fill_discrete(name = "")+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/p1_percase_Net_Monetary_Benefit.png", width = 150, height = 100, units='mm',dpi=1000)

outputs_comb_percase <- ggarrange(p1_percase_IncrementalCost, p1_percase_IncrementalQALYGain 
                          , p1_percase_Net_Monetary_Benefit,nrow=3,  common.legend = TRUE,align = "v",
                          labels = c("A","B","C"))

ggsave("outputs/outputs_comb_percase.png", width = 250, height = 200, units='mm',dpi=1000)




