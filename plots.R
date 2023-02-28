####### antibiotic use ####
p2<-ggplot(ab_use, aes(cycles,value)) + 
  geom_line(aes(colour = variable),size=0.5) + xlab("hours") + ylab("proportion hours on antibiotic") +
  geom_point(aes(colour = variable),size=0.3)+
  scale_color_brewer(name = "type", palette = "Set2") + scale_y_continuous(expand = c(0,0),labels = label_comma()) +
  scale_x_continuous(limits = c(0, cycles),expand = c(0,0),labels = label_comma()) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  facet_wrap(. ~ variable, scales="free_y",ncol=1)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
p2

p2b<-ggplot(ab_use_ifast, aes(cycles,value)) + 
  geom_line(aes(colour = variable),size=0.5) + xlab("hours") + ylab("proportion hours on antibiotic") +
  geom_point(aes(colour = variable),size=0.3)+
  scale_color_brewer(name = "type", palette = "Set2") + scale_y_continuous(expand = c(0,0),labels = label_comma()) +
  scale_x_continuous(limits = c(0, cycles),expand = c(0,0),labels = label_comma()) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  facet_wrap(. ~ variable, scales="free_y",ncol=1)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
p2b

######## life hours ####
lh <- as.data.frame(lh)
lh$cycles <- 1:cycles
colnames(lh)[1] <- c("QALHs")
lh$QALHs <- as.numeric(as.character(lh$QALHs))   
lh$cycles <- as.numeric(as.character(lh$cycles))   
p3<-ggplot(lh, aes(cycles,QALHs)) + 
  geom_line(size=0.5) + xlab("hours") + ylab("QALHs") +
  #scale_color_distiller(name = "state", palette = "Set2")+
  geom_point(size=0.3) + scale_y_continuous(limits = c(min(lh$QALHs)-0.001, 1),expand = c(0,0)) +
  scale_x_continuous(limits = c(0, cycles+2),expand = c(0,0)) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()

#### hospital costs #####

p4<-ggplot(cost_time, aes(cycles,cumsumcost)) + 
  geom_line(size=0.5) + xlab("hours") + ylab("cumulative cost") +
  #scale_color_distiller(name = "state", palette = "Set2")+
  geom_point(size=0.3) + scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits = c(0, cycles+2),expand = c(0,0)) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()

#### other plots from ross - havent checked if work with new data ####
#plots
tracedata_INT <- as.data.frame(trace_INT)
tracedata_INT$cycle <- 1: cycles
tracedata_INT$BSI <- tracedata_INT$D.BSI1 + tracedata_INT$G.BSI2 + tracedata_INT$J.BSI3 + tracedata_INT$M.BSI4 
tracedata_INT$stillinhospital <- tracedata_INT$C.noBSI1 + tracedata_INT$E.culdir + tracedata_INT$F.noBSI2 + tracedata_INT$H.resfail + tracedata_INT$I.noBSI3 + tracedata_INT$K.severe + tracedata_INT$L.noBSI4
df2_INT <- select(tracedata_INT,cycle,B.treatment_INT,stillinhospital,BSI,N.recovery, O.death)
colnames(df2_INT) <- c("cycle", "incoming_patients", "still_in_hospital","BSI", "recovery", "death")
df2_INT <- melt(df2_INT ,  id.vars = 'cycle', variable.name = 'variable')

p1_INT<- ggplot(df2_INT, aes(cycle,value)) + 
  geom_line(aes(colour = variable),size=0.5) + xlab("hours") + ylab("proportion") +
  geom_point(aes(colour = variable),size=0.3)+
  scale_color_brewer(name = "state", palette = "Set2") + scale_y_continuous(limits = c(0, 1),expand = c(0,0)) +
  scale_x_continuous(limits = c(0, cycles),expand = c(0,0)) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()

print(plot_grid(p1,p1_INT,
                #align = "v", 
                ncol = 1))

#deaths plot (state O.death)
df3<- select(tracedata,cycle,O.death)
df3_INT <- select(tracedata_INT,cycle,O.death)
df3$O.death_INT <- df3_INT$O.death
colnames(df3) <- c("cycle", "deaths", "deaths_int")
df3 <- melt(df3 ,  id.vars = 'cycle', variable.name = 'variable')

p_deaths<- ggplot(df3, aes(cycle,value)) + 
  geom_line(aes(colour = variable),size=0.5) + xlab("hours") + ylab("proportion") +
  geom_point(aes(colour = variable),size=0.3)+
  scale_color_brewer(name = "state", palette = "Set2") + scale_y_continuous(limits = c(0, max(df3$value)+0.001),expand = c(0,0)) +
  scale_x_continuous(limits = c(0, cycles),expand = c(0,0)) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()
p_deaths

#ICU plot (state K)
df4<- select(tracedata,cycle,G.ICU)
df4_INT <- select(tracedata.ifast,cycle,G.ICU)
df4$K.severe_INT <- df4_INT$G.ICU
total_hours_ICU <- colSums(df4)[2:3]
colnames(df4) <- c("cycle", "ICU_hours_current", "ICU_hours_iFAST")
df4 <- melt(df4 ,  id.vars = 'cycle', variable.name = 'variable')
p_ICU<- ggplot(df4, aes(cycle,value)) + 
  geom_line(aes(colour = variable),size=0.5) + xlab("hours") + ylab("proportion") +
  geom_point(aes(colour = variable),size=0.3)+
  scale_color_brewer(name = "state", palette = "Set2") + scale_y_continuous(limits = c(0, max(df4$value)+0.000001),expand = c(0,0)) +
  scale_x_continuous(limits = c(0, cycles),expand = c(0,0)) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()
p_ICU
total_hours_ICU

#BSI plot for proportion of those in states D, G, J, K
df5<- select(tracedata,cycle,BSI)
df5_INT <- select(tracedata_INT,cycle,BSI)
df5$BSI_INT <- df5_INT$BSI
total_hours_BSI <- colSums(df5)[2:3]
colnames(df5) <- c("cycle", "BSI_hours", "BSI_hours_int")
df5 <- melt(df5 ,  id.vars = 'cycle', variable.name = 'variable')
p_BSI<- ggplot(df5, aes(cycle,value)) + 
  geom_line(aes(colour = variable),size=0.5) + xlab("hours") + ylab("proportion") +
  geom_point(aes(colour = variable),size=0.3)+
  scale_color_brewer(name = "state", palette = "Set2") + scale_y_continuous(limits = c(0, max(df5$value)+0.000001),expand = c(0,0)) +
  scale_x_continuous(limits = c(0, cycles),expand = c(0,0)) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()
p_BSI
total_hours_BSI

#assume (state K) + ICU_BSI_scale*(D, G, J, K) (assume 50% of BSI are in ICU severe)
ICU_BSI_scale <- 0.5
total_hours_ICU_BSI <- total_hours_BSI*ICU_BSI_scale + total_hours_ICU
df6 <- df4
df6$value <- ICU_BSI_scale*df5$value + df4$value
p_ICU_BSI<- ggplot(df6, aes(cycle,value)) + 
  geom_line(aes(colour = variable),size=0.5) + xlab("hours") + ylab("proportion") +
  geom_point(aes(colour = variable),size=0.3)+
  scale_color_brewer(name = "state", palette = "Set2") + scale_y_continuous(limits = c(0, max(df6$value)+0.000001),expand = c(0,0)) +
  scale_x_continuous(limits = c(0, cycles),expand = c(0,0)) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()
p_ICU_BSI
names(total_hours_ICU_BSI) <- c("ICU_BSI_hours", "ICU_BSI_hours_int")
total_hours_ICU_BSI
