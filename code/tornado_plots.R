
###TORNADO 

require("ggpubr")
require("stringr")

out_uni <- read.csv("outputs/Univariate_Uniform.csv")
out_uni <- as.data.table(out_uni)
out_det <- out_uni[out_uni$univariate_scenario=="deterministic",][2,]
out_uni <- out_uni[seq(from=2,to=nrow(out_uni),by=2),]
out_uni_low <-out_uni[out_uni$univariate_scenario=="low",]
out_uni_low<-out_uni_low[order(out_uni_low$parameter),]
out_uni_high <-out_uni[out_uni$univariate_scenario=="high",]
out_uni_high<-out_uni_high[order(out_uni_high$parameter),]
#out_uni$intervention <- rep(c("current","ifast"),nrow(out_uni)/2)
length_params <- length(table(out_uni[seq(from=2,to=nrow(out_uni),by=2),]$parameter))
out_uni_flat <- as.data.table(out_uni_low$parameter)
colnames(out_uni_flat)[1]<- "parameter"

out_uni_flat$IncrementalCost_low <- out_uni_low$IncrementalCost
out_uni_flat$IncrementalCost_high <- out_uni_high$IncrementalCost
out_uni_flat$IncrementalCost_det <- out_det$IncrementalCost
out_uni_flat$IncrementalQALYGain_low <- out_uni_low$IncrementalQALYGain
out_uni_flat$IncrementalQALYGain_high <- out_uni_high$IncrementalQALYGain
out_uni_flat$IncrementalQALYGain_det <- out_det$IncrementalQALYGain
out_uni_flat$NetMonetaryBenefit_low <- out_uni_low$NetMonetaryBenefit
out_uni_flat$NetMonetaryBenefit_high <- out_uni_high$NetMonetaryBenefit
out_uni_flat$NetMonetaryBenefit_det <- out_det$NetMonetaryBenefit

out_uni_flat$parameter  <- str_replace(out_uni_flat$parameter, "_other", paste0(".other")) #replace _ with .
out_uni_flat$parameter  <- str_replace(out_uni_flat$parameter, "complicated_IV", paste0("complicated.IV")) #replace _ with .
out_uni_flat$parameter  <- str_replace(out_uni_flat$parameter, "complicated_oral", paste0("complicated.oral")) #replace _ with .
out_uni_flat[, c("uti_group","age_group" ,"sex_group",
                 "parameter") := tstrsplit(parameter, "_", fixed=TRUE)]

out_uni_flat$parameter <- factor(out_uni_flat$parameter, levels=unique(out_uni_flat$parameter))

#dummy values to test
#out_uni_flat$icer_det <- -2
#out_uni_flat$icer_low <- c(-19,-20,-5,-3,-4)
#out_uni_flat$icer_high <- c(10,1,7,16,4)

tornado_1<-
  ggplot(out_uni_flat, aes(x=parameter)) +
  geom_crossbar(
    aes(y=NetMonetaryBenefit_det, ymin=NetMonetaryBenefit_low, ymax=NetMonetaryBenefit_high),
    fill='dodgerblue2', width=0.8
  ) +
  facet_grid(parameter~., scales='free', space='free', switch = 'y') +
  labs(x="", y='Net Monetary Benefit') +
  coord_flip()+
  facet_wrap(. ~ uti_group+age_group+sex_group,ncol=5,scales = "free_y")+
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/NetMonetaryBenefit.png", width = 300, height = 300, units='mm',dpi=500)

tornado_2 <-   ggplot(out_uni_flat, aes(x=parameter)) +
  geom_crossbar(
    aes(y=IncrementalCost_det, ymin=IncrementalCost_low, ymax=IncrementalCost_high),
    fill='dodgerblue2', width=0.8
  ) +
  facet_grid(parameter~., scales='free', space='free', switch = 'y') +
  labs(x="", y='Incremental Cost') +
  coord_flip()+
  facet_wrap(. ~ uti_group+age_group+sex_group,ncol=5,scales = "free_y")+
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/IncrementalCost.png", width = 300, height = 300, units='mm',dpi=500)


tornado_3 <- ggplot(out_uni_flat, aes(x=parameter)) +
  geom_crossbar(
    aes(y=IncrementalQALYGain_det, ymin=IncrementalQALYGain_low, ymax=IncrementalQALYGain_high),
    fill='dodgerblue2', width=0.8
  ) +
  facet_grid(parameter~., scales='free', space='free', switch = 'y') +
  labs(x="", y='Incremental QALY Gain') +
  coord_flip()+
  facet_wrap(. ~ uti_group+age_group+sex_group,ncol=5,scales = "free_y")+
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.title = element_blank())+ 
  theme(panel.grid = element_line(color = "black",
                                  size = 0.05,
                                  linetype = 1))+ theme_linedraw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))
ggsave("outputs/IncrementalQALYGain.png", width = 300, height = 300, units='mm',dpi=500)


