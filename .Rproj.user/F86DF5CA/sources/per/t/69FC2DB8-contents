

# outcome correlations


library(Matrix)
library(lme4)
library(lmerTest)
library(tidyverse)
#library(corrplot)
#library(patchwork)
#library(broom)
#library(ggcorrplot)
library(ggpubr)
#library(GGally)
library(car)

source("functions/geom_boxjitter.R")


rie2 <- read.csv("rawdata/rie_and_maxMC.csv")
rie2 <- rie2[,-1]

rie.forcor <- rie2 %>% 
  select(subslice,group,pnd,RMP.pre,epsc.events,epsc.amp,epsc.auc.pAms,mepsc.events,mepsc.amp,mepsc.auc.pAms,max_MC_pA)




######
# #Create correlation matrices for each group
# cor_list <- rie.forcor %>%
#   group_by(group) %>%
#   group_map(~ {
#     cor_matrix <- cor(.x[, 3:11], use = "pairwise.complete.obs")
#     return(cor_matrix)
#   })
# 
# names(cor_list) <- unique(rie.forcor$group)
# 
# # Create a combined dataframe for plotting
# cor_long <- map2_dfr(cor_list, names(cor_list), ~ {
#   as.data.frame(.x) %>%
#     rownames_to_column("var1") %>%
#     pivot_longer(cols = -var1, names_to = "var2", values_to = "correlation") %>%
#     mutate(group = .y)
# })
# 
# # Plot with facets
# ggplot(cor_long, aes(x = var1, y = var2, fill = correlation)) +
#   geom_tile() +
#   geom_text(aes(label = round(correlation, 2)), color = "white", size = 3) +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white",
#                        midpoint = 0, limit = c(-1, 1), space = "Lab",
#                        name = "Correlation") +
#   facet_wrap(~ group) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = "Correlation Matrix by Group", x = "", y = "")



######

#first all epsc corrs

ep.rmp.lm <- lm(epsc.events~group*RMP.pre, data=rie.forcor, na.action=na.exclude)
summary(ep.rmp.lm) #no interaction 


ep.epam.lm <- lm(epsc.events~group*epsc.amp, data=rie.forcor, na.action=na.exclude)
summary(ep.epam.lm) #no interaction -- poss main eff


ep.epauc.lm <- lm(epsc.events~group*epsc.auc.pAms, data=rie.forcor, na.action=na.exclude)
summary(ep.epauc.lm) #no interaction 


ep.mepev.lm <- lm(epsc.events~group*mepsc.events, data=rie.forcor, na.action=na.exclude)
summary(ep.mepev.lm) #no interaction 


ep.mcpa.lm <- lm(epsc.events~group*max_MC_pA, data=rie.forcor, na.action=na.exclude)
summary(ep.mcpa.lm) #no interaction 


epamp.mcpa.lm <- lm(epsc.amp~group*max_MC_pA, data=rie.forcor, na.action=na.exclude)
summary(epamp.mcpa.lm) #no interaction 


epamp.epauc.lm <- lm(epsc.amp~group*epsc.auc.pAms, data=rie.forcor, na.action=na.exclude)
summary(epamp.epauc.lm) #no interaction -- poss main eff


epauc.mcpa.lm <- lm(epsc.auc.pAms~group*max_MC_pA, data=rie.forcor, na.action=na.exclude)
summary(epauc.mcpa.lm) #no interaction -- poss main eff


# mini corrs

mep.rmp.lm <- lm(mepsc.events~group*RMP.pre, data=rie.forcor, na.action=na.exclude)
summary(mep.rmp.lm) #no interaction -- poss main eff 


mep.epam.lm <- lm(mepsc.events~group*mepsc.amp, data=rie.forcor, na.action=na.exclude)
summary(mep.epam.lm) #no interaction 


mep.epauc.lm <- lm(mepsc.events~group*mepsc.auc.pAms, data=rie.forcor, na.action=na.exclude)
summary(mep.epauc.lm) #no interaction 


mep.mcpa.lm <- lm(mepsc.events~group*max_MC_pA, data=rie.forcor, na.action=na.exclude)
summary(mep.mcpa.lm) #no interaction 


mepamp.epauc.lm <- lm(mepsc.amp~group*mepsc.auc.pAms, data=rie.forcor, na.action=na.exclude)
summary(mepamp.epauc.lm) #INTERACTION p=0.0275


mepamp.mcpa.lm <- lm(mepsc.amp~group*max_MC_pA, data=rie.forcor, na.action=na.exclude)
summary(mepamp.mcpa.lm) #no interaction -- poss main eff 


mepauc.mcpa.lm <- lm(mepsc.auc.pAms~group*max_MC_pA, data=rie.forcor, na.action=na.exclude)
summary(mepauc.mcpa.lm) #no interaction



# rmp and MC

mcpa.rmp.lm <- lm(max_MC_pA~group*RMP.pre, data=rie.forcor, na.action=na.exclude)
summary(mcpa.rmp.lm) #no interaction








###### significant figures 

### mepsc.amp~group*mepsc.auc.pAms
ggplot(aes(x=mepsc.amp, y=mepsc.auc.pAms, color=group, linetype = group, shape = group), data=rie.forcor) +
  labs(title="mEPSC amplitude by AUC ",x="mEPSC Amplitude (pA)", y = "AUC (pA-ms)") +
  geom_smooth(method="lm", se=F) +
  geom_point(size = 3) +
   stat_cor(label.x = -18, size = 5, show.legend=FALSE)+
   scale_linetype_manual(values=c("longdash", "solid"))+
   scale_color_manual(values=c('hotpink','lightblue'))+
   # geom_label(x = 1,  y = 1, 
   #            label = "Interaction p = .0275", 
   #            colour = "black",
   #            size=6) +
  theme_classic() +
  annotate("text",x=-16.4, y=-38, label = "Interaction p=.0275", color = "black", size = 6) +
  theme(axis.line = element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = "black", size = 1),
        legend.position = c(.37, .908),
        legend.title = element_blank(),
        legend.text = element_text(size=12,face="bold"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=24, hjust = 0.4),
        legend.background=element_blank())
#save 1100x650

mepamp.epauc.lm <- lm(mepsc.amp~group*mepsc.auc.pAms, data=rie.forcor, na.action=na.exclude)
summary(mepamp.epauc.lm) #INTERACTION p=0.0275





