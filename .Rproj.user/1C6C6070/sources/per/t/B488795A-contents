
# prelim group diffs

library(tidyverse)
library(ggpubr)

source("functions/geom_boxjitter.R")


rie <- read.csv("rawdata/rmp-impre-epsc-data.csv")

#### RMP

rmp.tp <- rie %>% 
  select(1,5:8) %>% 
  pivot_longer(col = c(3:5), names_to = "RMP.timepoint", values_to = "RMP")


rmp.tp %>%
  ggplot(aes(RMP.timepoint,RMP, fill = group))+
  stat_compare_means(method = "t.test", size = 6, label.x = 1.5) +
  geom_boxjitter(outlier.color = NA, jitter.shape = 21, 
                 alpha = 1,
                 width = 0.5,
                 jitter.height = 0.02, jitter.width = 0.02, errorbar.draw = TRUE,
                 position = position_dodge(0.8)) +
  scale_fill_manual(values=c("hotpink", "skyblue")) +
  labs(title="Resting Membrane Potential",x="Group", y = "RMP (mV)") +
  theme_classic() +
  theme(axis.line = element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = "black", size = 1),
        #        legend.position="none",
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=24, hjust = 0.4)) 




#### input res (10mV minus -10mV div by 20)

in.re.tp <- rie %>% 
  select(1,5, 24:29) %>% 
  mutate(
    in.re.pre = (inp.pre.10 - inp.pre.n10) / 20,
    in.re.ttx = (inp.ttx.10 - inp.ttx.n10) / 20,
    in.re.xe = (inp.xe.10 - inp.xe.n10) / 20
  ) %>% 
  select(1,2,9,10,11) %>% 
  pivot_longer(col = c(3:5), names_to = "Inp.re.timepoint", values_to = "Inp.re")

  
in.re.tp %>%
  ggplot(aes(Inp.re.timepoint,Inp.re, fill = group))+
  stat_compare_means(method = "t.test", size = 6, label.x = 1.5) +
  geom_boxjitter(outlier.color = NA, jitter.shape = 21, 
                 alpha = 1,
                 width = 0.5,
                 jitter.height = 0.02, jitter.width = 0.02, errorbar.draw = TRUE,
                 position = position_dodge(0.8)) +
  scale_fill_manual(values=c("hotpink", "skyblue")) +
  labs(title="Input Resistance",x="Group", y = "Input Resistance (MΩ)") +
  theme_classic() +
  theme(axis.line = element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = "black", size = 1),
        #        legend.position="none",
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        plot.title = element_text(size=24, hjust = 0.4)) 



