
# M-current

library(tidyverse)
library(ggpubr)

source("functions/geom_boxjitter.R")

rie <- read.csv("rawdata/rmp-impre-epsc-data.csv")
mcd <- read.csv("rawdata/mc-data.csv")

rie.id <- rie %>% select(subject,slice,pnd,group)

tmcd <- as.data.frame(t(mcd))

names(tmcd) <- lapply(tmcd[1, ], as.character)
tmcd <- tmcd[-1,]
tmcd1 <- tibble::rownames_to_column(tmcd, "ID")


tmcd2 <- tmcd1 %>%
  separate(ID, into = c("mc.tp", "subject"), sep = "_", remove = TRUE) %>%
  select(subject, everything()) %>%
  separate(mc.tp, into = c("drug", "tp"), sep = "\\.", remove = TRUE) %>%
  select(subject, everything())

mcd3 <- left_join(rie.id,tmcd2, by="subject")

mcd4 <- mcd3[!is.na(mcd3$drug),]

