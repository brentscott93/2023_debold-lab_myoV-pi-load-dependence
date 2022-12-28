library(data.table)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(magick)
library(ggridges)
library(ggtext)
library(svg)
library(drc)
library(readxl)
library(randomcoloR)
library(dplyr)

# read in data
myoV <- fread("data/MyoV_Pi_Rebind_Events4.csv")

# rename conditions for consistent style/format
# MYOSIN_PI_POWER
myoV$conditions2 <- ifelse(myoV$conditions == "S217A", "S217A_0mM-Pi_1.5-W",
                           ifelse(myoV$conditions == "S217A 30mM Pi","S217A_30mM-Pi_1.5-W",
                                  ifelse(myoV$conditions == "S217A 30mM Pi 2W", "S217A_30mM-Pi_2-W",
                                         ifelse(myoV$conditions == "S217A 30mM Pi_3W", "S217A_30mM-Pi_3-W",
                                                ifelse(myoV$conditions == "WT", "WT_0mM-Pi_1.5-W",
                                                       ifelse(myoV$conditions == "WT 30mM Pi", "WT_30mM-Pi_1.5-W", 
                                                              ifelse(myoV$conditions == "WT 30mM Pi 2W", "WT_30mM-Pi_2-W",
                                                                     ifelse(myoV$conditions == "WT 30mM Pi_3W", "WT_30mM-Pi_3-W", "un-labeled"
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
                                  )
                           )
)





myoV$conditions2 <- factor(myoV$conditions2, levels = c("WT_0mM-Pi_1.5-W",
                                                        "WT_30mM-Pi_1.5-W",
                                                        "WT_30mM-Pi_2-W",
                                                        "WT_30mM-Pi_3-W",
                                                        "S217A_0mM-Pi_1.5-W",
                                                        "S217A_30mM-Pi_1.5-W",
                                                        "S217A_30mM-Pi_2-W",
                                                        "S217A_30mM-Pi_3-W"))


# define a helper function
splitConditions <- function(x, n){
  strsplit(x, split = "_", fixed = TRUE)[[1]][[n]]
}

# add new columns
myoV$myo <- sub("_.*", "", myoV$conditions2)
myoV$prettyConditions <- gsub("_", " ", myoV$conditions2)
myoV$prettyConditions <- factor(myoV$prettyConditions, levels = gsub("_", " ", c("WT_0mM-Pi_1.5-W",
                                                                                 "WT_30mM-Pi_1.5-W",
                                                                                 "WT_30mM-Pi_2-W",
                                                                                 "WT_30mM-Pi_3-W",
                                                                                 "S217A_0mM-Pi_1.5-W",
                                                                                 "S217A_30mM-Pi_1.5-W",
                                                                                 "S217A_30mM-Pi_2-W",
                                                                                 "S217A_30mM-Pi_3-W")))


myoV$myo <- factor(myoV$myo, levels = c("WT", "S217A"))
myoV$pi  <- sapply(as.character(myoV$conditions2), splitConditions, n = 2)
myoV$power <- sapply(as.character(myoV$conditions2), splitConditions, n = 3)
myoV$piPower <- paste0(myoV$pi, "_", myoV$power)
myoV$power2  <- sub("-.*", "", myoV$power)
myoV$velocity  <- myoV$displacement_nm / myoV$time_on_ms
myoV$pn_nm <- ifelse(myoV$power2 == 1.5, "0.04 \n pN/nm",
                     ifelse(myoV$power2 == 2, "0.06 \n  pN/nm",
                            ifelse(myoV$power2 == 3, "0.1 \n pN/nm", NA)))

myoV$pn_nm2 <- ifelse(myoV$power2 == 1.5, 0.04,
                      ifelse(myoV$power2 == 2, 0.06,
                             ifelse(myoV$power2 == 3, 0.1, NA)))

myoV$pn_nm3 <- ifelse(myoV$power2 == 1.5, "0.04 pN/nm",
                      ifelse(myoV$power2 == 2, "0.06 pN/nm",
                             ifelse(myoV$power2 == 3, "0.1 pN/nm", NA)))

myoV$force <- myoV$displacement_nm*myoV$pn_nm2

# get some pretty colors
reds  <- RColorBrewer::brewer.pal(9, "Reds")
blues  <- RColorBrewer::brewer.pal(9, "Blues")

plot_colors <- c(blues[[4]], blues[[5]], blues[[7]], blues[[9]],
                 reds[[4]], reds[[5]], reds[[7]], reds[[9]])

plot_colors2 <- c(blues[[5]], blues[[5]], blues[[7]], blues[[9]],
                  reds[[5]], reds[[5]], reds[[7]], reds[[9]])






######################################################################

gg1 <-
ggplot(myoV)+ 
  geom_histogram(aes(x=force, y=stat(density), fill=conditions2),
                 color="black",
                 show.legend=FALSE
                 )+
  ggtitle("Forces")+
  xlab("piconewtons")+
  facet_grid(pi+pn_nm3~myo)+
  scale_fill_manual(values=plot_colors2)+
  theme_linedraw()+
  theme(panel.grid=element_blank())

gg2 <-
ggplot(myoV)+ 
  geom_histogram(aes(x=displacement_nm, y=stat(density), fill=conditions2),
                 color="black",
                 show.legend=FALSE
  )+
  ggtitle("Displacements")+
  xlab("nanometers")+
  facet_grid(pi+pn_nm3~myo)+
  scale_fill_manual(values=plot_colors2)+
  theme_linedraw()+
  theme(panel.grid=element_blank())

plot_grid(gg1, gg2)

ggsave