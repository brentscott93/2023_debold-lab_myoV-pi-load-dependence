# Some of the code/style might look different here
# I challenged myself to use as much base-R as possible in this script
# I tried to avoid any {tidyverse} related functions
# ggplot2 is an exception
# this was just for fun...

library(data.table)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(magick)
library(ggtext)
library(drc)
library(readxl)
library(randomcoloR)
library(dplyr)
library(ggpubr)
library(svglite)

# read in data
myoV <- fread("data/MyoV_Pi_Rebind_Events6.csv")

# get some pretty colors
reds  <- RColorBrewer::brewer.pal(9, "Reds")
blues  <- RColorBrewer::brewer.pal(9, "Blues")
# rename conditions for consistent style/format
# MYOSIN_PI_POWER
## myoV$conditions2 <- ifelse(myoV$conditions == "S217A", "S217A_0mM-Pi_1.5-W",
##                      ifelse(myoV$conditions == "S217A 30mM Pi","S217A_30mM-Pi_1.5-W",
##                       ifelse(myoV$conditions == "S217A 30mM Pi 2W", "S217A_30mM-Pi_2-W",
##                        ifelse(myoV$conditions == "S217A 30mM Pi_3W", "S217A_30mM-Pi_3-W",
##                         ifelse(myoV$conditions == "WT", "WT_0mM-Pi_1.5-W",
##                          ifelse(myoV$conditions == "WT 30mM Pi", "WT_30mM-Pi_1.5-W",
##                           ifelse(myoV$conditions == "WT 30mM Pi 2W", "WT_30mM-Pi_2-W",
##                            ifelse(myoV$conditions == "WT 30mM Pi_3W", "WT_30mM-Pi_3-W", "un-labeled"
##                            )
##                           )
##                          )
##                         )
##                        )
##                       )
##                      )
##                     )





## myoV$conditions2 <- factor(myoV$conditions2, levels = c("WT_0mM-Pi_1.5-W",
##                                                         "WT_30mM-Pi_1.5-W",
##                                                         "WT_30mM-Pi_2-W",
##                                                         "WT_30mM-Pi_3-W",
##                                                         "S217A_0mM-Pi_1.5-W",
##                                                         "S217A_30mM-Pi_1.5-W",
##                                                         "S217A_30mM-Pi_2-W",
##                                                         "S217A_30mM-Pi_3-W"))
myoV$myo <- myoV$`myosin type`
myoV$myo <- factor(myoV$myo, levels = c("WT", "S217A"))
myoV$Load <- factor(myoV$Load, levels = c("Low", "Med", "High"))
myoV$pi_mM <- paste0(myoV$Pi, "mM-Pi")


colz <- c(blues[4], blues[6], blues[8], reds[4], reds[6], reds[8])

myoV$myo_load <- paste(myoV$myo, myoV$Load, sep = "-")
myoV$myo_load <- factor(myoV$myo_load, levels = c("WT-Low",
                                                  "WT-Med",
                                                  "WT-High",
                                                  "S217A-Low",
                                                  "S217A-Med",
                                                  "S217A-High"))

myoV$pi_mM <- paste0(myoV$Pi, "mM-Pi")

# define a helper function
## splitConditions <- function(x, n){
##         strsplit(x, split = "_", fixed = TRUE)[[1]][[n]]
## }

## # add new columns
## myoV$myo <- sub("_.*", "", myoV$conditions2)
## myoV$prettyConditions <- gsub("_", " ", myoV$conditions2)
## myoV$prettyConditions <- factor(myoV$prettyConditions, levels = gsub("_", " ", c("WT_0mM-Pi_1.5-W",
##                                                                         "WT_30mM-Pi_1.5-W",
##                                                                         "WT_30mM-Pi_2-W",
##                                                                         "WT_30mM-Pi_3-W",
##                                                                         "S217A_0mM-Pi_1.5-W",
##                                                                         "S217A_30mM-Pi_1.5-W",
##                                                                         "S217A_30mM-Pi_2-W",
##                                                                         "S217A_30mM-Pi_3-W")))


## myoV$myo <- factor(myoV$myo, levels = c("WT", "S217A"))
## myoV$pi  <- sapply(as.character(myoV$conditions2), splitConditions, n = 2)
## myoV$power <- sapply(as.character(myoV$conditions2), splitConditions, n = 3)
## myoV$piPower <- paste0(myoV$pi, "_", myoV$power)
## myoV$power2  <- sub("-.*", "", myoV$power)
## myoV$velocity  <- myoV$displacement_nm / myoV$time_on_ms
## myoV$pn_nm <- ifelse(myoV$power2 == 1.5, "0.04 \n pN/nm",
##                 ifelse(myoV$power2 == 2, "0.06 \n  pN/nm",
##                   ifelse(myoV$power2 == 3, "0.1 \n pN/nm", NA)))

## myoV$pn_nm2 <- ifelse(myoV$power2 == 1.5, 0.04,
##                      ifelse(myoV$power2 == 2, 0.06,
##                             ifelse(myoV$power2 == 3, 0.1, NA)))

## myoV$pn_nm3 <- ifelse(myoV$power2 == 1.5, "0.04 pN/nm",
##                      ifelse(myoV$power2 == 2, "0.06 pN/nm",
##                             ifelse(myoV$power2 == 3, "0.1 pN/nm", NA)))

## myoV$force <- myoV$displacement_nm*myoV$pn_nm2

# ned <- 
#   myoV %>% 
#   dplyr::select(myo, pi, power, displacement_nm, time_on_ms, force)
# 
# fwrite(ned, "data/ned.csv")

# myoV$force_conversion <-  myoV$force/myoV$displacement_nm
# hm <- myoV[, .(conditions, date, obs, displacement_nm, force, force_conversion)]
# summarize data {data.table} style
# this does the same as group_by()%>%summarize()
dataSummary <- myoV[, 
                    .(displacementAvg = mean(displacement_nm),
                      displacementSD = sd(displacement_nm), 
                      displacementSE = (sd(displacement_nm)/sqrt(.N)),
                      forceAvg = mean(force),
                      forceSE = (sd(force)/sqrt(.N)),
                      forceSD = sd(force),
                      timeOnAvg = mean(time_on_ms),
                      timeOnSD = sd(time_on_ms),
                      timeOnSE = (sd(time_on_ms)/sqrt(.N)),
                      rateAvg =  ( 1 / mean(time_on_ms) ) * 1000,
                      rateSE = (( 1 / sd(time_on_ms )) * 1000) / sqrt(.N),
                      rateSD = (( 1 / sd(time_on_ms)) * 1000),
                      timeOffAvg = mean(time_off_ms, na.rm = TRUE),
                      timeOffSD = sd(time_off_ms, na.rm = TRUE),
                      trapStiffnessAvg = mean(trap_stiffness, na.rm = TRUE)),
                      ## velocityAvg = mean(velocity, na.rm = TRUE),
                      ## velocitySE = (sd(velocity)/sqrt(.N))),
                    by = list(myo, Pi, Load, pi_mM, myo_load )]



plot_colors <- c(blues[[4]], blues[[6]], blues[[8]],
                 reds[[4]], reds[[6]], reds[[8]])

## plot_colors2 <- c(blues[[5]], blues[[5]], blues[[7]], blues[[9]],
##                  reds[[5]], reds[[5]], reds[[7]], reds[[9]])

## plot_colors3 <- c("grey40", blues[[5]], blues[[7]], blues[[9]],
##                  "grey40", reds[[5]], reds[[7]], reds[[9]])

## plot_colors4 <- c( blues[[5]], blues[[7]], blues[[9]],
##                    blues[[5]], blues[[5]], blues[[7]], blues[[9]],
##                     reds[[5]], reds[[5]], reds[[7]], reds[[9]]
##                    )

# FIG 1
#### TRAP TRACES #### 
wt_con_trace_1.5w <- readRDS("traces/wt-con-1.5watts_2021-02-08_obs-04_13.3539-18.8743.rds")
wt_con_trace_1.5w$layers[[1]] <- NULL
(trace1 <- 
    wt_con_trace_1.5w + 
    geom_line(linewidth = 0.3)+
    coord_cartesian(xlim = c(0, 5.25), ylim = c(-100, 100))+
    scale_color_manual(values = c("black", rep(blues[[5]], 15)))+
    scale_x_continuous(breaks = 0:10, expand = expansion(0, 0.2))+
    scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0.01, 0))+
    geom_segment(aes(x = 1.39, xend = 2.1, y = -50, yend=-50), color = "red", size = 0.2,
                 arrow = arrow(length = unit(0.02, "npc"), ends = "first"))+
    geom_segment(aes(x = 2.39, xend = 3.1, y = -50, yend=-50), color = "red", size = 0.2,
                 arrow = arrow(length = unit(0.02, "npc"), ends = "last"))+
    # geom_segment(aes(x = -0.1, y= -80, xend = 0.9, yend=-80),  color = "black")+
    # geom_segment(aes(x = -0.1, y= -80, xend = -0.1, yend=-40), color = "black")+
    annotate("text", x = 2.25, y = -50, label = "t[on]", parse=TRUE, size = 3, vjust = 0.5 )+
    annotate("text", x = 3.05, y = 52, label = "[ ]", size = 4, vjust = 0.5, color = "red" )+
    annotate("text", x = 3.25, y = 53, label = "Peak Displacement", size = 2, vjust = 0.5, hjust = 0)+
    ggtitle("WT 0mM-Pi (0.04 pN/nm)")+
    theme_cowplot()+
    theme(
      plot.title = element_markdown(size = 8, hjust = 0.5 ),
      plot.margin=unit(c(0.1, 0, 0, 0), 'cm'),
      legend.position = "none",
      axis.line = element_blank(),
       axis.text.y = element_blank(),
       axis.text.x = element_blank(),
      axis.ticks = element_blank(),
    #  panel.grid =  element_line(color = "gr", size = 0.1),
      panel.grid =  element_blank()
  )+
  draw_line(x = c(-0.1, 0.9),
            y = -80,
            linewidth = 1)+
  draw_line(x = -0.1,
            y = c(-80, -40),
            linewidth = 1)
)

wt_con_trace_3w <-readRDS("traces/WT_100uMATP_23watts_2022-11-23_obs-20_20.9324-26.2342.rds")
wt_con_trace_3w$layers[[1]] <- NULL
(trace1.1 <- wt_con_trace_3w +
    geom_line(linewidth = 0.3)+
    ## coord_cartesian(c(0, 3.685))+
    scale_color_manual(values = c("black", rep(blues[[8]], 15)))+
    scale_x_continuous(breaks = 0:10, expand = expansion(0, 0.1))+
    scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0.01, 0))+
    ggtitle("WT 0mM-Pi (0.1 pN/nm)")+
    ## annotate("segment", x = -0.1, y= -60, xend = 0.9, yend=-60,  color = "black", size = 1.5)+
    ## annotate("segment", x = -0.1, y= -60, xend = -0.1, yend=-20, color = "black", size = 1 )+
    coord_cartesian(ylim = c(-100, 100))+
    theme_cowplot()+
    theme(
      plot.title = element_markdown(size = 8, hjust = 0.5 ),
      plot.margin=unit(rep(0, 4), 'cm'),
      legend.position = "none",
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      #  panel.grid =  element_line(color = "gr", size = 0.1),
      panel.grid =  element_blank()
  )+
  draw_line(x = c(-0.1, 0.9),
            y = -50,
            linewidth = 1)+
  draw_line(x = -0.1,
            y = c(-50, -10),
            linewidth = 1)
)



wt_pi_trace_1.5w <- readRDS("traces/wt-pi-1.5-watts test_2020-12-19_obs-09_10.0407-15.7875.rds")
wt_pi_trace_1.5w$layers[[1]] <- NULL
(trace2 <- wt_pi_trace_1.5w +
    geom_line(linewidth = 0.3)+
    coord_cartesian(xlim = c(0, 3.685), ylim = c(-100, 100))+
    scale_color_manual(values = c("black", rep(blues[[5]], 15)))+
    scale_x_continuous(breaks = 0:10, expand = expansion(0, 0.09))+
    scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0.01, 0))+
    ggtitle("WT 30mM-Pi (0.04 pN/nm)")+
    ## geom_segment(aes(x = -0.1, y= -60, xend = 0.9, yend=-60),  color = "black")+
    ## geom_segment(aes(x = -0.1, y= -60, xend = -0.1, yend=-20), color = "black")+
    theme_cowplot()+
    theme(
      plot.title = element_markdown(size = 8, hjust = 0.5 ),
      plot.margin=unit(rep(0, 4), 'cm'),
      legend.position = "none",
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      #  panel.grid =  element_line(color = "gr", size = 0.1),
      panel.grid =  element_blank()
  )+
  draw_line(x = c(-0.08, 1.08),
            y = -60,
            linewidth = 1)+
  draw_line(x = -0.08,
            y = c(-60, -20),
            linewidth = 1)
)

wt_pi_trace_3w <- readRDS("traces/wt-pi-test-3-watts_2021-06-04_obs-03_10.5301-16.7364.rds")
wt_pi_trace_3w$layers[[1]] <- NULL
wt_pi_3w_data <- dplyr::filter(wt_pi_trace_3w$data, new_time_index >= 5000*0.09)
wt_pi_trace_3w$data <- wt_pi_3w_data
(trace3 <- wt_pi_trace_3w +
          geom_line(size = 0.3)+
          coord_cartesian(xlim = c(0.09, 5.04), ylim = c(-100, 100))+
          scale_color_manual(values = c("black", rep(blues[[8]], 15)))+
          scale_x_continuous(
           # limits = c(0.1, 5.04),
                             breaks = 0:10, expand = expansion(0, 0.1))+
          scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0.01, 0))+
             # geom_segment(aes(x = 0.09, y= -60, xend = 1.1, yend=-60),  color = "black")+
         # geom_segment(aes(x = 0, y= -60, xend = 0, yend=-20), color = "black")+
    # annotate(x = 0, y= -70, xend = 1, yend=-70, geom="segment")+
    # annotate(x = 0, y= -70, xend = 0, yend=-30, geom="segment")+
          ggtitle("WT 30mM-Pi (0.1 pN/nm)")+
    theme_cowplot()+
          theme(
            plot.title = element_markdown(size = 8, hjust = 0.5 ),
            plot.margin=unit(rep(0, 4), 'cm'),
            legend.position = "none",
            axis.line = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            #  panel.grid =  element_line(color = "gr", size = 0.1),
            panel.grid =  element_blank()
) +
  draw_line(x = c(0, 1),
            y = -70,
            linewidth = 1)+
  draw_line(x = 0,
            y = c(-70, -30),
            linewidth = 1)
)


wt_trace <- plot_grid(trace1, trace2, trace1.1, trace3, ncol = 1, labels = c("b", "", "", ""))

# wt_pi_trace_3w_zoom <- readRDS("traces/wt-pi-test-3-watts_2021-06-04_obs-03_13.2451-13.4092.rds")
# (trace3_zoom <- wt_pi_trace_3w_zoom +
#  #         ggtitle("WT 30mM-Pi", subtitle = "Medium Power - 0.1 pN/nm")+
#         scale_color_manual(values = c("black", rep(plot_colors[[3]], 15)))+
#         scale_x_continuous(breaks  = seq(0, 1, by = 1/20))+
#         scale_y_continuous(breaks  = seq(-40, 100, by = 40))+
#         xlab("seconds")+
#         ylab("nanometers")+
#         theme_linedraw()+
#         theme(
#                 plot.title = element_text(size = 12),
#                 panel.border = element_rect(color = "grey50", linetype = "dashed", size = 1), 
#                 legend.position = "none"
#         )
#         )
# 




#### s217a traces ####

# s217a_con <- readRDS("traces/s217a 0mM Pi_2020-06-22_obs-01_3.5688-14.4825.rds")
# s217a_con$layers[[1]] <- NULL
# s217a_con+
#   geom_line(size = 0.3)+
#   coord_cartesian(c(1, 6))+
#   scale_x_continuous(expand = expansion(0, 0))
#   

# s217a_con2<- readRDS("traces/s217a 0mM Pi_2020-06-27_obs-05_1.2611-7.482.rds")
# s217a_con2$layers[[1]] <- NULL
# 
# s217a_con2 <- 
# s217a_con2+
#   geom_line(size = 0.3)+
#   coord_cartesian(c(2, 6))+
#     scale_x_continuous(breaks = 0:10, expand = expansion(0, 0.1))+
#     scale_y_continuous(breaks = seq(-100, 120, by = 20), expand = expansion(0, 0))+
#   ggtitle("S217A 0mM-P<sub>i</sub> (0.02 pN/nm)")+
#   theme(
#     plot.title = element_markdown(size = 8, hjust = 0.5 ),
#     plot.margin=unit(c(0.1, 0, 0, 0), 'cm')
#   )

s217a_con <- readRDS("traces/S217A 100uM ATP_2022-05-06_obs-06_14.3674-19.3939.rds")
s217a_con$layers[[1]] <- NULL
(s217a_con <-
  s217a_con+
  geom_line(size = 0.3)+
  coord_cartesian(xlim = c(0, 5), ylim = c(-100, 100))+
    scale_x_continuous(breaks = 0:10, expand = expansion(0.01, 0.1))+
  scale_y_continuous(breaks = seq(-121, 120, by = 40), expand = expansion(0.01, 0))+
  scale_color_manual(values = c("black", rep(reds[[5]], 100)))+
  ggtitle("S217A 0mM-Pi (0.04 pN/nm)")+
  theme_cowplot()+
  theme(
    plot.title = element_markdown(size = 8, hjust = 0.5 ),
    plot.margin=unit(c(0.1, 0, 0, 0), 'cm'),
    legend.position = "none",
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    #  panel.grid =  element_line(color = "gr", size = 0.1),
    panel.grid =  element_blank()
)+
  draw_line(x = c(-0.1, 0.9),
            y = -100,
            linewidth = 1)+
  draw_line(x = -0.1,
            y = c(-100, -60),
            linewidth = 1)
)


s217a_con_3w <- readRDS("traces/s217a_100uMATP_3watts_2022-05-06_obs-15_12.5538-17.4733.rds")
s217a_con_3w$layers[[1]] <- NULL
(s217a_con_3w <-
  s217a_con_3w+
  geom_line(linewidth = 0.3)+
  coord_cartesian(xlim = c(0, 2.1), ylim = c(-100, 100))+
    scale_x_continuous(breaks = 0:10, expand = expansion(0.01, 0.1))+
  scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0.01, 0))+
  scale_color_manual(values = c("black", rep(reds[[8]], 100)))+
  ggtitle("S217A 0mM-Pi (0.1 pN/nm)")+
  theme_cowplot()+
  theme(
    plot.title = element_markdown(size = 8, hjust = 0.5 ),
    plot.margin=unit(c(0.1, 0, 0, 0), 'cm'),
    legend.position = "none",
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    #  panel.grid =  element_line(color = "gr", size = 0.1),
    panel.grid =  element_blank()
)+
  draw_line(x = c(-0.1, 0.9),
            y = -80,
            linewidth = 1)+
  draw_line(x = -0.1,
            y = c(-80, -40),
            linewidth = 1)
)


s217a_pi <- readRDS("traces/s217a 30mM Pi_2020-06-24_obs-02_3.2593-9.7281.rds")
s217a_pi$layers[[1]] <- NULL
(s217a_pi <-
  s217a_pi+
  geom_line(size = 0.3)+
  #coord_cartesian(c(0, 4))+
  coord_cartesian(ylim = c(-100, 100))+
  scale_x_continuous(breaks = 0:10, expand = expansion(0.01, 0.1))+
  scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0.01, 0))+
  scale_color_manual(values = c("black", rep(reds[[5]], 100)))+
    ggtitle("S217A 30mM-Pi (0.04 pN/nm)")+
    # geom_segment(aes(x = -0.1, y= -70, xend = 0.9, yend=-70, size="1"), color = "black")+
    # geom_segment(aes(x = -0.1, y= -70, xend = -0.1, yend=-30,  size="1"), color = "black")+
    # scale_size_manual(values = 1)+
    theme_cowplot()+
    theme(
      plot.title = element_markdown(size = 8, hjust = 0.5 ),
      plot.margin=unit(rep(0, 4), 'cm'),
      legend.position = "none",
      axis.line = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      #  panel.grid =  element_line(color = "gr", size = 0.1),
      panel.grid =  element_blank()
) +
  draw_line(x = c(-0.1, 0.9),
            y = -75,
            linewidth = 1)+
  draw_line(x = -0.1,
            y = c(-75, -35),
            linewidth = 1)
)

s217a_pi_3w <- readRDS("traces/S217A 30mM Pi_3W_2022-01-08_obs-17_5.7081-11.0694.rds")
s217a_pi_3w$layers[[1]] <- NULL
(s217a_pi_3w <-
s217a_pi_3w+
  geom_line(size = 0.3)+
  coord_cartesian(xlim = c(0, 5), ylim = c(-100, 100))+
  #  geom_segment(data = NULL, aes(x = 0, y = -60, xend = 1, yend = -60), color = "black")
  scale_x_continuous(breaks = 0:10, expand = expansion(0.01, 0.2))+
  scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0.01, 0))+
  scale_color_manual(values = c("black", rep(reds[[8]], 100)))+
  ggtitle("S217A 30mM-Pi (0.1 pN/nm)")+
    # geom_segment(aes(x = -0.1, y= -65, xend = 0.9, yend=-65, size="1"), color = "black")+
    # geom_segment(aes(x = -0.1, y= -65, xend = -0.1, yend=-25,  size="1"), color = "black")+
   # scale_size_manual(values = 1)+
    theme_cowplot()+
  theme(
    plot.title = element_markdown(size = 8, hjust = 0.5 ),
    plot.margin=unit(rep(0, 4), 'cm'),
    legend.position = "none",
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    #  panel.grid =  element_line(color = "gr", size = 0.1),
    panel.grid =  element_blank()
)+
  draw_line(x = c(0.9, -0.1, -0.1),
            y = c(-65, -65, -25),
            linewidth = 1)
  ## draw_line(x = -0.1,
  ##           y = c(-65, -25))
)
  
  
s217a_trace <- plot_grid(s217a_con,
                         s217a_pi,
                         s217a_con_3w,
                         s217a_pi_3w,
                         ncol = 1,
                         labels = c("c", "", "", ""))


(raw <- plot_grid(wt_trace, s217a_trace, ncol =  2))

trapSchematic  <- image_ggplot(image_read("traps2.png"))


png(filename = "figs/figure-1-alt.png", width = 6.5, height = 7, units = "in", res = 1200)
plot_grid(trapSchematic, raw, nrow = 2, rel_heights = c(1.25, 2), labels = c("a", ""))
dev.off()


svg(filename = "figs/figure-1-alt.svg", width = 6.5, height = 7)
plot_grid(trapSchematic, raw, nrow = 2, rel_heights = c(1.25, 2), labels = c("a", ""))
dev.off()

svg(filename = "figs/figure-1.svg", width = 7, height = 4)
plot_grid(trapSchematic, raw, nrow = 1, rel_widths = c(1, 2), labels = c("a", ""))
dev.off()

png(filename = "figs/figure-1.png", width = 7, height = 4, units = "in", res = 1200)
plot_grid(trapSchematic, raw, nrow = 1, rel_widths = c(1, 2), labels = c("a", ""))
dev.off()



#### FIG 2 ####

#######################################################################
# Cumulative Distributions
#######################################################################
colz <- c(blues[4], blues[6], blues[8], reds[4], reds[6], reds[8])

timeOnECDF  <- myoV[, .(timeOn = unique(time_on_ms),
                        ecdf = ecdf(time_on_ms)(unique(time_on_ms))),
                    by = list(myo, Pi, Load)]

timeOnECDF$myo_load <- paste(timeOnECDF$myo, timeOnECDF$Load, sep = "-")
timeOnECDF$myo_load <- factor(timeOnECDF$myo_load, levels = c("WT-Low",
                                                              "WT-Med",
                                                              "WT-High",
                                                              "S217A-Low",
                                                              "S217A-Med",
                                                              "S217A-High"))

timeOnECDF$pi_mM <- paste0(timeOnECDF$Pi, "mM-Pi")

(ggTimeOnECDF  <- 
    ggplot(timeOnECDF, aes(timeOn, ecdf)) +
    geom_step(aes(color = myo_load, linetype = pi_mM), linewidth = 0.3, alpha = 0.7)+
    ## annotate("rect", xmin = 0, xmax = 800, ymin = 0, ymax = 1, fill = "grey50", color = NA, alpha = 0.2)+
    ## facet_grid(myo~pi_mM)+
    scale_color_manual(values = colz)+
    coord_cartesian(xlim = c(0, 3000), ylim = c(0, 1))+
   ylab("Cumulative Probability")+
   xlab("Time (ms)")+
   ## ggtitle("Attachment Times (distribution)")+
    theme_cowplot(12)+
    theme(
     panel.grid = element_blank(),
     legend.position = "none"
     )
)


(ggTimeOnECDF2 <-
    ggplot(timeOnECDF, aes(timeOn, ecdf)) +
    geom_step(aes(color = myo_load, linetype = pi_mM), linewidth = 0.3)+
    facet_grid(myo~pi_mM)+
    scale_color_manual(values = colz)+
    coord_cartesian(xlim = c(0, 1000), ylim = c(0, 1))+
   ylab("Cumulative Probability")+
   xlab("Time (ms)")+
    theme_cowplot(12)+
    theme(
     panel.grid = element_blank(),
     legend.position = "none",
     strip.background = element_rect(fill = "transparent")
     )
)

wt_0 <- myoV[myo == "WT" & pi_mM == "0mM-Pi" & Load %in% c("Low", "High")]
wt_30 <- myoV[myo == "WT" & pi_mM == "30mM-Pi" & Load %in% c("Low", "High")]
s217a_0 <- myoV[myo == "S217A" & pi_mM == "0mM-Pi" & Load %in% c("Low", "High")]
s217a_30 <- myoV[myo == "S217A" & pi_mM == "30mM-Pi" & Load %in% c("Low", "High")]

plot_hist <- function(dat, colors){
ggplot(dat)+
  geom_histogram(aes(x = time_on_ms,
                     y = after_stat(density),
                     fill = Load),
                 color = "black",
                 binwidth = 50,
                 show.legend = FALSE)+
  facet_grid(~Load, labeller = as_labeller(c("Low" = "0.04 pN/nm", "High" = "0.1 pN/nm")))+
  coord_cartesian(xlim = c(0, 1000))+
  scale_y_continuous(expand = expansion(mult = c(0, NA), add = c(0, NA)))+
  scale_x_continuous(breaks = c(0, 300, 600, 900))+
  scale_fill_manual(values = colors)+
  ylab("PDF")+
  xlab("")+
  theme_cowplot(10)+
  theme(
   strip.background = element_rect(fill = "transparent"),
   axis.text.y = element_blank(),
   axis.text.x = element_text(size = 8)
   )
}

gg_ton_wt0 <- plot_hist(wt_0, colors = c(plot_colors[[1]], plot_colors[[3]]))
gg_ton_wt30 <- plot_hist(wt_30, colors = c(plot_colors[[1]], plot_colors[[3]]))
gg_ton_s2170 <- plot_hist(s217a_0, colors = c(plot_colors[[4]], plot_colors[[6]]))
gg_ton_s21730 <- plot_hist(s217a_30, colors = c(plot_colors[[4]], plot_colors[[6]]))

fig2_right <-
ggdraw(ggTimeOnECDF2)+
  draw_plot(gg_ton_wt0, 0.2, 0.5, 0.3, 0.35)+
  draw_plot(gg_ton_wt30, 0.6, 0.5, 0.3, 0.35)+
  draw_plot(gg_ton_s2170, 0.2, 0.09, 0.3, 0.35)+
  draw_plot(gg_ton_s21730, 0.6, 0.09, 0.3, 0.35)

## fig_ton_right <- ggdraw(ggTimeOnECDF)+
##   draw_plot(ggTimeOnECDF2, 0.25, 0.15, 0.7, 0.7 )

(ggTimeOn  <-
    ggplot()+
    geom_errorbar(data = dataSummary,
                  aes(x = pi_mM,
                      y = timeOnAvg,
                      ymax = timeOnAvg + timeOnSE,
                      ymin = timeOnAvg - timeOnSE,
                      color = myo_load),
                  position = position_dodge(width = 0.9),
                  width = 0.3)+
                  ## position = position_dodge2(width = NULL,
                  ##                            preserve = "single",
                  ##                            padding = 0.5))+
    geom_col(data = dataSummary,
             aes(x = pi_mM,
                 y = timeOnAvg,
                 fill = myo_load),
             position = position_dodge(width = 0.9),
             ## position = position_dodge2(width = NULL,
             ##                            preserve = "single",
             ##                            padding = 0),
             show.legend = FALSE)+
    facet_wrap(~myo, nrow = 1)+
    xlab("")+
    ylab("Time (ms)")+
    ## ggtitle("Attachment Times (average)")+
    scale_fill_manual(values = colz, name = "")+
    scale_color_manual(values = colz, name = "")+
    scale_y_continuous(expand = expansion(0, c(0, 10)))+
    theme_cowplot(12)+
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "transparent"),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(size = 8)
    )
  )



fig_ton <- plot_grid(fig2_right,
                     plot_grid(NA, ggTimeOn, NA, nrow = 1, rel_widths = c(0.15, 0.5, 0.15)),
                      ncol = 1, rel_heights = c(0.6, 0.4), labels = c("a", "b") )
fig_ton_title <- ggdraw() +
  draw_label(
    "Attachment Times",
    fontface = 'bold',
    size = 12,
    x = 0.5,
    hjust = 0.5
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 0)
  )


## ggplot(myoV)+
##   geom_histogram(aes(x = time_on_ms, y = stat(density), fill = myo_load), color = "black", bins = 250)+
##   scale_fill_manual(values = colz)+
##   ylab("PDF")+
##   coord_cartesian(c(0, 1000))+
##   ## scale_x_log10()+
##   facet_grid(pi_mM+Load~myo, scales = "free_x")+
##   theme_cowplot()+
##   theme(
##    axis.text.y = element_blank(),
##    axis.ticks.y = element_blank()
##   )

## ggsave("figs/dumb-plot2.png")

png(filename = "figs/figure-2.png", height = 7, width = 7, res = 1200, units = "in")
plot_grid(fig_ton_title, fig_ton, rel_heights = c(0.05, 1.1), nrow = 2)
dev.off()


svg(filename = "figs/figure-2.svg", height = 3, width = 7)
plot_grid(fig_ton_title, fig_ton, rel_heights = c(0.1, 1.1), nrow = 2)
dev.off()


# FORCES
#####################################################################
##### FIG 3 #####
(ggForce  <- 
        ggplot()+
        geom_errorbar(data = dataSummary,
                      aes(x = pi_mM,
                          y = forceAvg,
                          ymax =  forceAvg + forceSE,
                          ymin = forceAvg - forceSE,
                          color = myo_load),
                      width = 0.3,
                      position = position_dodge(width = 0.9))+
        geom_col(data = dataSummary,
                 aes(x = pi_mM,
                     y = forceAvg,
                    fill = myo_load,
                    color = myo_load),
#                   color = "black",
                    position = position_dodge(width = 0.9))+
  facet_grid(~myo)+
    scale_fill_manual(values = colz, name = "")+
    scale_color_manual(values = colz, name = "")+
  ## ggtitle("Forces")+
  ylab("Force (pN)")+
  xlab("")+
    scale_y_continuous(expand = expansion(0, c(0, 0.1)))+
    theme_cowplot(14)+
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "transparent"),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(size = 10)
    )
  )



(ggDispl  <-
        ggplot()+
        geom_errorbar(data = dataSummary,
                      aes(x = pi_mM,
                          y = displacementAvg,
                          ymax =  displacementAvg + displacementSE,
                          ymin = displacementAvg - displacementSE,
                          color = myo_load),
                      width = 0.3,
                      position = position_dodge(width = 0.9))+
        geom_col(data = dataSummary,
                 aes(x = pi_mM,
                     y = displacementAvg,
                    fill = myo_load,
                    color = myo_load),
#                   color = "black",
                    position = position_dodge(width = 0.9))+
  facet_grid(~myo)+
    scale_fill_manual(values = colz, name = "")+
    scale_color_manual(values = colz, name = "")+
  ## ggtitle("Forces")+
  ylab("Displacement (nm)")+
  xlab("")+
    scale_y_continuous(expand = expansion(c(0, 0.1), c(0, 1)))+
    theme_cowplot(14)+
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "transparent"),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(size = 10)
    )
  )


ggtrapForces  <- image_ggplot(image_read("trap-force-500dpi.png"))

equiData <- fread("data/Equipartitions_traces.csv")

( ggEqui <- ggplot(data = equiData, aes(x = 1:length(processed_bead1.5)/5000))+
        geom_line(aes(y = processed_bead1.5), color = "black")+
        geom_line(aes(y = processed_bead2), color = "hotpink3")+
        geom_line(aes(y = processed_bead3), color = "grey50")+
        coord_cartesian(xlim = c(0, 2), ylim = c(-80, 80))+
        theme_cowplot(12)+
        xlab("seconds")+
        ylab("nanometers")+
        theme(
               axis.line = element_blank()
              # axis.text.x = element_blank(),
              # axis.ticks.x = element_blank()
        )
        )


equi1.5 <- data.frame(x = -100:100,
                      y = dnorm(-100:100, 
                                mean(equiData$processed_bead1.5, na.rm = T), 
                                sd = sd(equiData$processed_bead1.5, na.rm = T)),
id = 1.5)


equi2 <- data.frame(x = -90:90,
                      y = dnorm(-90:90, 
                                mean(equiData$processed_bead2, na.rm = T), 
                                sd = sd(equiData$processed_bead2, na.rm = T)),
id = 2)

equi3 <- data.frame(x = -50:50,
                      y = dnorm(-50:50, 
                                mean(equiData$processed_bead3, na.rm = T), 
                                sd = sd(equiData$processed_bead3, na.rm = T)),
id = 3)


equiCurve <- rbind(equi1.5, equi2, equi3)

(ggEquiCurve <- ggplot(data = equiCurve)+
       geom_line(aes(x = x, y = y, color = as.factor(id)), size = 1.5)+
       scale_y_log10()+
       coord_flip(xlim = c(-80, 80), ylim = c(1e-4, 0.1))+
       annotate("text", x = 70, y = Inf, label = "0.04 pN/nm", color = "black", hjust = 1, size = 8/.pt)+
       annotate("text", x = 55, y = Inf, label = "0.06 pN/nm", color = "hotpink3", hjust = 1, size = 8/.pt)+
    annotate("text", x = 40, y = Inf, label = "0.1 pN/nm", color = "grey50", hjust = 1, size = 8/.pt)+
       scale_color_manual(values = c("black", "hotpink3", "grey50"))+
       theme_cowplot(12)+
       ylab("log(PDF)")+
       xlab("")+
        theme(
          plot.margin=unit(c(7, 7, 7, 0), 'pt'),
              axis.line.y = element_blank(),
              axis.text.x = element_text(color = "white"),
          axis.title.y = element_blank(),
              #axis.line.x = element_blank(),
              #axis.ticks.x = element_line(color = "transparent"),
              legend.position = "none"
        )
       )

(fig3Top <- plot_grid(ggtrapForces, ggEqui, ggEquiCurve, labels = "auto", rel_widths = c(0.3, 0.7, 0.3), nrow = 1))

( fig3Bottom <- plot_grid(ggDispl, ggForce, labels = c("d", "e")))
( fig3 <- plot_grid(fig3Top, fig3Bottom, nrow = 2, rel_heights = c(0.4, 0.6))) 

fig3_table <- dataSummary
fig3_table[, heads := round(displacementAvg/7, 1)]
fig3_table[, forceHead := round(forceAvg/heads, 2)]
fig3_table[, `:=`(Myosin = myo,
                  "Pi (mM)" = Pi,
                  "Stiffness (pN/nm)" = ifelse(Load=="Low", 0.04,
                                               ifelse(Load=="Med", 0.06,
                                                      ifelse(Load=="High", 0.1, NA ))),
                  "Displacement (nm)" = paste0(round(displacementAvg, 1), " ± ", round(displacementSE, 1)),
                  "Force (pN)" = paste0(round(forceAvg, 2), " ± ", round(forceSE, 2)),
                  "Heads" = heads,
                  "Force/Head (pN)" = forceHead)]

fig3_table$Myosin <- factor(fig3_table$Myosin, levels = c("WT", "S217A"))

table_colors <- c(rep(colz[1:3], 2), rep(colz[4:6], 2))


(ggfig3table <-
  fig3_table[, -c(1:22)] %>%
    dplyr::arrange(Myosin, `Pi (mM)`) %>%
    ggpubr::ggtexttable(rows = NULL,
                        theme = ttheme(
                          colnames.style = colnames_style(fill = "white",
                                                          size = 10),
                          tbody.style = tbody_style(fill = alpha(table_colors, 0.6),
                                                    size = 11)
                        )))


png(filename = "figs/figure-3.png", width = 7, height = 8, units = "in", res = 1200)
plot_grid(fig3, ggfig3table, nrow = 2, rel_heights = c(0.5, 0.4), labels = c("", "f"))
dev.off()


svg(filename = "figs/figure-3.svg", width = 7, height = 8)
plot_grid(fig3, ggfig3table, nrow = 2, rel_heights = c(0.5, 0.4), labels = c("", "f"))
dev.off()

## ggsave("figs/fig3-displacement-forces.png", bg = "white")


###############################################################################################
# FORCE vs 1/Time-ON
################################################################################################
# myoVForces <- myoV[, .(conditions2, myo, pi, power, force)]

# boxData  <- merge(myoVForces, dataSummary, by = c("conditions2", "myo", "pi", "power"), all.x = TRUE)

# boxData[, rate := 1000*(1/timeOnAvg)]
# veigel equation: k1 = k0 exp(−W/kT) ; work is Force * d. d is a float


load_dep_data <-  dataSummary[, .(data_nest=list(.SD)), by = .(myo, pi_mM)]

fit_bell <- function(dat){
  n <- 4.2
  kT <-  4.10
  fit <- nls(rateAvg ~ k0*exp((forceAvg*d) / 4.10 ), data = dat, start = list(k0=1, d=4))
  predict_fit <- data.frame(y = predict(fit, newdata=data.frame(forceAvg=seq(0, n, by = 0.01))),
                            x = seq(0, n, by=0.01))
  return(list(fit = fit, predict_fit = predict_fit))
}

load_dep_data[, mod := lapply(data_nest, fit_bell)]

## wt0 <- data.frame(myo="WT",
##                   pi="0mM-Pi",
##                   power=1.5,
##                   forceAvg=0)
## kT <-  4.10
## fit <- nls(rateAvg ~ k0*exp((forceAvg*d) / 4.10 ), data = load_dep_data, start = list(k0=1, d=4))

## predict_fit <- data.frame(y = predict(fit, newdata=data.frame(forceAvg=seq(0, 4.1, by = 0.01))),
##                           x = seq(0, 4.1, by=0.01))

## load_dep_data2 <- dataSummary[pi_mM == "30mM-Pi" & myo == "S217A"]
## fit2 <- nls(rateAvg ~ k0*exp((forceAvg*d) / 4.10 ), data = load_dep_data2, start = list(k0=1, d=4))

## predict_fit2 <- data.frame(y = predict(fit2, newdata=data.frame(forceAvg=seq(0, 2.5, by = 0.01))),
##                           x = seq(0, 2.5, by=0.01))


predict_lines <-load_dep_data[, mod[[1]][["predict_fit"]], by = .(myo, pi_mM)]


(ggLoad <-
   ggplot()+
   geom_line(data = predict_lines,
             aes(x = x,
                 y = y,
                 color = myo,
                 linetype = pi_mM),
             linewidth = 0.7, show.legend = FALSE)+
 geom_point(data = dataSummary,
            aes(x = forceAvg,
                y = rateAvg,
                color = myo,
                shape = pi_mM),
                size = 2.5, alpha = 0.5)+
 geom_errorbar(data = dataSummary,
               aes(x = forceAvg,
                   ymin = rateAvg - rateSE,
                  ymax = rateAvg + rateSE,
                  color = myo),
               show.legend = FALSE)+
 geom_errorbarh(data = dataSummary,
                aes(
                    y = rateAvg,
                   xmin = forceAvg - forceSE,
                   xmax= forceAvg + forceSE,
                   color = myo),
                show.legend = FALSE)+
   scale_color_manual(values =  c(colz[2], colz[5]), name = "")+
   scale_shape(name = "")+
   coord_cartesian(ylim = c(0, 13))+
 xlab("Force (pN)")+
 ylab("*k<sub>det</sub>* (s<sup>-1</sup>)")+
 ggtitle("Load Dependence of Detachment")+
   ## scale_linetype_manual(values = c("solid", "dashed"), name = "")+
   theme_cowplot(8)+
   theme(
     legend.position = "top",
     plot.title = element_markdown(size = 8, hjust = 0.5),
     axis.title.y = element_markdown()
  )
)



bell_numbers <- load_dep_data[, .(pars = lapply(mod, `[[`, "fit")), by = .(myo, pi_mM)]

bell_numbers <- bell_numbers[, .(tidy_pars = lapply(pars, broom::tidy)), by = .(myo, pi_mM)]



for(row in 1:nrow(bell_numbers)){
 filename <- paste0("data/", bell_numbers$myo[[row]], "_", bell_numbers$pi_mM[[row]], "_bell-parameters.txt")
 sink(filename)
 print(bell_numbers$tidy_pars[[row]])
 sink()
}

parAll <- bell_numbers[, tidy_pars[[1]], by = .(myo, pi_mM)]
parAll$myo <- factor(parAll$myo, levels = c("WT", "S217A"))
parAll$term2 <- ifelse(parAll$term == "k0", "*k<sub>0</sub>* (s<sup>-1</sup>)", "*d* (nm)")
parAll$term2 <- factor(parAll$term2, levels = c("*k<sub>0</sub>* (s<sup>-1</sup>)", "*d* (nm)"))

bg_fill <- data.frame(pi_mM = c(rep("0mM-Pi", 2), rep("30mm-Pi", 2)),
                      hue = c(rep("transparent", 2), rep("grey50", 2)),
                      term2 = c("*k<sub>0</sub>* (s<sup>-1</sup>)", "*d* (nm)",
                                "*k<sub>0</sub>* (s<sup>-1</sup>)", "*d* (nm)"))

(ggParAll <- 
        ggplot(data=parAll)+
   geom_rect(data = subset(parAll, pi_mM == '30mM-Pi'),
             aes(fill = pi_mM),
             xmin = -Inf,
             xmax = Inf,
            ymin = -Inf,
            ymax = Inf,
            alpha = 0.3) +
         ## geom_rect(data = bg_fill,
        ##           aes(fill = hue),
        ##           xmin = -Inf,
        ##           xmax =
        ##             Inf,
        ##           ymin = -Inf,
        ##           ymax = Inf,
        ##           alpha = 1,
        ##           width = 1) +
       geom_point(aes(x = myo, y = estimate, color = myo), position = "dodge", alpha = 0.6, shape = 16)+
        geom_errorbar(aes(x = myo, 
                          ymax=estimate+std.error, 
                          ymin=estimate-std.error, 
                          color = myo), 
                      position="dodge",
                      width = 0.25)+
        facet_wrap(term2~pi_mM, nrow = 1
                   ## nrow =1,
                   ## strip.position = "top"
                   )+
        ggtitle("Fit Parameters")+
        xlab("")+
        ylab("")+
        scale_color_manual(values = c(blues[[7]], reds[[7]]))+
   scale_fill_manual(values = "grey80")+
        ## scale_y_continuous(breaks = -1:10, expand = expansion(c(0.1, 0.1)))+
        theme_cowplot(8)+
        theme(
         plot.title = element_text(hjust = 0.5),
         axis.text.x = element_markdown(size = 6),
         ## axis.text.y = element_markdown(size = ),
         axis.title.y = element_markdown(),
         legend.position = "none",
         strip.text = element_markdown(),
         strip.text.y.left = element_markdown(),
         strip.background = element_blank(),
         strip.placement = "outside"
        )
)


ggdelta_g  <- image_ggplot(image_read("traces/Figure 4C.pptx.png"))

(fig4top <- plot_grid(ggLoad, ggParAll, nrow = 2, rel_heights = c(0.6, 0.4), labels = c("a", "b")))

(fig4top2 <- plot_grid(ggParAll, ggdelta_g, nrow = 2, rel_heights = c(0.5, 0.6), labels = c("b", "c")))

(fig4top3 <- plot_grid(ggLoad, ggdelta_g, nrow = 1, rel_widths = c(0.6, 0.5), labels = c("a", "c")))


png(filename = "figs/figure-4.png", height = 3.5, width = 6.5, units = "in", res = 1200)
plot_grid(fig4top, ggdelta_g, nrow = 1, labels = c("", "c"), rel_widths = c(0.4, 0.6))
dev.off()



svg(filename = "figs/figure-4.svg", height = 3.5, width = 6.5)
plot_grid(fig4top, ggdelta_g, nrow = 1, labels = c("", "c"), rel_widths = c(0.4, 0.6))
dev.off()



ggsave("figs/fig4-load-dependence-rates.jpg", bg="white")

sink("figs/load-dep-rates.txt")
summary(fit)
summary(fit2)
sink()

