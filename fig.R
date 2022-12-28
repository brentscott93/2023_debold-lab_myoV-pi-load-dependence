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
                      timeOffAvg = mean(time_off_ms, na.rm = TRUE),
                      timeOffSD = sd(time_off_ms, na.rm = TRUE),
                      trapStiffnessAvg = mean(trap_stiffness, na.rm = TRUE),
                      velocityAvg = mean(velocity, na.rm = TRUE),
                      velocitySE = (sd(velocity)/sqrt(.N))),
                    by = list(conditions2, myo, pi, power, power2, piPower, pn_nm, pn_nm2, pn_nm3, prettyConditions)]


# get some pretty colors
reds  <- RColorBrewer::brewer.pal(9, "Reds")
blues  <- RColorBrewer::brewer.pal(9, "Blues")

plot_colors <- c(blues[[4]], blues[[5]], blues[[7]], blues[[9]],
                 reds[[4]], reds[[5]], reds[[7]], reds[[9]])

plot_colors2 <- c(blues[[5]], blues[[5]], blues[[7]], blues[[9]],
                 reds[[5]], reds[[5]], reds[[7]], reds[[9]])

plot_colors3 <- c("grey40", blues[[5]], blues[[7]], blues[[9]],
                 "grey40", reds[[5]], reds[[7]], reds[[9]])

plot_colors4 <- c( blues[[5]], blues[[7]], blues[[9]],
                   blues[[5]], blues[[5]], blues[[7]], blues[[9]],
                    reds[[5]], reds[[5]], reds[[7]], reds[[9]]
                   )

# FIG 1
#### TRAP TRACES #### 
wt_con_trace_1.5w <- readRDS("traces/wt-con-1.5watts_2021-02-08_obs-04_13.3539-18.8743.rds")
wt_con_trace_1.5w$layers[[1]] <- NULL
(trace1 <- 
    wt_con_trace_1.5w + 
    geom_line(size = 0.3)+
    coord_cartesian(c(0, 5.25))+
    scale_color_manual(values = c("black", rep(plot_colors2[[2]], 15)))+
    scale_x_continuous(breaks = 0:10, expand = expansion(0, 0.1))+
    scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0, 0))+
    geom_segment(aes(x = 1.39, xend = 2.1, y = -50, yend=-50), color = "red", size = 0.2,
                 arrow = arrow(length = unit(0.02, "npc"), ends = "first"))+
    geom_segment(aes(x = 2.39, xend = 3.1, y = -50, yend=-50), color = "red", size = 0.2,
                 arrow = arrow(length = unit(0.02, "npc"), ends = "last"))+
    
    # geom_segment(aes(x = -0.1, y= -80, xend = 0.9, yend=-80),  color = "black")+
    # geom_segment(aes(x = -0.1, y= -80, xend = -0.1, yend=-40), color = "black")+
    
    annotate("text", x = 2.25, y = -50, label = "t[on]", parse=TRUE, size = 3, vjust = 0.5 )+
    annotate("text", x = 3.07, y = 52, label = "[ ]", size = 5, vjust = 0.5, color = "red" )+
    annotate("text", x = 3.25, y = 51, label = "Peak Displacement", size = 3, vjust = 0.5, hjust = 0)+
    ggtitle("WT 0mM-P<sub>i</sub> (0.04 pN/nm)")+
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
            y = -80)+
  draw_line(x = -0.1,
            y = c(-80, -40))
)


wt_pi_trace_1.5w <- readRDS("traces/wt-pi-1.5-watts test_2020-12-19_obs-09_10.0407-15.7875.rds")
wt_pi_trace_1.5w$layers[[1]] <- NULL

(trace2 <- wt_pi_trace_1.5w +
    geom_line(size = 0.3)+
    coord_cartesian(c(0, 3.685))+
    scale_color_manual(values = c("black", rep(plot_colors2[[2]], 15)))+
    scale_x_continuous(breaks = 0:10, expand = expansion(0, 0.1))+
    scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0, 0))+
    ggtitle("WT 30mM-P<sub>i</sub> (0.04 pN/nm)")+
    
    geom_segment(aes(x = -0.1, y= -60, xend = 0.9, yend=-60),  color = "black")+
    geom_segment(aes(x = -0.1, y= -60, xend = -0.1, yend=-20), color = "black")+
    
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
            y = -60)+
  draw_line(x = -0.1,
            y = c(-60, -20))
)

wt_pi_trace_3w <- readRDS("traces/wt-pi-test-3-watts_2021-06-04_obs-03_10.5301-16.7364.rds")
wt_pi_trace_3w$layers[[1]] <- NULL
wt_pi_3w_data <- dplyr::filter(wt_pi_trace_3w$data, new_time_index >= 5000*0.09)
wt_pi_trace_3w$data <- wt_pi_3w_data
(trace3 <- wt_pi_trace_3w +
          geom_line(size = 0.3)+
          coord_cartesian(c(0.09, 5.04))+
          scale_color_manual(values = c("black", rep(plot_colors2[[4]], 15)))+
          scale_x_continuous(
           # limits = c(0.1, 5.04),
                             breaks = 0:10, expand = expansion(0.0, 0.1))+
          scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0, 0))+
    
         # geom_segment(aes(x = 0.09, y= -60, xend = 1.1, yend=-60),  color = "black")+
         # geom_segment(aes(x = 0, y= -60, xend = 0, yend=-20), color = "black")+
    # annotate(x = 0, y= -70, xend = 1, yend=-70, geom="segment")+
    # annotate(x = 0, y= -70, xend = 0, yend=-30, geom="segment")+
    # 
  
          ggtitle("WT 30mM-P<sub>i</sub> (0.1 pN/nm)")+
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
            y = -70)+
  draw_line(x = 0,
            y = c(-70, -30))
)


wt_trace <- plot_grid(trace1, trace2, trace3, ncol = 1, labels = c("B", "", ""))

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
  coord_cartesian(c(0, 5))+
    scale_x_continuous(breaks = 0:10, expand = expansion(0, 0.1))+
  scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0, 0))+
  scale_color_manual(values = c("black", rep(reds[[5]], 100)))+
  ggtitle("S217A 0mM-P<sub>i</sub> (0.04 pN/nm)")+
    
    # annotate(x = -0.1, y= -100, xend = 0.9, yend=-100, geom="segment")+
    # annotate(x = -0.1, y= -100, xend = -0.1, yend=-60, geom="segment")+
    
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
            y = -100)+
  draw_line(x = -0.1,
            y = c(-100, -60))
)

s217a_pi <- readRDS("traces/s217a 30mM Pi_2020-06-24_obs-02_3.2593-9.7281.rds")
s217a_pi$layers[[1]] <- NULL

(s217a_pi <- 
  s217a_pi+
  geom_line(size = 0.3)+
  #coord_cartesian(c(0, 4))+
  scale_x_continuous(breaks = 0:10, expand = expansion(0, 0.1))+
  scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0, 0))+
  scale_color_manual(values = c("black", rep(reds[[5]], 100)))+
    ggtitle("S217A 30mM-P<sub>i</sub> (0.04 pN/nm)")+
    
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
            y = -75)+
  draw_line(x = -0.1,
            y = c(-75, -35))
)

s217a_pi_3w <- readRDS("traces/S217A 30mM Pi_3W_2022-01-08_obs-17_5.7081-11.0694.rds")
s217a_pi_3w$layers[[1]] <- NULL

(s217a_pi_3w <- 
s217a_pi_3w+
  geom_line(size = 0.3)+
  coord_cartesian(c(0, 5))+
  #  geom_segment(data = NULL, aes(x = 0, y = -60, xend = 1, yend = -60), color = "black")
  scale_x_continuous(breaks = 0:10, expand = expansion(0, 0.1))+
  scale_y_continuous(breaks = seq(-120, 120, by = 40), expand = expansion(0, 0))+
  scale_color_manual(values = c("black", rep(reds[[9]], 100)))+
  ggtitle("S217A 30mM-P<sub>i</sub> (0.1 pN/nm)")+
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
  draw_line(x = c(-0.1, 0.9),
            y = -65)+
  draw_line(x = -0.1,
            y = c(-65, -25))
  
)
  
  
  






s217a_trace <- plot_grid(s217a_con, s217a_pi, s217a_pi_3w, ncol = 1, labels = c("C", "", ""))


raw <- plot_grid(wt_trace, s217a_trace, ncol =  2)



# 
# 
# 
# 
# 
# colorz <- RColorBrewer::brewer.pal(8, 'Dark2')
# wtSig <- data.frame(myo = 'WT', pi = '0mM-Pi', power = '1.5-W', y = 230, label = '#') 
# ggplot()+
#         geom_errorbar(data = dataSummary,
#                       aes(x = myo,
#                           y = timeOnAvg,
#                           ymax = timeOnAvg + timeOnSE,
#                           ymin = timeOnAvg - timeOnSE,
#                           color = power),
#                       width = 0.25,
#                       position = position_dodge(width = 0.75))+
#         geom_col(data = dataSummary,
#                  aes(x = myo, 
#                      y = timeOnAvg,
#                   #   alpha = power,
#                      fill = power),
#                  width = 0.73,
#                  color = 'black',
#                  #                     size = 1,
#                  position = position_dodge())+
#         geom_text(data = wtSig, aes(x = myo, y = y, label = label), color = colorz[[1]])+
#         xlab("")+
#         ylab("Time (ms)")+
#         scale_y_continuous(breaks = seq(0, 300, by = 50), expand = expansion(0, c(0, 10)))+
#         scale_fill_manual(values = colorz)+
#         scale_color_manual(values = c(rep("black", 3)))+
#         facet_wrap(~pi)+
#         theme_cowplot(10)+
#         theme(
#                 strip.background = element_rect(fill = "transparent"),
#                 strip.text = element_text(face = "bold", size = 10)
#         )
# 
# 
# 



########################################################################

# fig1Left  <- 
#         (ggdraw(ggTimeOnECDF)+
#          draw_plot(ggTimeOn, x = 0.25, y = 0.075, width = 0.75, height = 0.65)+
#          draw_plot_label(
#          c("C", "D"),
#          c(0, 0.26),
#          c(1, 0.7)
#          )
#         )


trapSchematic  <- image_read("traps2.png")

ggtrap  <- ggdraw()+ draw_image(trapSchematic)


# fig1traces <- plot_grid(trace1, trace2, trace3, trace3_zoom,  ncol = 1, rel_heights = c(1, 1, 1, 2), labels = c("B", "", "", "D"))
# 
# ( fig1left <- plot_grid(ggtrap, ggTimeOn,  ncol = 1, labels = c("A", "C"), rel_heights = c(0.5, 0.5)) )
# 
# 
# ( fig1  <- plot_grid(fig1left, fig1traces, ncol = 2, rel_widths  = c(0.4, 0.6)) )
# 
# fig1middle <- plot_grid(ggTimeOnECDF, ggTimeOn, nrow = 1, labels = c("B", "C"))

plot_grid(ggtrap, raw, nrow = 1, rel_widths = c(4, 6), labels = c("A", ""))

ggsave("figs/fig1-traces.jpg", dpi = 500, bg = "white", height = 4, width = 12)

storm <- read_excel("katie-storm/myosin5-real10ug-FOV1_subFOV1.xlsx")

storm <- 
  storm %>% 
  dplyr::rename(x=`CenterX[µm]`, y=`CenterY[µm]`, point=ObjectId, neighbor=`NearestObjDist[µm]`) %>% 
  dplyr::mutate(x=1000*x, y=1000*y)

colorz <- randomColor(nrow(storm), luminosity = "bright")
#storm$point <- as.factor(storm$point)
(fig1a <- 
    ggplot(storm)+
    geom_point(aes(x, y, color = as.factor(point)),
               #color="palegreen2",
               alpha=0.5,
               #shape=16,
               size=0.2)+
    annotate("rect", xmin = 0, xmax = 1000, ymin = 0, ymax = 1000, color = "white", fill = "transparent")+
    scale_color_manual(values = colorz)+
    xlab("Position X (nm)")+
    ylab("Position Y (nm)")+
    theme_dark()+
    theme(panel.background = element_rect(fill = 'black', color = 'black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          
          plot.background = element_rect(fill='black', color = "black"),
          axis.text = element_text(color='white'),
          axis.ticks=element_line(color='white'),
          axis.title=element_text(color='white'),
          legend.position="none"
          
    )
)

#n145 <- filter(storm, point==237)
roi <- dplyr::filter(storm, x<1 & y<1)
n46 <- dplyr::filter(storm, point==46)
n77 <- dplyr::filter(storm, point==77)

(fig1b<-
    fig1a+
    geom_point(aes(x, y, color = as.factor(point)), 
               show.legend = FALSE,
               #color="palegreen2",
               size=1.5)+
    coord_cartesian(xlim=c(0,1000), ylim=c(0,1000))+
    #geom_text(aes(x, y, label = point), color = "white")+
    geom_point(data = n46,
               aes(x, y), 
               shape = 13,
               size = 3,
               color = "white")+
    geom_segment(aes(x = n46$x,
                     y = n46$y,
                     xend = n77$x,
                     yend = n77$y-8),
                 color = "white",
                 arrow = arrow(length = unit(0.02, "npc")))+
    annotate("text", x=450, y=150, label= paste0("d = ", n46$neighbor*1000), color="white")+
    scale_y_continuous(expand = expansion(c(0.01, 0.01)))
)


(fig1c<-
    ggplot(storm)+
    geom_histogram(aes(x=neighbor*1000, 
                       y = stat(density)), 
                   color='black', 
                   bins=25, 
                   size = 1,
                   fill = "#cf9814")+
    stat_function(fun = dnorm,
                  args = list(mean = mean(storm$neighbor*1000),
                              sd = sd(storm$neighbor*1000)))+
    annotate("text", x=350, y=0.004,
             label=paste0("Average Spacing = ",
                          round(mean(storm$neighbor*1000), 0),
                          " nm"),
             color='black')+
    coord_cartesian(xlim=c(0,500))+
    
    xlab("Nearest Neighbor (nm)")+
    ylab("Density")+
    ggtitle("Distance Between Molecules")+
    scale_y_continuous(expand = expansion(c(0, 0.01)))+
    theme_cowplot(10)
)


fig1row2 <- 
  plot_grid(fig1a,fig1b,
            labels=c("C", "D"), 
            nrow=1, 
            rel_widths = c(1,1),
            label_colour = c("white"))

storm_plot <- 
  plot_grid(figleft, fig1c,
          labels=c("", "C"),
          nrow=1,
          rel_widths=c(1,0.4)
)


fig1top <- plot_grid(ggtrap, fig1c, nrow = 1, labels = c("A", "B"))

fig1top2 <- plot_grid(fig1top, fig1row2, nrow = 2)

plot_grid(fig1top2, raw, nrow = 2, rel_heights = c(0.6, 0.4))

ggsave("figs/fig1.png", bg = "white", height = 10, width = 8.5, units = "in", dpi = 500)
#################################################################################
#################################################################################

#### FIG 2 ####

#######################################################################
# Cumulative Distributions
#######################################################################

timeOnECDF  <- myoV[, .(conditions = conditions2,
                        timeOn = unique(time_on_ms),
                        ecdf = ecdf(time_on_ms)(unique(time_on_ms))),
                    by = list(conditions2, myo, pi, power)]

(ggTimeOnECDF  <- 
    ggplot(timeOnECDF, aes(timeOn, ecdf)) + 
    geom_step(aes(color = conditions, linetype = pi), size = 0.75)+
    coord_cartesian(xlim = c(0, 2000), ylim = c(0, 1))+
    facet_grid(~myo)+
    ggtitle("Attachment Times (distribution)")+
    ylab("Cumulative Distribution")+ 
    xlab("Time (ms)")+
    scale_color_manual(values = plot_colors)+
    scale_linetype_manual(values = c("dashed", "solid"))+
    scale_y_continuous(expand = expansion(c(0, 0.1)))+
    theme_cowplot(12)+ 
    theme(
      strip.background = element_rect(fill="transparent"),
      strip.text = element_text(face = "bold"),
      legend.position = "none"
    )
)


(ggTimeHistoWT  <- 
    
    ggplot(myoV[myo=="WT"], aes(time_on_ms)) + 
  
    geom_histogram(aes(y = stat(density), fill = conditions2), binwidth = 20, color = "black")+
   
    # geom_boxplot(aes(x = time_on_ms, 
    #                  y = -0.25, 
    #                 # fill = conditions2,
    #                  color = prettyConditions
    #                  #color = conditions2
    #                  ), 
    #            #  color = "black",
    #              width = 0.2,
    #              outlier.size = 1.5,
    #              outlier.alpha = 0.3,
    #            outlier.shape = 16)+
    #scale_x_log10()+
    # geom_vline(data = dataSummary[myo=="WT"], 
    #            aes(xintercept = timeOnAvg, 
    #                color = prettyConditions),
    #            color = "black", 
    #            linetype = "dashed",
    #            size = 0.5)+
    
    xlab("Time (ms)")+
   coord_cartesian(xlim = c(0, 750))+
   scale_y_continuous(expand = expansion(c(0, 0)))+
    scale_fill_manual(values = plot_colors2)+
    scale_color_manual(values = plot_colors2)+
    facet_wrap(pi~pn_nm3, ncol=1, strip.position = "right")+
    theme_cowplot(9)+
    theme(
      strip.background = element_rect(fill="transparent"),
      strip.text = element_text(size = 7, face = "bold"),
      legend.position = "none",
      axis.text.y = element_blank()
    )
)


(ggTimeHistoS217A  <- 
    
    ggplot(myoV[myo=="S217A"], aes(time_on_ms)) + 
    
    geom_histogram(aes(y = stat(density), fill = conditions2), binwidth=20, color = "black")+
    
    # geom_boxplot(aes(x = time_on_ms, 
    #                  y = -0.25, 
    #                  # fill = conditions2,
    #                  color = prettyConditions
    #                  #color = conditions2
    # ), 
    # #  color = "black",
    # width = 0.2,
    # outlier.size = 1.5,
    # outlier.alpha = 0.3,
    # outlier.shape = 16)+
    # #scale_x_log10()+
    # geom_vline(data = dataSummary[myo=="S217A"], 
    #            aes(xintercept = timeOnAvg, 
    #                color = prettyConditions),
    #            color = "black", 
    #            linetype = "dashed",
    #            size = 0.5)+
    
    xlab("Time (ms)")+
    coord_cartesian(xlim = c(0, 750))+
    scale_y_continuous(expand = expansion(c(0, 0)))+
    scale_fill_manual(values = plot_colors2[5:8])+
    scale_color_manual(values = plot_colors2[5:8])+
    facet_wrap(pi~pn_nm3, ncol=1, strip.position = "right")+
    theme_cowplot(9)+
    theme(
      strip.background = element_rect(fill="transparent"),
      strip.text = element_text(size = 7, face = "bold"),
      legend.position = "none",
      axis.text.y = element_blank()
    )
)


(fig2Right <- 
  ggdraw(ggTimeOnECDF)+
    draw_plot(ggTimeHistoWT, 0.2, 0.1, 0.3, 0.65)+
    draw_plot(ggTimeHistoS217A, 0.7, 0.1, 0.3, 0.65)
)

(ggTimeOn  <- 
    ggplot()+
    
    
    
    geom_errorbar(data = dataSummary,
                  aes(x = pi,
                      y = timeOnAvg,
                      ymax = timeOnAvg + timeOnSE,
                      ymin = timeOnAvg - timeOnSE,
                      color = conditions2),
                  width = 1,
                  position = position_dodge2(width = NULL,
                                             preserve = "single",
                                             padding = 0.5))+
    
    # 
    # 
    # 
    geom_col(data = dataSummary,
             aes(x = pi,
                 y = timeOnAvg,
                 fill = conditions2),
             width = 1,
             position = position_dodge2(width = NULL,
                                        preserve = "single",
                                        padding = 0),
             show.legend = FALSE)+
    geom_text(data = dataSummary,
              aes(x = pi,
                  y = 10,
                  label = pn_nm,
                  group = conditions2),
              vjust = -1,
              position = position_dodge2(width = 1, preserve = "single"),
              size = 2.75)+
    facet_wrap(~myo, scales = "free_x")+
    xlab("")+
    ylab("Time (ms)")+
    ggtitle("Attachment Times (average)")+
    scale_fill_manual(values = plot_colors2, name = "Conditions")+
    scale_color_manual(values = plot_colors2, name = "Conditions")+
    scale_y_continuous(expand = expansion(0, c(0, 10)))+
    theme_cowplot(12)+
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "transparent"),
      strip.text = element_text(face = "bold")
    )
)


( fig2 <-plot_grid(ggTimeOn, fig2Right, nrow = 1, rel_widths = c(0.45, 0.55), labels = "AUTO", label_size = 16) )

ggsave("figs/fig2-attachment.jpg", height = 5, width = 12, units = "in", bg = "white")
# fig2Title <- ggdraw() + 
#   draw_label(
#     "Attachment Times",
#     fontface = 'bold',
#     x = 0,
#     hjust = 0,
#     size = 16
#   ) +
#   theme(
#     # add margin on the left of the drawing canvas,
#     # so title is aligned with left edge of first plot
#    # plot.margin = margin(0, 0, 0, 7)
#   )
# 
# plot_grid(fig2Title, 
#           fig2, 
#           ncol = 1,
#           # rel_heights values control vertical title margins
#           rel_heights = c(0.1, 1)
# ) 
# (ggfreqWT  <- 
#     ggplot(myoV[myo=="WT"], aes(time_on_ms)) + 
#     geom_freqpoly(aes(y = stat(density), color = conditions2, linetype = pi), binwidth=20)+
#     #scale_x_log10()+
#     coord_cartesian(xlim = c(0, 1000))+
#     scale_y_continuous(expand = expansion(c(0, 0.1)))+
#     scale_color_manual(values = plot_colors2)+
#     scale_linetype_manual(values = c("dashed", "solid"))+
#     #facet_wrap(pi~pn_nm, ncol=1)+
#     theme_cowplot(10)+
#     theme(
#       strip.background = element_rect(fill="transparent"),
#       strip.text = element_text(face = "bold"),
#       legend.position = "none"
#     )
# )
  

















################################################################################
myoVLinear  <- myoV[, 
                    .(displVsTon = list(.SD)), 
                    by = list(conditions2, myo, pi, power), 
                    .SDcols = c("displacement_nm", "time_on_ms")]

myoVLinear$mod <- lapply(myoVLinear$displVsTon, function(x) lm(displacement_nm ~ time_on_ms, data =  x))

myoVLinear$line <-  lapply(myoVLinear$mod, function(x) as.data.frame(predict(x, 
                                                                     newdata = data.frame(time_on_ms = 1:3000),
                                                                     interval = "confidence")) |>
                                                         transform(x = 1:3000))
toDegrees  <- 180/pi

myoVLinear$slope  <- sapply(myoVLinear$mod, function(x) coef(x)[[2]])

myoVLines <- myoVLinear[, 
                        line[[1]], 
                        by = list(conditions2, myo, pi, power)
                       ] 


(ggTimeDispl <- ggplot()+
                 geom_point(data = myoV,
                            aes(time_on_ms,
                                displacement_nm, 
                                color = conditions2),
                           alpha = 0.4, 
                           shape = 16)+
                geom_line(data = myoVLines, 
                          aes(x = x, 
                              y = fit),
                          color = "black")+
                geom_text(data = myoVLinear,
                          aes(x = Inf,
                              y = 0,
                              label = paste0("slope = ", round(slope*1000, 0), " nm/s"),
                              color = conditions2),
                              hjust = 1,
                              vjust = 0,
                              size = 3.5
                              )+
                 facet_grid(myo~power+pi, scales = "free")+
                 coord_cartesian(xlim = c(0, 2500), ylim = c(0, 150))+
                 scale_color_manual(values = plot_colors)+
                 xlab("Time (ms)")+
                 ylab("Displacement (nm)")+
                 ggtitle("Displacement Vs. Duration")+
                 theme_cowplot(12)+
                 theme(
                  legend.position = "none", 
                  strip.background = element_rect(fill = "transparent"),
                  strip.text = element_text(face = "bold"),
                  axis.text.x = element_text(size = 8)
                )
        )

#ggsave("figs/time-vs-displ.png", bg = "white")
#################axis.text.y = element_text(vjust = 0)####################################################
# FORCES
#####################################################################
##### FIG 3 #####
(ggForce  <- 
        ggplot()+
        geom_errorbar(data = dataSummary,
                      aes(x = pi,
                          y = forceAvg,
                          ymax =  forceAvg + forceSE,
                          ymin = forceAvg - forceSE,
                          color = conditions2),
                      width = 1,
                      position = position_dodge2(width = NULL,
                                                 preserve = "single",
                                                 padding = 0.5))+
   
        geom_col(data = dataSummary,
                 aes(x = pi, 
                     y = forceAvg,
                    fill = conditions2,
                    color = conditions2),
#                   color = "black",
                    width = 1,
                    position = position_dodge2(width = NULL,
                                                preserve = "single",
                                                padding = 0),
                                            show.legend = FALSE)+
  geom_text(data = dataSummary,
            aes(x = pi,
                y = 0,
                label = pn_nm,
                group = conditions2),
            vjust = -1,
            position = position_dodge2(width = 1, preserve = "single"),
            size = 3)+
facet_wrap(~myo, scales = "free_x", nrow = 1)+
        xlab("")+
        ylab("piconewtons")+
        ggtitle("Forces")+
        scale_fill_manual(values = plot_colors2, name = "Conditions")+
        scale_color_manual(values = plot_colors2, name = "Conditions")+
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
        theme_cowplot(12)+
        theme(
         legend.position = "none",
         strip.background = element_rect(fill = "transparent"),
         strip.text = element_text(face = "bold")
         #plot.title = element_text(size = 12)
       )
)




(ggDispl  <- 
        ggplot()+
        geom_errorbar(data = dataSummary,
                      aes(x = pi,
                          y = displacementAvg,
                          ymax =  displacementAvg + displacementSE,
                          ymin = displacementAvg - displacementSE,
                          group = power,
                          color = conditions2),
                      width = 1,
                      position = position_dodge2(width = NULL,
                                                 preserve = "single",
                                                 padding = 0.5))+
        geom_col(data = dataSummary,
                 aes(x = pi, 
                     y = displacementAvg,
                    fill = conditions2,
                    color = conditions2),
#                   color = "black",
                    width = 1,
                    position = position_dodge2(width = NULL,
                                               preserve = "single",
                                               padding = 0),
                    show.legend = FALSE)+
    geom_text(data = dataSummary,
              aes(x = pi,
                  y = 0,
                  label = pn_nm,
                  group = conditions2),
              vjust = -1,
              position = position_dodge2(width = 1, preserve = "single"),
              size = 3)+
        facet_wrap(~myo, scales = "free_x")+
        xlab("")+
        ylab("nanometers")+
        ggtitle("Displacements")+
        scale_fill_manual(values = plot_colors2, name = "Conditions")+
        scale_color_manual(values = plot_colors2, name = "Conditions")+
        scale_y_continuous(expand = expansion(0, c(0, 5)))+
        theme_cowplot(12)+
        theme(
         legend.position = "none",
         strip.background = element_rect(fill = "transparent"),
         strip.text = element_text(face = "bold")
        # plot.title = element_text(size = 12)
       )
)

trapForces  <- image_read("trap-force-500dpi.png")

ggtrapForce  <- ggdraw()+ draw_image(trapForces)


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
       annotate("text", x = 60, y = Inf, label = "0.04 pN/nm", color = "black", hjust = 1)+
       annotate("text", x = 50, y = Inf, label = "0.06 pN/nm", color = "hotpink3", hjust = 1)+
    annotate("text", x = 40, y = Inf, label = "0.1 pN/nm", color = "grey50", hjust = 1)+
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

(fig3Top <- plot_grid(ggtrapForce, ggEqui, ggEquiCurve, labels = "AUTO", rel_widths = c(0.3, 0.7, 0.3), nrow = 1))

( fig3Bottom <- plot_grid(ggDispl, ggForce, labels = c("D", "E")))
( fig3 <- plot_grid(fig3Top, fig3Bottom, nrow = 2, rel_heights = c(0.4, 0.6))) 

#ggsave("figs/fig3-displacement-forces.png", bg = "white", height = 8, width = 12, units = "in")




fig3_table <- dataSummary
fig3_table$pi <- as.numeric(gsub("[^0-9]","", fig3_table$pi))
fig3_table[, heads := round(displacementAvg/7, 1)]
fig3_table[, forceHead := round(forceAvg/heads, 2)]
fig3_table[, `:=`(Myosin = myo,
                  "Pi (mM)" = pi,
                  "Stiffness (pN/nm)" = pn_nm2, 
                  "Displacement (nm)" = paste0(round(displacementAvg, 1), " ± ", round(displacementSE, 1)),
                  "Force (pN)" = paste0(round(forceAvg, 2), " ± ", round(forceSE, 2)),
                  "Heads" = heads,
                  "Force/Head (pN)" = forceHead)]

fig3_table$Myosin <- factor(fig3_table$Myosin, levels = c("WT", "S217A"))

ggfig3table <- 
  fig3_table[, -c(1:28)] %>% 
    dplyr::arrange(Myosin) %>% 
    ggpubr::ggtexttable(rows = NULL,
                        theme = ttheme(
                          colnames.style = colnames_style(fill = "white"),
                          tbody.style = tbody_style(fill = alpha(plot_colors2, 0.6))))

 
( fig3plus <- plot_grid(fig3, ggfig3table, nrow = 2, rel_heights = c(0.7, 0.3), labels = c("", "F")) )

ggsave("figs/fig3-displacement-forces.png", bg = "white", height = 10, width = 11, units = "in")

 (forceRidges <-  ggplot(data = myoV)+
                   geom_density_ridges(aes(x = force, 
                                           y = prettyConditions, 
                                           fill = prettyConditions, 
                                           color = prettyConditions),
                                       quantile_lines = TRUE, 
                                       quantiles = 2,
                                       jittered_points = TRUE,
                                       position = position_points_jitter(width = 0.05, height = 0),
                                       point_shape = '|', 
                                       point_size = 2, 
                                       point_alpha = 0.8, 
                                       alpha = 0.7)+
                   scale_fill_manual(values = plot_colors)+ 
                   scale_color_manual(values = plot_colors)+ 
                   scale_y_discrete(limits = rev)+
                   xlab("piconewtons")+
                   ylab("")+
                   ggtitle("Forces (density)")+
                   theme_ridges(12, center_axis_labels = TRUE)+
                   theme(
                    legend.position = "none",
                    plot.title = element_text(hjust = 0, size = 12),
                    axis.text.y = element_text(size = 8))
)

( fig2 <- plot_grid(forceRidges, ggForce) )

ggsave("figs/forces.png", bg = "white")


(displRidges <-  ggplot(data = myoV)+
                   geom_density_ridges(aes(x = displacement_nm, 
                                           y = prettyConditions, 
                                           fill = prettyConditions, 
                                           color = prettyConditions),
                                       quantile_lines = TRUE, 
                                       quantiles = 2,
                                       jittered_points = TRUE,
                                       position = position_points_jitter(width = 0.05, height = 0),
                                       point_shape = '|', 
                                       point_size = 2, 
                                       point_alpha = 0.8, 
                                       alpha = 0.7)+
                   scale_fill_manual(values = plot_colors)+ 
                   scale_color_manual(values = plot_colors)+ 
                   scale_y_discrete(limits = rev)+
                   xlab("nanometers")+
                   ylab("")+
                   ggtitle("Displacement Distributions (density)")+
                   theme_ridges(12, center_axis_labels = TRUE)+
                   theme(
                    legend.position = "none",
                    axis.text.y = element_text(size = 8),
                    plot.title = element_text(size = 12)
                    )
 )


#  ggsave("figs/displacement-ridges.png", bg = "white")


#################################################################################
# VELOCITY
#################################################################################

(ggVelocity <- ggplot()+
 geom_col(data = dataSummary, 
          aes(x = pi, 
              y = velocityAvg*1000, 
              fill = conditions2,
              color = conditions2),
          width = 0.75,
          position = position_dodge(width = 0.75))+
geom_errorbar(data = dataSummary, 
              aes(pi, 
                  velocityAvg*1000,
                  ymin = (velocityAvg - velocitySE) * 1000,
                  ymax = (velocityAvg + velocitySE) * 1000,
                  color = conditions2),
              width = 0.25,
              position = position_dodge(width = 0.75))+
        geom_text(data = dataSummary,
                  aes(x = pi, 
                      y = 0, 
                      label = power2,
                      group = power),
                  vjust = -1, 
                 position = position_dodge(width = 0.75),
                 size = 4)+
facet_wrap(~myo)+
xlab("")+
ylab("nm/s")+
ggtitle("Velocity (average)")+
scale_fill_manual(values = plot_colors)+ 
scale_color_manual(values = plot_colors)+
scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
theme_cowplot(12)+
theme(
 legend.position = "none",
 strip.background = element_rect(fill = "transparent"),
 strip.text = element_text(face = "bold"),
 plot.title = element_text(size = 12)
 )
)


(velocityRidges <-  ggplot(data = myoV)+
                   geom_density_ridges(aes(x = velocity*1000, 
                                           y = prettyConditions, 
                                           fill = prettyConditions, 
                                           color = prettyConditions),
                                       quantile_lines = TRUE, 
                                       quantiles = 2,
                                       jittered_points = TRUE,
                                       position = position_points_jitter(width = 0.05, height = 0),
                                       point_shape = '|', 
                                       point_size = 2, 
                                       point_alpha = 0.8, 
                                       alpha = 0.7)+
                   scale_fill_manual(values = plot_colors)+ 
                   scale_color_manual(values = plot_colors)+ 
                   scale_y_discrete(limits = rev)+
                   scale_x_log10()+
                   coord_cartesian(xlim = c(1, 10000))+
                   xlab("nm/s")+
                   ylab("")+
                   ggtitle("Velocity Distributions (density)")+
                   theme_ridges(12, center_axis_labels = TRUE)+
                   theme(
                    legend.position = "none",
                    axis.text.y = element_text(size = 8),
                    plot.title = element_text(size = 12)
                    )
 )

( figVelocity  <- plot_grid(velocityRidges, ggVelocity) )

ggsave("figs/velocity.png", bg = "white")



ggplot()+
        geom_point(data = myoV,
        aes(x = force, 
            y = velocity*1000, 
            color = conditions2),
                   alpha = 0.4,
                   shape = 16)+
        facet_grid(myo~pi+power)+
                   scale_color_manual(values = plot_colors)+
xlab("Force (pN)")+
ylab("Velocity (nm/s)")+
ggtitle("Force Vs. Velocity")+
theme_cowplot(12)+
theme(
 legend.position = "none",
 strip.background = element_rect(fill = "transparent"),
 strip.text = element_text(face = "bold"),
 plot.title = element_text(size = 12)
 )
ggsave("figs/force-velocity.png", bg = "white")


###############################################################################################
# FORCE vs 1/Time-ON
################################################################################################
# myoVForces <- myoV[, .(conditions2, myo, pi, power, force)]

# boxData  <- merge(myoVForces, dataSummary, by = c("conditions2", "myo", "pi", "power"), all.x = TRUE)

# boxData[, rate := 1000*(1/timeOnAvg)]
# veigel equation: k1 = k0 exp(−W/kT) ; work is Force * d. d is a float

load_dep_data <- dataSummary[pi == "30mM-Pi" & myo == "WT", .(myo, pi, power, forceAvg, rateAvg)]

wt0 <- data.frame(myo="WT",
                  pi="0mM-Pi",
                  power=1.5,
                  forceAvg=0)
kT <-  4.10
fit <- nls(rateAvg ~ k0*exp((forceAvg*d) / 4.10 ), data = load_dep_data, start = list(k0=1, d=4))

predict_fit <- data.frame(y = predict(fit, newdata=data.frame(forceAvg=seq(0, 4.1, by = 0.01))),
                          x = seq(0, 4.1, by=0.01))

load_dep_data2 <- dataSummary[pi == "30mM-Pi" & myo == "S217A"]
fit2 <- nls(rateAvg ~ k0*exp((forceAvg*d) / 4.10 ), data = load_dep_data2, start = list(k0=1, d=4))

predict_fit2 <- data.frame(y = predict(fit2, newdata=data.frame(forceAvg=seq(0, 2.5, by = 0.01))),
                          x = seq(0, 2.5, by=0.01))

(ggLoad <- 
ggplot(data = dataSummary[pi == "30mM-Pi"], aes(x = forceAvg)) +
 geom_line(data=predict_fit, aes(x = x, y = y), color = blues[[9]], size = 0.7)+
 geom_line(data=predict_fit2, aes(x = x, y = y), color = reds[[9]],  size = 0.7)+
 geom_point(aes(y = rateAvg,
                       color = conditions2),
                   size = 1.5)+
 geom_errorbar(aes(ymin = rateAvg - rateSE,
                  ymax = rateAvg + rateSE,
                  color = conditions2))+
 geom_errorbarh(aes(y = rateAvg, 
                   xmin = forceAvg - forceSE,
                   xmax= forceAvg + forceSE,
                   color = conditions2))+
 xlab("Force (pN)")+
 ylab("*k<sub>pi+</sub>* (s<sup>-1</sup>)")+
 ggtitle("Load Dependence of *k<sub>pi+</sub>*")+
 coord_cartesian(xlim = c(0, NA), ylim = c(0, NA))+
 scale_color_manual(values = c(plot_colors[2:4], plot_colors[6:8]), guide = "none")+
 scale_fill_manual(values = plot_colors, guide = "none")+
 scale_shape_manual(values = c(15, 16), name = "")+
# facet_grid(~myo)+
 theme_cowplot(12)+
 theme(
  strip.background = element_rect(fill = "transparent"),
  strip.text = element_text(face = "bold"),
  plot.title = element_markdown(size = 10, hjust = 0.5),
  axis.title.y = element_markdown()
 )
 )

par1 <- broom::tidy(fit)
par1$myo <- "WT"


par2 <- broom::tidy(fit2)
par2$myo <- "S217A"

parAll <- rbind(par1, par2)
parAll$myo <- factor(parAll$myo, levels = c("WT", "S217A"))
parAll$term2 <- ifelse(parAll$term == "k0", "*k<sub>0</sub>* (s<sup>-1</sup>)", "*d* (nm)")
parAll$term2 <- factor(parAll$term2, levels = c("*k<sub>0</sub>* (s<sup>-1</sup>)", "*d* (nm)"))



(ggParAll <- 
        ggplot(data=parAll)+
        geom_col(aes(x = myo, y = estimate, fill = myo), position = "dodge")+
        geom_errorbar(aes(x = myo, 
                          ymax=estimate+std.error, 
                          ymin=estimate-std.error, 
                          color = myo), 
                      position="dodge",
                      width = 0.25)+
        facet_wrap(~term2,
                   strip.position = "left",
                   scales = "free")+
        ggtitle("Fit Parameters")+
        xlab("")+
        ylab("")+
        scale_fill_manual(values = c(blues[[7]], reds[[7]]))+
        scale_color_manual(values = c(blues[[7]], reds[[7]]))+
        scale_y_continuous(expand = expansion(c(0, 0.1)))+
        theme_cowplot(12)+
        theme(
         plot.title = element_text(hjust = 0.5, size = 10),
         axis.text.x = element_markdown(size = 6),
         axis.text.y = element_markdown(size = 10),
         axis.title.y = element_markdown(),
         legend.position = "none",
         strip.text = element_markdown(),
         strip.text.y.left = element_markdown(),
         strip.background = element_blank(),
         strip.placement = "outside"
        )
)

fig4top <- plot_grid(ggLoad, ggParAll, nrow = 1, rel_widths = c(0.65, 0.35), labels = c("a", "b"))

delta_g  <- image_read("traces/Figure 4C.pptx.png")

ggdelta_g  <- ggdraw()+draw_image(delta_g)


# fig4_inlay <- 
#   ggdraw(ggLoad)+
#   draw_plot(ggParAll,
#             x = 0.45, 
#             y = 0.1, 
#             width = 0.5, 
#             height = 0.5)
#   
# 
# 
# 
# plot_grid(fig4_inlay, ggdelta_g, nrow = 2, rel_heights = c(0.55, 0.44))

#plot_grid(ggLoad, ggParAll, ggdelta_g, ncol = 1, rel_heights = c(0.5, 0.2, 0.4))
plot_grid(fig4top, ggdelta_g, nrow = 2, labels = c("", "c"), rel_heights = c(0.4, 0.6))
ggsave("figs/fig4-load-dependence-rates.jpg", bg="white")

sink("figs/load-dep-rates.txt")
summary(fit)
summary(fit2)
sink()

