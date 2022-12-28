library(readxl)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(randomcoloR)

storm <- read_excel("myosin5-real10ug-FOV1_subFOV1.xlsx")

storm <- 
  storm %>% 
  rename(x=`CenterX[µm]`, y=`CenterY[µm]`, point=ObjectId, neighbor=`NearestObjDist[µm]`) %>% 
  mutate(x=1000*x, y=1000*y)

colorz <- randomColor(nrow(storm))
#storm$point <- as.factor(storm$point)
(fig1a <- 
  ggplot(storm)+
    geom_point(aes(x, y, color = as.factor(point)),
               #color="palegreen2",
               alpha=0.5,
               #shape=16,
               size=0.4)+
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
roi <- filter(storm, x<1 & y<1)
n46 <- filter(storm, point==46)
n77 <- filter(storm, point==77)

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
                  yend = n77$y-0.01),
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
                   bins=25)+
    stat_function(fun = dnorm,
                  args = list(mean = mean(storm$neighbor*1000),
                              sd = sd(storm$neighbor*1000)))+
    annotate("text", x=350, y=0.004,
             label=paste0("Average distance = ",
                          round(mean(storm$neighbor*1000), 0),
                          " nm"),
                          color='black')+
    coord_cartesian(xlim=c(0,500))+

    xlab("Nearest Neighbor (nm)")+
    ylab("Density")+
    scale_y_continuous(expand = expansion(c(0, 0.01)))+
    theme_cowplot()
)


figleft <- 
  plot_grid(fig1a,fig1b,
          labels="AUTO", 
          nrow=1, 
          rel_widths = c(1,0.9),
          label_colour = c("white"))

plot_grid(figleft, fig1c,
          labels=c("", "C"),
          nrow=1,
          rel_widths=c(1,0.4)
)


ggsave("storm-fig2.jpg", dpi = 500, height = 5, width=15, bg = "white")
