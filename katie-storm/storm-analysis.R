library(readxl)
library(data.table)
library(ggplot2)
library(cowplot)

storm <- read_excel("/home/beta/umass/lab/myoV-storm/myosin5-real10ug-FOV1_subFOV2.xlsx")
setDT(storm)
old_names <- c("CenterX[µm]", "CenterY[µm]")
new_names <- c("x", "y")
setnames(storm, old_names, new_names)

# test <- storm[1,]
# test10 <- storm[10,]
(gg_all <- 
  ggplot()+
  geom_point(aes(storm$x, storm$y), alpha=0.5, shape = 16, size = 1)+
    # geom_point(aes(test$x, test$y),  size = 2, color = "red")+
    # geom_point(aes(test10$x, test10$y),  size = 2, color = "blue")+
    # coord_cartesian(c(4, 5), c(0, 0.05))+
  theme_classic()
)

x_width <- 0.5 #um
y_height <- 0.5 #um
 
x_interval <- seq(0, max(storm$x), by = x_width)
y_interval <- seq(0, max(storm$y), by = y_height)

x_interval_start <- head(x_interval, -1)
x_interval_stop <- tail(x_interval, -1)

y_interval_start <- head(y_interval, -1)
y_interval_stop <- tail(y_interval, -1)

x_coord <- data.frame(start = x_interval_start, 
                      stop = x_interval_stop)

y_coord <- data.frame(start = y_interval_start, 
                      stop = y_interval_stop)

(gg_all <- 
  gg_all+
  geom_vline(aes(xintercept=x_interval_stop))+
  geom_hline(aes(yintercept=y_interval_stop))
)

all_molecules <- vector("list")
gg <- vector("list")
molecules <- vector()
g <- 1
for(x in 1:nrow(x_coord)){
  x_1 <- x_coord$start[[x]]
  x_2 <- x_coord$stop[[x]]
  for(y in 1:nrow(y_coord)){
    y_1 <- y_coord$start[[y]]
    y_2 <- y_coord$stop[[y]]
    
    roi <- storm[x >= x_1 & x<=x_2 & y>=y_1 & y<=y_2]
   molecules[y] <- nrow(roi)
   gg[[g]] <- ggplot(roi)+
     geom_point(aes(x, y), alpha=0.5, shape = 16)+
     xlab("")+
     ylab("")+
     theme_bw()
   print(paste0("Analyzing x = ", x, "; y = ", y))
   g <- g+1
  }
  all_molecules[[x]] <- molecules
}

all_molecules <- unlist(all_molecules)

mean(all_molecules)

ggplot()+
  geom_histogram(aes(x = all_molecules), binwidth = 1, color = "black")

plot_grid(plotlist = gg[1:60])

ggplot()+geom_histogram(aes(x=storm$`NearestObjDist[µm]`*1000), color = "black", binwidth = 20)+
  scale_x_continuous(breaks = seq(0, 1500, 50))
