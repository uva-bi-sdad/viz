
#SE DATA
##https://ncsesdata.nsf.gov/doctoratework/2017/html/sdr2017_dst_4-3.html
##https://ncsesdata.nsf.gov/doctoratework/2013/html/SDR2013_DST4_3.html
FOS <- c("Aerospace, Aeronautical, & Astronautical", "Chemical", "Civil", "Electrical & Computer", "Materials & Metallurgical", "Mechanical", "Other")
YR2013 <- c(89.7, 87.6, 90.6, 92.0, 90.4, 92.9, 88.8)
YR2013SE <- c(2.2,1.25, 1.40, .7, 1.4, 1.1, .95)
YR2017 <- c(89.9, 88.3, 90.8, 91.8, 91.0, 88.7, 88.7)
YR2017SE <- c(1.5, .95, .9, .65, .9, 1.05, .65)
DIF<-ifelse((YR2017>YR2013),"UP","DOWN")
SED2<-data.frame(FOS,YR2013,YR2013SE, YR2017, YR2017SE, DIF)
library(ggplot2)

#Color Blind Palettes
#color blind palette with grey
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#color blind palette with black
cbPalette.blk<-c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#Display the colorblind palette
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...)
wheel(cbPalette)


# only plotting one slope tails

fun <- function(x){
  (.2*x)+89.7
}

ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun(0.1), yend = fun(0.9)), 
               size = 1.2) +
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(86, 94)) +
  geom_point(aes(x =0, y = 89.7+2.2))+
  geom_point(aes(x =0, y = 89.7-2.2)) +
  geom_point(aes(x =1, y = 89.9+1.5))+
  geom_point(aes(x =1, y = 89.9-1.5))+
  geom_curve(aes(x = 0, xend = .10, y = 89.7+2.2, yend = fun(.1)), curvature = .3, angle = 90)+
  geom_curve(aes(x = 0, xend = .10, y = 89.7-2.2, yend = fun(.1)), curvature = -.3, angle = 90) +
  geom_curve(aes(x = 1, xend = .90, y =  89.9+1.5, yend = fun(.9)), curvature = -.3, angle = 90)+
  geom_curve(aes(x = 1, xend = .90, y =  89.9-1.5, yend = fun(.9)), curvature = .3, angle = 90) 





fun2 <- function(x){
  ((YR2017-YR2013)*x) + YR2013 
}

# SLOPE TAILS, MULTICOLOR
ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun2(0.1), yend = fun2(0.9), color = FOS), size = 2) +
  scale_color_manual(values = c(cbPalette[2], cbPalette[3], cbPalette[4], cbPalette[5], cbPalette[6], cbPalette[7], cbPalette[8])) +
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(min(YR2013-YR2013SE -2), 94)) +
  geom_point(aes(x =0, y = YR2013-YR2013SE, color = FOS)) +
  geom_point(aes(x =0, y = YR2013+YR2013SE, color = FOS)) +
  geom_point(aes(x =1, y = YR2017+YR2017SE, color = FOS)) +
  geom_point(aes(x =1, y = YR2017-YR2017SE, color = FOS)) +
  geom_curve(aes(x = 0, xend = .10, y = YR2013+YR2013SE, yend = fun2(.1), color = FOS), curvature = .3, angle = 90) +
  geom_curve(aes(x = 0, xend = .10, y = YR2013-YR2013SE, yend = fun2(.1), color = FOS), curvature = -.3, angle = 90) +
  geom_curve(aes(x = 1, xend = .90, y = YR2017+YR2017SE, yend = fun2(.9), color = FOS), curvature = -.3, angle = 90) +
  geom_curve(aes(x = 1, xend = .90, y = YR2017-YR2017SE, yend = fun2(.9), color = FOS), curvature = .3, angle = 90) +
  geom_text(aes(x=c(0, 1),
                y = min(YR2013-YR2013SE -1),
                label=c("2013", "2017")), 
            col= "grey30",
            hjust="right", size = 6, 
            fontface = "bold") +
  geom_text(aes(x=0-0.15,
                y=YR2013,
                label=stringr::str_wrap(paste0(FOS), width = 25)),
            hjust="right") +
  geom_text(aes(x = 0-0.03, 
                y = YR2013, 
                label = paste0(round(YR2013, 1), "%")), 
            hjust="right", 
            col="grey30") +
  geom_text(aes(x=1+0.08,
                y=YR2017,
                label=paste0(round(YR2017, 1), "%")),
            col="grey30") +
  labs(title="Labor force participation rate among \nU.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017") +
  theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
  theme_classic()+
  theme_void() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), 
        legend.position = "none") 

# SLOPE TAILS, TWOCOLOR
ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun2(0.1), yend = fun2(0.9), color = DIF), size = 2) +
  scale_color_manual(values = c(cbPalette[2], cbPalette[3], cbPalette[4], cbPalette[5], cbPalette[6], cbPalette[7], cbPalette[8])) +
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(min(YR2013-YR2013SE -2), 94)) +
  geom_point(aes(x =0, y = YR2013-YR2013SE, color = FOS)) +
  geom_point(aes(x =0, y = YR2013+YR2013SE, color = DIF)) +
  geom_point(aes(x =1, y = YR2017+YR2017SE, color = DIF)) +
  geom_point(aes(x =1, y = YR2017-YR2017SE, color = DIF)) +
  geom_curve(aes(x = 0, xend = .10, y = YR2013+YR2013SE, yend = fun2(.1), color = DIF), curvature = .3, angle = 90) +
  geom_curve(aes(x = 0, xend = .10, y = YR2013-YR2013SE, yend = fun2(.1), color = DIF), curvature = -.3, angle = 90) +
  geom_curve(aes(x = 1, xend = .90, y = YR2017+YR2017SE, yend = fun2(.9), color = DIF), curvature = -.3, angle = 90) +
  geom_curve(aes(x = 1, xend = .90, y = YR2017-YR2017SE, yend = fun2(.9), color = DIF), curvature = .3, angle = 90) +
  geom_text(aes(x=c(0, 1),
                y = min(YR2013-YR2013SE -1),
                label=c("2013", "2017")), 
            col= "grey30",
            hjust="right", size = 6, 
            fontface = "bold") +
  geom_text(aes(x=0-0.15,
                y=YR2013,
                label=stringr::str_wrap(paste0(FOS), width = 25)),
            hjust="right") +
  geom_text(aes(x = 0-0.03, 
                y = YR2013, 
                label = paste0(round(YR2013, 1), "%")), 
            hjust="right", 
            col="grey30") +
  geom_text(aes(x=1+0.08,
                y=YR2017,
                label=paste0(round(YR2017, 1), "%")),
            col="grey30") +
  labs(title="Labor force participation rate among \nU.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017") +
  theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
  theme_classic()+
  theme_void() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), 
        legend.position = "none") 




#size of dot
# line size from [3, 7]
# x = [.65, 2.2]

fun3 <- function(x){
  (((7-3)/(max(YR2013SE, YR2017SE) - min(YR2013SE, YR2017SE)))* (x - min(YR2013SE, YR2017SE))) + 3
}



ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = YR2013, yend = YR2017, color = DIF), size = 2) +
  geom_point(aes(x = 0, y = YR2013, color = DIF), shape = 21,  size = fun3(YR2013SE)) +
  geom_point(aes(x = 1, y = YR2017, color = DIF), shape = 21, size = fun3(YR2017SE)) +
  scale_color_manual(values = c(cbPalette[6], cbPalette[2])) +
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(85, 94)) +
  geom_point(aes(x =0, y = YR2013, color = DIF)) +
  geom_point(aes(x =1, y = YR2017, color = DIF))+
  geom_text(aes(x=c(0, 1),
                y=min(YR2013 - 1),
                label=c("2013", "2017")), 
            col= "grey30",
            hjust="right", size = 6, 
            fontface = "bold") +
  geom_text(aes(x=0-0.1,
                y=YR2013,
                label=paste0(FOS)), 
            col= "grey30",
            hjust="right") +
  geom_text(aes(x = 0-0.03, 
                y = YR2013, 
                label = paste0(round(YR2013, 1), "%")), 
            hjust="right", 
            col="grey30") +
  geom_text(aes(x=1+0.08,
                y=YR2017,
                label=paste0(round(YR2017, 1), "%")),
            col="grey30") +
  labs(title="Labor force participation rate among U.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017", caption = "Circles are proportional to standard error. Larger circles indicate greater standard error. \nEmpty circles to show overlap (e.g. Mechanical, Other).") +
  theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
  theme_classic()+
  theme_void() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), 
        legend.position = "none") 

#shaded regions as seen in MU Collective
#https://mucollective.northwestern.edu/files/2019-BeliefDrivenVis-C+J.pdf

y <- c(89.7+2.2, 89.7-2.2, 89.9-1.5, 89.9+1.5, 87.6+1.25, 87.6-1.25, 88.3-.95, 88.3+.95)
x <- c(0, 0, 1, 1, 0,0,1,1)
id <-rep(c("a", "b"), each = 4)

frame <-data.frame(x, y, id)


y <- c(YR2013+YR2013SE, YR2013-YR2013SE, YR2017-YR2017SE, YR2017+YR2017SE)
x <- rep(c(0, 1), each = 14)
id <- rep(names, 4)
frame <-data.frame(x, y, id)




# don't like this
ggplot(data = frame, aes(x=x, y=y)) +
  geom_polygon(aes(group = id, fill = id), alpha = .2) +
  geom_segment(data = SED2, aes(x =0, xend = 1, y = YR2013, yend =YR2017))




#SLOPE AREA

# plot one
fun <- function(x){
  (.2*x)+89.7
}


y <- c(89.7+2.2, 89.7-2.2, fun(.1), 89.9+1.5, 89.9-1.5, fun(.9))
x <- c(0, 0, .1, 1, 1, .9)
id <- rep(c("a", "b"), each = 3)


ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun(0.1), yend = fun(0.9)), 
               size = 1.2) +
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(86, 94)) +
  geom_point(aes(x =0, y = 89.7+2.2))+
  geom_point(aes(x =0, y = 89.7-2.2)) +
  geom_point(aes(x =1, y = 89.9+1.5))+
  geom_point(aes(x =1, y = 89.9-1.5)) +
  geom_polygon(aes(x =x, y = y, group = id), alpha = 0.1)
 




# SLOPE AREA MULTI
FOS <- c("Aerospace, Aeronautical, & Astronautical", "Chemical", "Civil", "Electrical & Computer", "Materials & Metallurgical", "Mechanical", "Other")
YR2013 <- c(89.7, 87.6, 90.6, 92.0, 90.4, 92.9, 88.8)
YR2013SE <- c(2.2,1.25, 1.40, .7, 1.4, 1.1, .95)
YR2017 <- c(89.9, 88.3, 90.8, 91.8, 91.0, 88.7, 88.7)
YR2017SE <- c(1.5, .95, .9, .65, .9, 1.05, .65)
DIF<-ifelse((YR2017>YR2013),"UP","DOWN")


SED2<-data.frame(FOS,YR2013,YR2013SE, YR2017, YR2017SE, DIF)
library(ggplot2)

# plot multiple slope tails

fun2 <- function(x){
  ((YR2017-YR2013)*x) + YR2013 
}

y <- c(YR2013+YR2013SE, YR2013-YR2013SE, fun2(.1), YR2017+YR2017SE, YR2017-YR2017SE, fun2(.9))
x <- rep(c(0, 0, .1, 1, 1, .9), each = 7)
id <- rep(c("a", "b", "c", "d", "e", "f", "g"), 3)
id2 <- rep(c("h", "i", "j", "k", "l", "m", "n"), 3)
id <- data.frame(id)
id2 <- data.frame(id2)
colnames(id2)[colnames(id2)=="id2"] <- "id"
id <- rbind(id, id2)
id <- id$id



# slope area multi



ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun2(0.1), yend = fun2(0.9), color = FOS),
               size = 1.2) +
  scale_color_manual(values = c(cbPalette[2], cbPalette[3], cbPalette[4], cbPalette[5], cbPalette[6], cbPalette[7], cbPalette[8]))+
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(min(YR2013 - YR2013SE - 2), 94)) +
  geom_polygon(aes(x =x, y = y, group = id, fill = id), alpha = 0.2) +
  scale_fill_manual(values = c( rep(c(cbPalette[2], cbPalette[3], cbPalette[4], cbPalette[5], cbPalette[6], cbPalette[7], cbPalette[8]), 2))) +
  geom_text(aes(x=c(0, 1),
                y=min(YR2013 - YR2013SE - 1),
                label=c("2013", "2017")), 
            col= "grey30",
            hjust="right", size = 6, 
            fontface = "bold") +
  geom_text(aes(x=0-0.15,
                y=YR2013,
                label=stringr::str_wrap(paste0(FOS), width = 25)),
            hjust="right") +
  geom_text(aes(x = 0-0.03,
                y = YR2013,
                label = paste0(round(YR2013, 1), "%")),
            hjust
            ="right",
            col="grey30") +
  geom_text(aes(x=1+0.08,
                y=YR2017,
                label=paste0(round(YR2017, 1), "%")),
            col="grey30") +
  labs(title="Labor force participation rate among \nU.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017",
       caption = "The shaded tails show the standard error.") +
  theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
  theme_classic()+
  theme_void() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.position = "none")

#slope dots MULTI
ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun2(0.1), yend = fun2(0.9), color = FOS),size = 1.2) +
  geom_segment(aes(x =0, xend = .1, y = fun2(0), yend = fun2(.1), color = FOS),  size = 1.2, linetype ="dotted") +
  geom_segment(aes(x =0.9, xend = 1, y = fun2(0.9), yend = fun2(1), color = FOS),  size = 1.2, linetype ="dotted") +
  scale_color_manual(values = c(cbPalette[2], cbPalette[3], cbPalette[4], cbPalette[5], cbPalette[6], cbPalette[7], cbPalette[8]))+
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(86, 94)) +
  geom_polygon(aes(x =x, y = y, group = id, fill = id), alpha = 0.2) +
  scale_fill_manual(values = c( rep(c(cbPalette[2], cbPalette[3], cbPalette[4], cbPalette[5], cbPalette[6], cbPalette[7], cbPalette[8]), 2))) +
  geom_text(aes(x=0-0.15,
                y=YR2013,
                label=stringr::str_wrap(paste0(FOS), width = 25)),
            hjust="right")  +
  geom_text(aes(x = 0-0.03,
                y = YR2013,
                label = paste0(round(YR2013, 1), "%")),
            hjust
            ="right",
            col="grey30") +
  geom_text(aes(x=1+0.08,
                y=YR2017,
                label=paste0(round(YR2017, 1), "%")),
            col="grey30") +
  labs(title="Labor force participation rate among \nU.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017",
       caption = "The shaded tails show the standard error.") +
  theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
  theme_classic()+
  theme_void() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.position = "none")



#slope dots TWOCOLOR
ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun2(0.1), yend = fun2(0.9), color = DIF),size = 1.2) +
  geom_segment(aes(x =0, xend = .1, y = fun2(0), yend = fun2(.1), color = DIF),  size = 1.2, linetype ="dotted") +
  geom_segment(aes(x =0.9, xend = 1, y = fun2(0.9), yend = fun2(1), color = DIF),  size = 1.2, linetype ="dotted") +
  scale_color_manual(values = c(cbPalette[2], cbPalette[3]))+
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(86, 94)) +
  geom_polygon(aes(x =x, y = y, group = id, fill = rep(DIF, 6)), alpha = 0.2) +
  scale_fill_manual(values = c( rep(c(cbPalette[2], cbPalette[3], cbPalette[4], cbPalette[5], cbPalette[6], cbPalette[7], cbPalette[8]), 2))) +
  geom_text(aes(x=0-0.15,
                y=YR2013,
                label=stringr::str_wrap(paste0(FOS), width = 25)),
            hjust="right")  +
  geom_text(aes(x = 0-0.03,
                y = YR2013,
                label = paste0(round(YR2013, 1), "%")),
            hjust
            ="right",
            col="grey30") +
  geom_text(aes(x=1+0.08,
                y=YR2017,
                label=paste0(round(YR2017, 1), "%")),
            col="grey30") +
  labs(title="Labor force participation rate among \nU.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017",
       caption = "The shaded tails show the standard error.") +
  theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
  theme_classic()+
  theme_void() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.position = "none")



#slope line MULTI

ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun2(0.1), yend = fun2(0.9), color = FOS),size = 1.2) +
  geom_segment(aes(x =0, xend = .1, y = fun2(0), yend = fun2(.1), color = FOS),  size = 1.2) +
  geom_segment(aes(x =0.9, xend = 1, y = fun2(0.9), yend = fun2(1), color = FOS),  size = 1.2) +
  scale_color_manual(values = c(cbPalette[2], cbPalette[3], cbPalette[4], cbPalette[5], cbPalette[6], cbPalette[7], cbPalette[8]))+
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(min(YR2013 - YR2013SE - 2), 94)) +
  geom_polygon(aes(x =x, y = y, group = id, fill = id), alpha = 0.2) +
  scale_fill_manual(values = c( rep(c( cbPalette[2], cbPalette[3], cbPalette[4], cbPalette[5], cbPalette[6], cbPalette[7], cbPalette[8]), 2))) +
  geom_text(aes(x=c(0, 1),
                y=min(YR2013 - YR2013SE - 1),
                label=c("2013", "2017")), 
            col= "grey30",
            hjust="right", size = 6, 
            fontface = "bold") +
  geom_text(aes(x=0-0.15,
                y=YR2013,
                label=stringr::str_wrap(paste0(FOS), width = 25)),
            hjust="right") +
  geom_text(aes(x = 0-0.03,
                y = YR2013,
                label = paste0(round(YR2013, 1), "%")),
            hjust
            ="right",
            col="grey30") +
  geom_text(aes(x=1+0.08,
                y=YR2017,
                label=paste0(round(YR2017, 1), "%")),
            col="grey30") +
  labs(title="Labor force participation rate among \nU.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017",
       caption = "The shaded tails show the standard error.") +
  theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
  theme_classic()+
  theme_void() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.position = "none")

#slope line TWO

ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun2(0.1), yend = fun2(0.9), color = DIF),size = 1.2) +
  geom_segment(aes(x =0, xend = .1, y = fun2(0), yend = fun2(.1), color = DIF),  size = 1.2) +
  geom_segment(aes(x =0.9, xend = 1, y = fun2(0.9), yend = fun2(1), color = DIF),  size = 1.2) +
  scale_color_manual(values = c(cbPalette[2], cbPalette[3], cbPalette[4], cbPalette[5], cbPalette[6], cbPalette[7], cbPalette[8]))+
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(min(YR2013 - YR2013SE - 2), 94)) +
  geom_polygon(aes(x =x, y = y, group = id, fill = rep(DIF, 6)), alpha = 0.2) +
  scale_fill_manual(values = c( rep(c( cbPalette[2], cbPalette[3], cbPalette[4], cbPalette[5], cbPalette[6], cbPalette[7], cbPalette[8]), 2))) +
  geom_text(aes(x=c(0, 1),
                y=min(YR2013 - YR2013SE - 1),
                label=c("2013", "2017")), 
            col= "grey30",
            hjust="right", size = 6, 
            fontface = "bold") +
  geom_text(aes(x=0-0.15,
                y=YR2013,
                label=stringr::str_wrap(paste0(FOS), width = 25)),
            hjust="right") +
  geom_text(aes(x = 0-0.03,
                y = YR2013,
                label = paste0(round(YR2013, 1), "%")),
            hjust
            ="right",
            col="grey30") +
  geom_text(aes(x=1+0.08,
                y=YR2017,
                label=paste0(round(YR2017, 1), "%")),
            col="grey30") +
  labs(title="Labor force participation rate among \nU.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017",
       caption = "The shaded tails show the standard error.") +
  theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
  theme_classic()+
  theme_void() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.position = "none")

