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

theme_Palette<-c("#1B3766", "#02ABD6", "#6DD4DB", "#A9D5A5", "#F17E1D")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col=col, radius=radius)
wheel(theme_Palette)




fun <- function(x){
  (.2*x)+89.7
}

# only plotting one
#
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
  








# plot multiple

fun2 <- function(x){
  ((YR2017-YR2013)*x) + YR2013 
}

ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun2(0.1), yend = fun2(0.9), color = FOS), size = 2) +
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(86, 94)) +
  geom_point(aes(x =0, y = YR2013-YR2013SE, color = FOS)) +
  geom_point(aes(x =0, y = YR2013+YR2013SE, color = FOS)) +
  geom_point(aes(x =1, y = YR2017+YR2017SE, color = FOS)) +
  geom_point(aes(x =1, y = YR2017-YR2017SE, color = FOS)) +
  geom_curve(aes(x = 0, xend = .10, y = YR2013+YR2013SE, yend = fun2(.1), color = FOS), curvature = .3, angle = 90) +
  geom_curve(aes(x = 0, xend = .10, y = YR2013-YR2013SE, yend = fun2(.1), color = FOS), curvature = -.3, angle = 90) +
  geom_curve(aes(x = 1, xend = .90, y = YR2017+YR2017SE, yend = fun2(.9), color = FOS), curvature = -.3, angle = 90) +
  geom_curve(aes(x = 1, xend = .90, y = YR2017-YR2017SE, yend = fun2(.9), color = FOS), curvature = .3, angle = 90) +
  geom_text(aes(x=0-0.1,
                y=YR2013,
                label=paste0(FOS), 
                col= FOS),
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
  labs(title="Labor force participation rate among U.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017") +
  theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
  theme_classic()+
  theme_void() +
  theme(axis.line=element_blank(),
  axis.text=element_blank(),
  axis.title=element_blank(),
  axis.ticks=element_blank(), 
  plot.title = element_text(hjust = 0.5), 
  legend.position = "none") 

#attempt to understand curved lines for polygon
gg <- ggplot() +
  geom_curve(aes(x = 0, xend = .10, y = YR2013+YR2013SE, yend = fun2(.1), color = FOS), curvature = .3, angle = 90) 


build <- ggplot_build(gg)

