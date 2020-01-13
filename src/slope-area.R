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

theme_Palette<-c("#1B3766", "#02ABD6", "#6DD4DB", "#A9D5A5", "#F17E1D")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col=col, radius=radius)
wheel(theme_Palette)
y <- c(YR2013+YR2013SE, YR2013-YR2013SE, fun2(.1), YR2017+YR2017SE, YR2017-YR2017SE, fun2(.9))
x <- rep(c(0, 0, .1, 1, 1, .9), each = 7)
id <- rep(c("a", "b", "c", "d", "e", "f", "g"), 3)
id2 <- rep(c("h", "i", "j", "k", "l", "m", "n"), 3)
id <- data.frame(id)
id2 <- data.frame(id2)
colnames(id2)[colnames(id2)=="id2"] <- "id"
id <- rbind(id, id2)
id <- id$id



# slope area



ggplot() +
geom_segment(aes(x = 0.1, xend = .9, y = fun2(0.1), yend = fun2(0.9), color = FOS),
size = 1.2) +
scale_color_manual(values = c(theme_Palette[1], theme_Palette[2], theme_Palette[3], theme_Palette[4], theme_Palette[5], "red", "purple"))+
scale_x_continuous(limits=c(-0.5, 1.25)) +
scale_y_continuous(limits = c(86, 94)) +
geom_polygon(aes(x =x, y = y, group = id, fill = id), alpha = 0.2) +
scale_fill_manual(values = c( rep(c(theme_Palette[1], theme_Palette[2], theme_Palette[3], theme_Palette[4], theme_Palette[5], "red", "purple"), 2))) +
geom_text(aes(x=0-0.1,
y=YR2013,
label=paste0(FOS),
col= FOS),
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
labs(title="Labor force participation rate among U.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017",
caption = "The shaded tails show the standard error.") +
theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
theme_classic()+
theme_void() +
theme(axis.line=element_blank(),
axis.text=element_blank(),
axis.title=element_blank(),
axis.ticks=element_blank(),
plot.title = element_text(hjust = 0.5),
legend.position = "none")

#slope dots
ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun2(0.1), yend = fun2(0.9), color = FOS),size = 1.2) +
  geom_segment(aes(x =0, xend = .1, y = fun2(0), yend = fun2(.1), color = FOS),  size = 1.2, linetype ="dotted") +
  geom_segment(aes(x =0.9, xend = 1, y = fun2(0.9), yend = fun2(1), color = FOS),  size = 1.2, linetype ="dotted") +
  scale_color_manual(values = c(theme_Palette[1], theme_Palette[2], theme_Palette[3], theme_Palette[4], theme_Palette[5], "red", "purple"))+
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(86, 94)) +
  geom_polygon(aes(x =x, y = y, group = id, fill = id), alpha = 0.2) +
  scale_fill_manual(values = c( rep(c(theme_Palette[1], theme_Palette[2], theme_Palette[3], theme_Palette[4], theme_Palette[5], "red", "purple"), 2))) +
  geom_text(aes(x=0-0.1,
                y=YR2013,
                label=paste0(FOS),
                col= FOS),
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
  labs(title="Labor force participation rate among U.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017",
       caption = "The shaded tails show the standard error.") +
  theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
  theme_classic()+
  theme_void() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

#slope line

ggplot() +
  geom_segment(aes(x = 0.1, xend = .9, y = fun2(0.1), yend = fun2(0.9), color = FOS),size = 1.2) +
  geom_segment(aes(x =0, xend = .1, y = fun2(0), yend = fun2(.1), color = FOS),  size = 1.2) +
  geom_segment(aes(x =0.9, xend = 1, y = fun2(0.9), yend = fun2(1), color = FOS),  size = 1.2) +
  scale_color_manual(values = c(theme_Palette[1], theme_Palette[2], theme_Palette[3], theme_Palette[4], theme_Palette[5], "red", "purple"))+
  scale_x_continuous(limits=c(-0.5, 1.25)) +
  scale_y_continuous(limits = c(86, 94)) +
  geom_polygon(aes(x =x, y = y, group = id, fill = id), alpha = 0.2) +
  scale_fill_manual(values = c( rep(c(theme_Palette[1], theme_Palette[2], theme_Palette[3], theme_Palette[4], theme_Palette[5], "red", "purple"), 2))) +
  geom_text(aes(x=0-0.1,
                y=YR2013,
                label=paste0(FOS),
                col= FOS),
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
  labs(title="Labor force participation rate among U.S. residing doctoral scientists and engineers, \nby field of doctorate: 2013 and 2017",
       caption = "The shaded tails show the standard error.") +
  theme(plot.title=element_text(size=17, colour="#2a2a2b")) +
  theme_classic()+
  theme_void() +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

