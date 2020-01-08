#https://ncsesdata.nsf.gov/doctoratework/2017/html/sdr2017_dst_09.html

Category <- c("Science", "Engineering", "Health")
Category2 <- c(1, 2, 3)
Number <- c(612450, 164650, 37950)
SE <- c(1500, 850, 425)
data <-data.frame(Category, Category2, Number, SE)
data$per <- (data$SE/data$Number)*100

library(ggplot2)
library(scatterpie)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#Lollipop with standard error as error bars
ncsesplot <-ggplot(data, aes(x=Category, y=Number, label=Number)) +
  geom_segment(aes(x=Category, xend=Category, y=0, yend=Number), color="grey", lwd=3) +
  geom_point(color=cbPalette[1], size=16) +
  geom_errorbar(aes(ymin = Number - SE, ymax = Number + SE), width=0.2)+
 # geom_point(color=rep(c("#FAFAFA","#C7C7C7","#949494"),c(7,7,7)), size=10) #+
  geom_text(colour="black", size=4, vjust = 0.5, hjust = -.75) +
  theme_minimal() + #removed theme_val(), cannot find package
  coord_flip() +
  ylab("Number") + xlab("") +
  #annotate("text", y=50, x=1.5, label="Reference Values", colour="#2a2a2b", size=5) +
  scale_y_continuous(breaks=seq(from=0, to=700000, by=100000),
                    labels=c("0","100000","200000","300000","400000","500000","600000","700000"),
                    limits=c(0, 700000)) + 
  labs(title="U.S. residing employed doctoral scientists and engineers, by field of doctorate ") +
  theme(axis.text.y=element_text(size=13, hjust=1.0, colour="#2a2a2b"),
        axis.text.x = element_text(angle = 90),
        plot.title=element_text(hjust=0, size=15),
        plot.subtitle=element_text(hjust=0, size=13),
        axis.title.x=element_text(size=14, hjust=0.5, colour="#2a2a2b")) +
  theme(aspect.ratio = 1/5)
ncsesplot





#Lollipop with circle standard deviation
ncsesplot2 <-ggplot(data, aes(x=Category2, y=Number, label=Number)) +
  geom_segment(aes(x=Category2, xend=Category2, y=0, yend=Number), color="grey", lwd=3) +
  geom_point(aes(size = (SE*2)), pch = 1, stroke = 2, color = cbPalette[8]) +
  geom_point( color = "black") +
  # geom_point(color=rep(c("#FAFAFA","#C7C7C7","#949494"),c(7,7,7)), size=10) #+
  geom_text(colour="black", size=4, vjust = .5, hjust = -.75) +
  theme_minimal() + #removed theme_val(), cannot find package
  coord_flip() +
  ylab("Number") + xlab("") +
  #annotate("text", y=50, x=1.5, label="Reference Values", colour="#2a2a2b", size=5) +
  scale_y_continuous(breaks=seq(from=0, to=700000, by=100000),
                     labels=c("0","100000","200000","300000","400000","500000","600000","700000"),
                     limits=c(0, 700000)) + 
  scale_x_continuous(breaks = seq(from = 1, to = 3, by= 1),
                     labels = Category) +
  labs(title="U.S. residing employed doctoral scientists and engineers, by field of doctorate ") +
  theme(axis.text.y=element_text(size=13, hjust=1.0, colour="#2a2a2b"),
        axis.text.x = element_text(angle = 90),
        plot.title=element_text(hjust=0, size=15),
        plot.subtitle=element_text(hjust=0, size=13),
        axis.title.x=element_text(size=14, hjust=0.5, colour="#2a2a2b"))+
  theme(aspect.ratio = 1/5)
ncsesplot2

library(ggforce)



#Lollipop with pie graph dots!

data$notSE <- data$Number - data$SE

# y = [0, 700000]
fun <- function (y){
  (y/350000) +1
}

#x = [1, 3]
revfun <- function(x){
  (350000*x) - 350000
}

seq <- seq(from = 1, to = 3, by = .25)

ggplot() +
  geom_segment(aes(x=Category2, xend=Category2, y=fun(0), yend=fun(Number)), color="grey", lwd=3) +
  geom_scatterpie(aes(x = Category2, y = fun(Number), group = Category), data = data, cols = c( "SE", "notSE")) +
  scale_x_continuous(breaks = seq(from = 1, to = 3, by= 1), labels = Category) +
  scale_y_continuous(breaks = seq(from = 1, to = 3, by= .25), 
                     labels = revfun(seq), 
                     limits = c(1,3))+
  coord_equal(ratio = 1)+
labs(title="U.S. residing employed doctoral scientists and engineers, by field of doctorate") +
theme_minimal() +
  ylab("Count of US Residing Employed Doctoral Scientists and Engineers")










  theme(axis.text.y=element_text(size=13, hjust=1.0, colour="#2a2a2b"),
        axis.text.x = element_text(angle = 90),
        plot.title=element_text(hjust=0, size=15),
        plot.subtitle=element_text(hjust=0, size=13),
        axis.title.x=element_text(size=14, hjust=0.5, colour="#2a2a2b"))


# idea for lollipop
Category <- c("Science", "Engineering", "Health")
Number <- c(612450, 164650, 37950)
SE <- c(60000, 16000, 3700)
test <-data.frame(Category, Number, SE)
test$posSE <- test$Number + test$SE
test$negSE <- test$Number - test$SE

library(dplyr)
library(tidyr)
library(gganimate)

test <- test %>% gather(Type, Value, c("Number", "posSE", "negSE"))

plot <- ggplot(test) +
  geom_segment(aes(x=Category, xend=Category, y=0, yend=Value), color="grey", lwd=3)

new <- plot + transition_states(Value, transition_length = 2, state_length = 1)
new  


animate(new, nframes = 24, renderer = gifski_renderer("gganim.gif"))

#look at double lollipop
#statebins


#https://ncsesdata.nsf.gov/doctoratework/2017/html/sdr2017_dst_09.html
library(data.table)
 library(readxl)
x <- read_excel("sdr2017.xlsx")
x<- x[-c(1:2),-c(2:5, 10:11)]


colnames(x) <- c("FieldStudy", "NativeBornNumb", "NativeBornSE", "NaturalizedNumb", "NaturalizedSE", "PermResidentNumb", "PermResidentSE", "TempResidentNumb", "TempResidentSE")
x <- x[-c(1:3, 102:106),]


 x <-x %>% filter(FieldStudy %like% "engineering|Engineering")

x <- x[-c(1, 5,10), ]

x$NativeBornNumb <- as.numeric(x$NativeBornNumb)
x$NativeBornSE <- as.numeric(x$NativeBornSE)

x$NaturalizedNumb <- as.numeric(x$NaturalizedNumb)
x$NaturalizedSE <- as.numeric(x$NaturalizedSE)

x$PermResidentNumb <- as.numeric(x$PermResidentNumb)
x$PermResidentSE <- as.numeric(x$PermResidentSE)

x$TempResidentNumb <- as.numeric(x$TempResidentNumb)
x$TempResidentSE <- as.numeric(x$TempResidentSE)

x$FieldStudy <- as.factor(x$FieldStudy)

x <-x %>% gather("NativeBornNumb", "NativeBornSE", "NaturalizedNumb", "NaturalizedSE", "PermResidentNumb", "PermResidentSE", "TempResidentNumb", "TempResidentSE", key = "Number", value = "Value")

test <- x[c(27:52), ]

test[c(14:26), ] <- test  %>% filter(Number == "NaturalizedSE") %>% mutate(Value = -Value, Number = "-NaturalizedSE")

theme_Palette<-c("#1B3766", "#02ABD6", "#6DD4DB", "#A9D5A5", "#F17E1D")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col=col, radius=radius)
wheel(theme_Palette)





#double lollipop!

ggplot(x, aes(x = NaturalizedNumb, y = fct_reorder(FieldStudy, NaturalizedNumb)) ) +
  geom_segment( aes(x=0, xend=NaturalizedNumb, yend=fct_reorder(FieldStudy, NaturalizedNumb)), color= theme_Palette[1],  lwd=2) +
  geom_point(aes(x = NaturalizedNumb), size = 5, color= theme_Palette[1]) +
  geom_segment(aes(x = 0, xend = -NaturalizedSE, yend = fct_reorder(FieldStudy, NaturalizedNumb)), lwd = 2, color = theme_Palette[5]) +
  geom_point(aes(x = -NaturalizedSE), , color = theme_Palette[5], size=5) +
  theme_minimal() +
  labs(title="U.S. Naturalized Residing Employed Doctoral Engineers, by Field of Doctorate ", 
       y = "", 
       x = "Number of Naturalized U.S. Residing Employed Doctoral Engineers") +
  scale_x_continuous(breaks = seq(from = 0, to = 15000, by = 2500))+
  theme(axis.text.y=element_text(size=13, hjust=1.0, colour="#2a2a2b"),
        axis.text.x = element_text(size = 13),
        plot.title=element_text(hjust=0.5, size=20, face = "bold"),
        plot.subtitle=element_text(hjust=0, size=13),
        axis.title.x=element_text(size=14, hjust=0.5, colour="#2a2a2b")) 




ggplot(x, aes(x = NaturalizedNumb, y = fct_reorder(FieldStudy, NaturalizedNumb)) ) +
  geom_segment( aes(x=0, xend=NaturalizedNumb, yend=fct_reorder(FieldStudy, NaturalizedNumb), color = theme_Palette[1]),  lwd=2) +
  geom_point(aes(x = NaturalizedNumb), size = 5, color= theme_Palette[1]) +
  geom_segment(aes(x = 0, xend = -NaturalizedSE, yend = fct_reorder(FieldStudy, NaturalizedNumb), color = theme_Palette[5]), lwd = 2) +
  geom_point(aes(x = -NaturalizedSE), , color = theme_Palette[5], size=5) +
  theme_minimal() +
  labs(title="U.S. Naturalized Residing Employed Doctoral Engineers, by Field of Doctorate ", 
       y = "", 
       x = "Number of Naturalized U.S. Residing Employed Doctoral Engineers") +
  scale_x_continuous(breaks = seq(from = 0, to = 15000, by = 2500))+
  scale_colour_manual(values = c(theme_Palette[1], theme_Palette[5]))+
  theme(axis.text.y=element_text(size=13, hjust=1.0, colour="#2a2a2b"),
        axis.text.x = element_text(size = 13),
        plot.title=element_text(hjust=0.5, size=20, face = "bold"),
        plot.subtitle=element_text(hjust=0, size=13),
        axis.title.x=element_text(size=14, hjust=0.5, colour="#2a2a2b")) 










ggplot(test, aes(x = Value, y = fct_reorder(FieldStudy, Value), color = Number) ) +
  geom_segment( aes(x=0, xend=Value, yend=FieldStudy),  lwd=2) +
  geom_point(aes(x = Value), size = 5) +
  scale_colour_manual(name = NULL, labels = c("Standard Error", "Count"), values = c(theme_Palette[5], theme_Palette[1]))+
  theme_minimal() +
  labs(title="U.S. Naturalized Residing Employed Doctoral Engineers, \nby Field of Doctorate ", 
       y = "", 
       x = "Number of Naturalized U.S. Residing Employed Doctoral Engineers") +
  scale_x_continuous(breaks = seq(from = 0, to = 15000, by = 2500), 
                     limits = c(-1000,16000))+
  theme(axis.text.y=element_text(size=13, hjust=1.0, colour="#2a2a2b"),
        axis.text.x = element_text(size = 13),
        plot.title=element_text(hjust=0.5, size=20, face = "bold"),
        plot.subtitle=element_text(hjust=0, size=13),
        axis.title.x=element_text(size=14, hjust=0.5, colour="#2a2a2b"), 
        legend.position = "bottom") 
