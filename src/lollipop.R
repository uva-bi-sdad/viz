#Dataset from https://ncsesdata.nsf.gov/doctoratework/2017/html/sdr2017_dst_09.html
library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scatterpie)
library(ggforce)
dataset <- read_excel("/home/sm9dv/viz/data/viz/original/sdr2017.xlsx")

theme_Palette<-c("#1B3766", "#02ABD6", "#6DD4DB", "#A9D5A5", "#F17E1D")
wheel <- function(col, radius = 1, ...)
  
  pie(rep(1, length(col)), col=col, radius=radius)
wheel(theme_Palette)

#Clean dataset
dataset <- dataset[-c(1:2, 104:108),-c(2:5, 10:11)]
colnames(dataset) <- c("FieldStudy", "NativeBornNumb", "NativeBornSE", "NaturalizedNumb", "NaturalizedSE", "PermResidentNumb", "PermResidentSE", "TempResidentNumb", "TempResidentSE")
dataset <- dataset[-c(1:3),]

#Engineers only
eng <-dataset %>% filter(FieldStudy %like% "engineering|Engineering")
#this avoids annoying errors later on
eng = as.data.frame(eng)
#Remove totals rows - I am interested in granular area of study
eng <- eng[-c(1, 5,10), ]

#Remove "engineering" - redundant on labels

eng$FieldStudy <- gsub("(e|E)ngineering,?", "", eng$FieldStudy)
eng$FieldStudy <- trimws(eng$FieldStudy, "both")
eng$FieldStudy <- paste(toupper(substr(eng$FieldStudy, 1, 1)), substr(eng$FieldStudy, 2, nchar(eng$FieldStudy)), sep="")



eng[, 2:9]<- sapply(eng[, 2:9], as.numeric)
eng$FieldStudy <- as.factor(eng$FieldStudy)

eng.long <- eng %>% gather("NativeBornNumb", "NativeBornSE", "NaturalizedNumb", "NaturalizedSE", "PermResidentNumb", "PermResidentSE", "TempResidentNumb", "TempResidentSE", key = "Number", value = "Value")

eng.long.nat  <- eng.long %>% filter(Number %in% c("NaturalizedNumb", "NaturalizedSE"))

#negate the standard error so that it goes in the other direction
eng.long.nat[c(14:26), ] <- eng.long.nat  %>% filter(Number == "NaturalizedSE") %>% mutate(Value = -Value, Number = "-NaturalizedSE")

#Double Lollipop

ggplot(eng.long.nat, aes(x = Value, y = fct_reorder(FieldStudy, Value), color = Number) ) +
  geom_segment( aes(x=0, xend=Value, yend=FieldStudy),  lwd=2) +
  geom_point(aes(x = Value), size = 5) +
  scale_colour_manual(name = NULL, labels = c("Standard Error", "Count"), values = c(theme_Palette[5], theme_Palette[1]))+
  theme_minimal() +
  labs(title="U.S. Naturalized Residing Employed Doctoral Engineers, \nby Field of Doctorate ", 
       y = "", 
       x = "Number of Naturalized U.S. Residing Employed Doctoral Engineers") +
  scale_x_continuous(breaks = c(seq(from = 0, to = 15000, by = 2500)),
                     limits = c(-1000,16000))+
  theme(axis.text.y=element_text(size=13, hjust=1.0, colour="#2a2a2b"),
        axis.text.x = element_text(size = 13),
        plot.title=element_text(hjust=0.5, size=20, face = "bold"),
        plot.subtitle=element_text(hjust=0, size=13),
        axis.title.x=element_text(size=14, hjust=0.5, colour="#2a2a2b"), 
        legend.position = "bottom") 




#Double Lollipop minor breaks THIS ONE IS BETTER
ggplot(eng.long.nat, aes(x = Value, y = fct_reorder(FieldStudy, Value), color = Number) ) +
  geom_segment( aes(x=0, xend=Value, yend=FieldStudy),  lwd=2) +
  geom_point(aes(x = Value), size = 5) +
  scale_colour_manual(name = NULL, labels = c("Standard Error", "Count"), values = c(theme_Palette[5], theme_Palette[1]))+
  theme_minimal() +
  labs(title="U.S. Naturalized Residing Employed Doctoral Engineers, \nby Field of Doctorate ", 
       y = "", 
       x = "Number of Naturalized U.S. Residing Employed Doctoral Engineers") +
  scale_x_continuous(breaks = c(-1000, seq(from = 0, to = 15000, by = 2500)), 
                     minor_breaks = c(seq(from= -1000, to = 0, by = 250)),
                     limits = c(-1000,16000))+
  theme(axis.text.y=element_text(size=13, hjust=1.0, colour="#2a2a2b"),
        axis.text.x = element_text(size = 13),
        plot.title=element_text(hjust=0.5, size=20, face = "bold"),
        plot.subtitle=element_text(hjust=0, size=13),
        axis.title.x=element_text(size=14, hjust=0.5, colour="#2a2a2b"), 
        legend.position = "bottom") 


#Lollipop with Pie Chart dots
eng.nat <- eng %>%select("FieldStudy", "NaturalizedNumb", "NaturalizedSE")

eng.nat$propSE <- eng.nat$NaturalizedSE/eng.nat$NaturalizedNumb
eng.nat$propSEnot <- 1 - eng.nat$propSE



eng.nat <-eng.nat[with(eng.nat, order(NaturalizedNumb)),]
eng.nat$position <- seq(1:13)


#y = [0, 15000]
fun <- function(y){
  (y/1250)+1
}

#x = [1, 13]
invfun <- function(x){
  (1250*x) - 1250
}

ggplot(eng.nat) +
  geom_segment(aes(x=fun(0), xend=fun(NaturalizedNumb), y=position, yend=position), lwd=3, color = theme_Palette[1]) +
  geom_scatterpie(aes(x = fun(NaturalizedNumb), y = position, r = 0.3), data = eng.nat, cols = c("propSE", "propSEnot"), color = theme_Palette[1]) +
  coord_equal(ratio = 1)+
  scale_fill_manual(name = NULL, labels = c("Proportion of Standard Error",  "Count"), values = c(theme_Palette[5], theme_Palette[1]))+
  scale_y_continuous(breaks = seq(from = 1, to = 13, by= 1), labels = eng.nat$FieldStudy) +
  scale_x_continuous(breaks = seq(from = 1, to = 13, by= 2), 
                     labels = invfun(seq(from = 1, to = 13, by = 2)), 
                     limits = c(1,13))+
  theme_minimal()+
  theme(axis.text.y=element_text(size=13, colour="#2a2a2b"),
        axis.text.x = element_text(size = 13),
        plot.title=element_text(hjust=0.5, size=20, face = "bold"),
        plot.subtitle=element_text(hjust=0, size=13),
        axis.title.x=element_text(size=14, colour="#2a2a2b"), 
        legend.position = "bottom")+
  labs(title="U.S. Naturalized Residing Employed Doctoral Engineers, \nby Field of Doctorate ", 
       y = "", 
       x = "Number of Naturalized U.S. Residing Employed Doctoral Engineers")
  

# Variability as line thickness
# we restrict line thickness to [1, 5] and map it to values equal to the max and min of standard error. Thicker lines are higher variability.


#[1,5]
seg.fun <- function(x){
  ((max(eng.nat$NaturalizedSE) - min(eng.nat$NaturalizedSE))/(5-1))*(x-1)+75
}

#[75, 450]
inv.seg.fun <- function(y){
  ((y-75)/((max(eng.nat$NaturalizedSE) - min(eng.nat$NaturalizedSE))/(5-1))) +1
}


ggplot(eng.nat) +
  geom_segment(aes(x=0, xend=NaturalizedNumb, y=position, yend=position), lwd=inv.seg.fun(eng.nat$NaturalizedSE),    color = theme_Palette[1]) +
  geom_point(aes(x = NaturalizedNumb, y = position), size = 6, color = theme_Palette[1]) +
  scale_y_continuous(breaks = seq(from = 1, to = 13, by= 1), labels = eng.nat$FieldStudy) +
  scale_x_continuous(breaks = c(seq(from = 0, to = 15000, by = 2500)), 
                     limits = c(0, 15000))+
  theme_minimal()+
  theme(axis.text.y=element_text(size=13, colour="#2a2a2b"),
        axis.text.x = element_text(size = 13),
        plot.title=element_text(hjust=0.5, size=20, face = "bold"),
        plot.subtitle=element_text(hjust=0, size=13),
        axis.title.x=element_text(size=14, colour="#2a2a2b"), 
        legend.position = "bottom", 
        plot.caption = element_text(hjust = 0, size = 12, face = "italic"))+
  labs(title="U.S. Naturalized Residing Employed Doctoral Engineers, \nby Field of Doctorate ", 
       y = "", 
       x = "Number of Naturalized U.S. Residing Employed Doctoral Engineers", 
       caption = "Thickness of segment indicates standard error. Electrical, electronics, and communications engineering \nhas the highest error (SE = 450), and Agricultural Engineering has the lowest error (SE = 75).")

#size of circle

fun <- function(y){
  (y/1250)+1
}

#x = [1, 13]
invfun <- function(x){
  (1250*x) - 1250
}


ggplot(eng.nat) +
  geom_segment(aes(x = 0, xend = NaturalizedNumb, y = invfun(position), yend = invfun(position)), lwd = 1,  color = theme_Palette[1])+
  geom_circle(aes(x0 = NaturalizedNumb, y0 = invfun(position), r = NaturalizedSE), data = eng.nat, color = theme_Palette[5]) +
  coord_fixed() +
  theme_minimal() +
  scale_y_continuous(breaks = seq(from = invfun(1), to = invfun(13), by= invfun(fun(15000/12))), labels = eng.nat$FieldStudy) +
  scale_x_continuous(breaks = seq(0, 15000, by= (15000/6))) +
  theme(axis.text.y=element_text(size=13, colour="#2a2a2b"),
        axis.text.x = element_text(size = 13),
        plot.title=element_text(hjust=0.5, size=20, face = "bold"),
        plot.subtitle=element_text(hjust=0, size=13),
        axis.title.x=element_text(size=14, colour="#2a2a2b"), 
        legend.position = "bottom")+
  labs(title="U.S. Naturalized Residing Employed Doctoral Engineers, \nby Field of Doctorate ", 
       y = "", 
       x = "Number of Naturalized U.S. Residing Employed Doctoral Engineers")




