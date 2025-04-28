# Population density plot for all Seychelles warbler islands - Chuen 28/04/2025
library(ggthemes)
library(tidyverse)
library(ggpubr)
library(ggbreak)

# make sure to update AllPopDen before continuing 
popdenallislands <- read_csv("~/Documents/PhD/R_analysis/Weather/AllPopDen.csv") 
popdenallislandsin <- popdenallislands %>% 
  mutate_if(is.character,as.numeric) %>% 
  pivot_longer(cols = -Year ) %>% 
  filter(!(name %in% "Total")) %>% 
  mutate(predicted = as.factor(case_when(name %in% "Denis" & Year %in% c("2022") | name %in% "Denis" & Year %in% c("2013") ~ "ID1", 
                                         name %in% "Aride" & Year %in% c("1993") | name %in% "Aride" & Year %in% c("1994") ~ "ID2" , 
                                         name %in% "Fregate" & Year %in% c("2013") | name %in% "Fregate" & Year %in% c("2022") ~ "ID3"  ))) %>% 
  arrange(Year) %>% 
  mutate(Year2 = as.factor(Year))

#write.csv(popdenallislandsin , "~/Downloads/popdensomeisland.csv")

# popdenforgeompoint is when an actual population estimate was generated from fieldwork, not just extrapolations from previous years - update this before continuing
popdenforgeompoint <- read_csv("~/Downloads/popdenforgeompoint.csv")

# make plots
p1 <- ggplot(popdenallislandsin, aes(Year, value, colour=name)) + 
  xlab("Year") + 
  ylab("Population size (number of independent Seychelles warblers)") + 
  geom_line(data=popdenallislandsin[!is.na(popdenallislandsin$value),],aes(group=name), linetype=1)  + 
  geom_point(data = popdenforgeompoint, aes(Year,value, colour=name)) + 
  scale_color_manual(name = "Island", values=c("black","#E69f00","#56b4e9","#009e73","#D55E00")) + 
  theme_tufte(base_size = 13, base_family = "Arial") + 
  theme(axis.line = element_line(colour = "black", linetype=1)) + 
  scale_x_continuous(breaks = round(seq(min(popdenallislandsin$Year), max(popdenallislandsin$Year), by = 1),1))  + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10)) 

p1


popdenallislandsin2 <- popdenallislands %>% mutate_if(is.character,as.numeric) %>% pivot_longer(cols = -Year ) %>% filter((name %in% "Total"))

p3 <- ggplot(popdenallislandsin2, aes(Year, value)) + 
  geom_line(data=popdenallislandsin2[!is.na(popdenallislandsin2$value),]) +
  geom_point() + 
  xlab("Year") + 
  ylab("Population size (number of independent Seychelles warblers)")+ 
  theme_tufte(base_size = 13, base_family = "Arial") + 
  theme(axis.line = element_line(colour = "black", linetype=1)) + 
  scale_x_continuous(breaks = round(seq(min(popdenallislandsin$Year), max(popdenallislandsin$Year), by = 1),1))  + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10))+ 
  coord_cartesian(xlim= c(1959.5,2021.5),ylim=c(90,4000))

p3

p1 + scale_y_cut(breaks = c(500), expand=T) +  
  geom_hline(yintercept = 1000) +
  annotate("text", x=1965, y=900, label= "Census population estimates")+
  annotate("text", x=1965, y=1100, label= "Extrapolated population estimates") + 
  coord_cartesian(xlim= c(1959.5,2021),ylim=c(90,3000))


p1 + coord_cartesian(xlim= c(1959.5,2021.5),ylim=c(18,400)) + 
  theme(legend.position="bottom")

p1 + coord_cartesian(xlim= c(1959.5,2021),ylim=c(500,2000)) + 
  geom_hline(yintercept = 1000) +
  annotate("text", x=1965, y=900, label= "Census population estimates")+
  annotate("text", x=1965, y=1100, label= "Extrapolated population estimates") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(legend.position="none")

ggarrange(p1 + coord_cartesian(xlim= c(1959.5,2021),ylim=c(500,2000)) +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
          , p1 + coord_cartesian(xlim= c(1959.5,2021),ylim=c(90,500)),common.legend = T, legend = "right", ncol = 1)
