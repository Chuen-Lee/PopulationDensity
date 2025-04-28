# Population density plot for Cousin - Chuen 28/04/2025

PopDenCN <- read_tsv("PopDenCN.tsv") # update PopDenCN.tsv and make sure path is correct

PopDenCNplot <- ggplot(PopDenCN, aes(PEnd,IndependentBirds)) + # x = End of FP, y=Independent birds
  geom_point(aes(colour=Pcolor)) + # colour by the most recent season
  geom_line()  + 
  xlab("Period End Date") + 
  ylab("Number of Independent Birds Observed") + 
  ylim(21,450) + 
  scale_x_date(limits=c(as.Date( "1997-01-01", "%Y-%m-%d"), 
                        as.Date("2025-04-01", "%Y-%m-%d")), 
               date_breaks = "5 years", date_labels = "%Y") + # Limit the x-axis and split by 5 year period - make sure to change the numbers
  theme_tufte(base_size = 15, base_family = "Arial") + 
  theme(axis.line = element_line(colour = "black", linetype=1)) +
  scale_color_manual(values = c("black","red")) +
  theme(legend.title=element_blank())

print(PopDenCNplot)
