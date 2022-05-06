library(dplyr)
library(viridis)
library(ggplot2)

LU <- read.csv("LU_ourworldindata.csv")
LU <- subset(LU, Entity=="World")
LU <- subset(LU, Year>=-2000)
LU$Built.up.Area..HYDE..2017.. <- LU$Built.up.Area..HYDE..2017../1e9
LU$Grazing..HYDE..2017.. <- LU$Grazing..HYDE..2017../1e9
LU$Cropland..HYDE..2017.. <- LU$Cropland..HYDE..2017../1e9

Lu <- LU[,-c(1,2)]
LU_melt <- reshape2::melt(Lu, id="Year")

LU_melt$variable <- factor(LU_melt$variable,
                           labels = c("Built-up area", "Grazing area", "Cropland"))

GGPoptions <- theme_classic() + theme(
  panel.border = element_rect(colour = "black", fill=NA),
  text = element_text(size=12, family="serif"), 
  axis.text.x = element_text(color="black", margin=ggplot2::margin(10,0,2,0,"pt"), size=12), 
  axis.text.y = element_text(color="black", margin=ggplot2::margin(0,10,0,0,"pt"), size=12),
  axis.ticks.length=unit(-0.1, "cm"),
  legend.text=element_text(size=12))

ggplot(LU_melt[LU_melt$variable!="Built-up area",],
       aes(x = Year, y=value, group=variable, col=variable, fill=variable)) +                         
  geom_line() +
  geom_area() + GGPoptions +
  scale_fill_viridis_d(begin = .5) +
  scale_colour_viridis_d(begin = .5) +
  ylab("Hectares (billion)") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "top") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / 13.003 * 100,
                        name = "% Total land surface")) +
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
  theme(axis.text.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8)))
#geom_vline(xintercept=1700, lty="dashed", col="grey") #+
  #geom_hline(yintercept=13.003, lty="dashed", col="grey") +
  

# 13,003 million ha. 4



