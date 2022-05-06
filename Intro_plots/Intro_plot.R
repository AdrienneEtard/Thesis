library(ggplot2)
library(dplyr)
library(lubridate)
library(viridis)
library(ggpubr)

#####################################################################################################
## plot for temperature anomalies

Anom <- read.csv("Annual_Temp_Anomalies_data.csv")
Anom <- Anom[c(5:146),]
colnames(Anom) <- c("Year", "TempAnom")
Anom$Year <- as.Date(Anom$Year, format = "%Y")

Anom$TempAnom <- as.numeric(Anom$TempAnom)
Anom$Col <- ifelse(Anom$TempAnom<0, "blue", "red")

GGPoptions <-   theme_bw() +theme(
  panel.border = element_rect(colour = "black", fill=NA),
  text = element_text(size=15, family="serif"),
  axis.text.x = element_text(color="black", margin=ggplot2::margin(10,0,2,0,"pt"), size=15),
  axis.text.y = element_text(color="black", margin=ggplot2::margin(0,10,0,0,"pt"), size=15),
  axis.ticks.length=unit(-0.1, "cm"),
  legend.text=element_text(size=15))

p_Temp <- 
ggplot(Anom, aes(x=Year, y=TempAnom, fill=Col)) +
  #geom_histogram(stat = "identity", binwidth = 15)+
  geom_line() +
  geom_area(alpha=0.5) + 
  geom_hline(yintercept=0, size=1) +
  theme(axis.text.x=element_text(angle = 90)) +
  #scale_x_continuous(limits=c(1880,2022), origin=1880)+
  scale_fill_manual(values=c("blue", "red")) +
  ylab("Land-surface temperature anomaly (°C)") +
  xlab("Year") +
  geom_hline(yintercept=0, col="black", linetype="solid") +
  GGPoptions +
  theme(legend.position = "none") +
  ggtitle("(b) Annual temperature anomalies") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . /1,
                        name = "", labels = NULL))+
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
  theme(axis.text.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8))) +
  theme(axis.title.y.left = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.text.y.left = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0))) 
p_Temp

#####################################################################################################
## plot for land use
LU <- read.csv("LU_ourworldindata.csv")
LU <- subset(LU, Entity=="World")
LU <- subset(LU, Year>=-2000)
LU$Built.up.Area..HYDE..2017.. <- LU$Built.up.Area..HYDE..2017../1e9
LU$Grazing..HYDE..2017.. <- LU$Grazing..HYDE..2017../1e9
LU$Cropland..HYDE..2017.. <- LU$Cropland..HYDE..2017../1e9

# subset between year 0 and 2016
LU <- subset(LU, Year>=0)
Lu <- LU[,-c(1,2)]

# subset for agricultural land uses
Lu <- Lu[, c(1,3,4)]
LU_melt <- reshape2::melt(Lu, id="Year")
LU_melt$variable <- factor(LU_melt$variable,
                           labels = c("Grazing area", "Cropland"))

LU_melt$variable %>% unique()

p_LU <- 
ggplot(LU_melt,
       aes(x = Year, y=value, group=variable, fill=variable)) +  
  #geom_line() +
  geom_area(alpha=.5, col="black") + 
  scale_fill_manual(values=c("#009E73", "#D55E00")) +
  GGPoptions +
  ylab("Hectares (billion)") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / 13.003 * 100,
                        name = "% Total land surface")) +
  theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 15))) +
  theme(axis.text.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8))) +
  theme(axis.title.y.left = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.text.y.left = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0))) +
  ggtitle("(a) Land-surface dedicated to agriculture")+
  theme(legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill = "white", color = "black"))

p_LU

#####################################################################################################
## arranging plots

PlotIntro <- 
ggarrange(p_LU, p_Temp, nrow=2, widths = c(1, 0.9))
ggsave("Intro_plot.pdf", height = 8, width = 8)


