library(ggplot2)
library(dplyr)
library(lubridate)

setwd("C:/Users/Adrienne/OneDrive - University College London/PhD/Writings/THESIS/Intro_plots")

Anom <- read.csv("Anomalies_data.csv")
Anom <- Anom[c(5:147),]
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
  legend.text=element_text(size=12))

ggplot(Anom, aes(x=Year, y=TempAnom, fill=Col,  alpha=0.8)) +
  geom_histogram(stat = "identity", binwidth = 15)+
  geom_hline(yintercept=0, size=1) +
  theme(axis.text.x=element_text(angle = 90)) +
  #scale_x_continuous(limits=c(1880,2022), origin=1880)+
  scale_fill_manual(values=c("blue", "red")) +
  ylab("Land-surface temperature anomaly (°C)") +
  xlab("Year") +
  geom_hline(yintercept=0, col="black", linetype="solid") +
  GGPoptions +
  theme(legend.position = "none")
