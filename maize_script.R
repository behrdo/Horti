library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(plot3D)

maize <- read_excel("Maize dataset.xlsx")

maize[4] <- NULL

#height
q <- q <- ggplot(maize, aes(x = Treatment, y = Pflanzenhoehe, fill = Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("A") +
  ylab("Plant height [cm???]") +
  geom_jitter(position = position_jitter(0.2), show.legend = FALSE) +
  theme_bw() 
q

#leafe density
w <- ggplot(maize, aes(x = Treatment, y = Blattdichte, fill = Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("B") +
  ylab("Leafe density [scale???]") +
  geom_jitter(position = position_jitter(0.2), show.legend = FALSE) +
  theme_bw() 
w

#dry matter (only for plant numbers 10<)
e <- ggbarplot(maize, x = "Treatment", y = "FM_Rest_Ernteblatt", 
          add = c("mean_se", "jitter"),
          color = "Treatment", fill = "Treatment", 
          position = position_dodge(0.8))
e

#checking for normald
spad.model <- lm(FM_Rest_Ernteblatt ~ SPAD, data = maize)
lab.model <- lm(FM_Rest_Ernteblatt ~ Chl_Labor, data = maize)
chl.model <- lm(Chl_Labor ~ SPAD, data = maize)

par(mfrow = c(2, 2))
plot(spad.model)
plot(lab.model)
plot(chl.model)
par(mfrow = c(1, 1))
#none of the data seems to be normal distributed so spearman coef. seems to be the prefered choice
cor.test(maize$SPAD, maize$Chl_Labor, method = "spearman")

ggscatter(maize, x = "SPAD", y = "Chl_Labor", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SPAD", ylab = "Chl_labor")


ggscatter(maize, x = "SPAD", y = "Chl_Labor", 
          add = "reg.line",                        
          conf.int = TRUE, cor.method = "spearman", cor.coef = TRUE,                       
          color = "Treatment", palette = "jco",           
          shape = "Treatment") 

#changing the size of the symbols according to leaf thickness
ggscatter(maize, x = "SPAD", y = "Chl_Labor", 
          add = "reg.line",                        
          conf.int = TRUE, cor.method = "spearman", cor.coef = TRUE,                       
          color = "Treatment", palette = "jco",           
          shape = "Treatment", size = "Blattdicke_Ernteblatt") 

scatter3D(maize$SPAD, maize$Chl_Labor, maize$Blattdicke_Ernteblatt,
          phi = 0, 
          xlab = "SPAD", ylab ="Chl_Labor", zlab = "Blattdicke_Ernteblatt", 
          ticktype = "detailed")



SPAD
Chl_Labor

