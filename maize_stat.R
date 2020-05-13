library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(lme4)

maize <- read_excel("Maize dataset.xlsx")

#statistics
#testing if the treatments have a sig. effect on the Plantheight
h1 <- lm(Pflanzenhoehe ~ Treatment, data = maize)
summary(h1)

h0 <- lm(Pflanzenhoehe ~ 1, data = maize)
summary(h0)

par(mfrow = c(2, 2))
plot(h1)
plot(h0)
par(mfrow = c(1, 1))

shapiro.test(maize$Pflanzenhoehe)

anova(h1, h0) #h1 is definitely the better model, but seems to be not nd
#p-value if we use lm and expect this to be nd: 0.0003835 -> Treatments have an effect

#using glm 
h1 <- glm(Pflanzenhoehe ~ Treatment, data = maize)
summary(h1)
#Nährstoffmangel seems to have a significant effect, trockenstress not??? (not sure about this, 
#never used a glm)

#testing if the treatments have a sig. effect on the chl_lab content
h1 <- lm(Chl_Labor ~ Treatment, data = maize)
summary(h1)

h0 <- lm(Chl_Labor ~ 1, data = maize)
summary(h0)

par(mfrow = c(2, 2))
plot(h1)
plot(h0)
par(mfrow = c(1, 1))

shapiro.test(maize$Chl_Labor)

anova(h1, h0)
#The treatments seem to have a sign effect on the chl lab content and nd seems at least more likely
#p-value: 0.007401














