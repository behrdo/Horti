library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(lme4)

maize <- read_excel("Maize dataset.xlsx")

#statistics
#testing if the treatments have a sig. effect on the Plantheight
height <- maize[!duplicated(maize$Pflanzennummer), ]

h1 <- lm(Pflanzenhoehe ~ Treatment, data = height)
summary(h1)

h0 <- lm(Pflanzenhoehe ~ 1, data = height)
summary(h0)

par(mfrow = c(2, 2))
plot(h1)
plot(h0)
par(mfrow = c(1, 1))

anova(h1, h0) #h1 seems to be the better model
#p-value if we expect this to be nd: 0.1628 -> Treatments have an effect on plant height
#which however is not significant


#testing if the treatments/leave age have a sig. effect on the chl_labor content
lab <- maize[!duplicated(maize$Chl_Labor), ]

lab$Treatment[lab$Treatment == "Nährstoffmangel"] <- "mngl"
lab$Treatment[lab$Treatment == "Trockenstress"] <- "trck"
lab$Treatment[lab$Treatment == "Kontrolle"] <- "kont" #names were to long for the lmer function

h2 <- lmer(Chl_Labor ~ Treatment + (1 | Blattalter), data = lab)
summary(h2)
#the variance dependent on the leave age is much higher than the residual variance, the
#variance dependent on the leave age appears to be significantly different from zero

confint(h2, parm = "sd_(Intercept)|Blattalter", level=0.95, 
        method = "profile", oldNames = F)
#Conclusion: The confidence interval for the standard deviation does not include zero; 
#-> reject the null hypothesis that the variance is zero.
#a significant effect of leave age on the chlorophyll content can be shown 

h1 <- lm(Chl_Labor ~ Treatment, data = lab)
summary(h1)

h0 <- lm(Chl_Labor ~ 1, data = lab)
summary(h0)

par(mfrow = c(2, 2))
plot(h1)
plot(h0)
par(mfrow = c(1, 1))

anova(h1, h0) #h1 seems to be the better model
#p-value if we expect this to be nd: 0.09161 -> Treatments have an effect on the 
#chlorophyll content, which however is not significant

#interaction plot
ggplot(maize, aes(x = Treatment, y = Chl_Labor, col = Blattalter)) + 
  geom_smooth(aes(group = Blattalter)) + 
  geom_jitter(width = 0.05, height = 0) + 
  ggtitle("Effect of Leave-Age and Treatments on the Chlorophyll-Content") + 
  ylab("Chlorophyll content") +
  xlab("Treatment") +
  theme_bw()

#this shows that we have a strong leave age effect, a small treatment effect and a small
#interaction between those two effects 








