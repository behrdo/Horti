library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(lme4)

maize <- read_excel("Maize dataset.xlsx")

#checking for normal distribution to figure out which cor.coef to use
spad.model <- lm(FM_Rest_Ernteblatt ~ SPAD*Blattalter, data = maize)
lab.model <- lm(FM_Rest_Ernteblatt ~ Chl_Labor, data = maize)
dualex.model <- lm(FM_Rest_Ernteblatt ~ Chl, data = maize)
multi.model1 <- lm(FM_Rest_Ernteblatt ~ SFR_R, data = maize)
multi.model2 <- lm(FM_Rest_Ernteblatt ~ SFR_G, data = maize)
field.model1 <- lm(FM_Rest_Ernteblatt ~ NDWI, data = maize)
field.model2 <- lm(FM_Rest_Ernteblatt ~ ChlNDI, data = maize)

par(mfrow = c(2, 2))
plot(spad.model)
plot(lab.model)
plot(dualex.model)
plot(multi.model1)
plot(multi.model2)
plot(field.model1)
plot(field.model2)
par(mfrow = c(1, 1))

shapiro.test(maize$FM_Rest_Ernteblatt)
shapiro.test(maize$Pflanzenhoehe)
shapiro.test(maize$SPAD)
shapiro.test(maize$Chl_Labor)
shapiro.test(maize$Chl)
shapiro.test(maize$SFR_R)
shapiro.test(maize$SFR_G)
shapiro.test(maize$NDWI)
shapiro.test(maize$ChlNDI)

#plotting the correlation matrix
###
#chl indices that were used previously: SFR_R, NDopt, mND, mSR, NDVI
#names(cor.df)[names(cor.df) == "NDopt"] <- "FS_NDopt"
#names(cor.df)[names(cor.df) == "mND"] <- "FS_mND"
#names(cor.df)[names(cor.df) == "mSR"] <- "FS_mSR"
#names(cor.df)[names(cor.df) == "NDVI"] <- "FS_NDVI"
###

cor.df <- select(maize, Chl_Labor, SPAD, DX_Chl_MW, SFR_R, ChlNDI)

names(cor.df)[names(cor.df) == "Chl_Labor"] <- "Laboratory"
names(cor.df)[names(cor.df) == "DX_Chl_MW"] <- "Dualex"
names(cor.df)[names(cor.df) == "SFR_R"] <- "Multiplex"
names(cor.df)[names(cor.df) == "ChlNDI"] <- "FieldSpec"

cormat <- round(cor(cor.df, method = "spearman"),2)

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

lt <- get_lower_tri(cormat)
lt

lt <- melt(lt)

ggplot(lt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    plot.title = element_text(hjust = 0.5),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#correlation scatterplots with leave age differentiation
maize$Blattalter[maize$Blattalter == "jung"] <- "Young"
maize$Blattalter[maize$Blattalter == "alt"] <- "Old"
maize$Treatment[maize$Treatment == "Nährstoffmangel"] <- "Nutrient deficiency"
maize$Treatment[maize$Treatment == "Trockenstress"] <- "Drought"
maize$Treatment[maize$Treatment == "Kontrolle"] <- "Control"

e <- ggplot(maize, aes(x = SPAD, y = Chl_Labor, color = Treatment, shape = Blattalter)) +
  geom_point() + 
  geom_smooth(method = lm) +
  labs(shape = "Leaf age", x = "SPAD value", 
       y = bquote("Chlorophyll content [nmol" ~ g^-1 ~ "]"),
         title = "A") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 50)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300)) +
  scale_color_manual(values = c("red3", "#0072B2", "#F0E442")) +
  theme_bw() +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10))

f <- ggplot(maize, aes(x = Chl, y = Chl_Labor, color = Treatment, shape = Blattalter)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(shape = "Leaf age", x = "Dualex value", 
       y = bquote("Chlorophyll content [nmol" ~ g^-1 ~ "]"),
       title = "B") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 42)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300)) +
  scale_color_manual(values = c("red3", "#0072B2", "#F0E442")) +
  theme_bw() +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10))

g <- ggplot(maize, aes(x = SFR_R, y = Chl_Labor, color = Treatment, shape = Blattalter)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(shape = "Leaf age", x = "Multiplex value", 
       y = bquote("Chlorophyll content [nmol" ~ g^-1 ~ "]"),
       title = "C") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300)) +
  scale_color_manual(values = c("red3", "#0072B2", "#F0E442")) +
  theme_bw() + 
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10))

h <- ggplot(maize, aes(x = ChlNDI, y = Chl_Labor, color = Treatment, shape = Blattalter)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(shape = "Leaf age", x = "Fieldspec value", 
       y = bquote("Chlorophyll content [nmol" ~ g^-1 ~ "]"),
       title = "D") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 0.61)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300)) +
  scale_color_manual(values = c("red3", "#0072B2", "#F0E442")) +
  theme_bw() + 
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10))

ggarrange(e, f, g, h, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")

#morphological plots
chl <- maize[!duplicated(maize$Blattdichte), ]

height <- maize[!duplicated(maize$Pflanzennummer), ]

tm <- slice(height, 10:30)
tm <- transform(tm, TM_Ganze_Pflanze = as.numeric(TM_Ganze_Pflanze))

#height
q <- ggplot(height, aes(x = Treatment, y = Pflanzenhoehe, fill = Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("A") +
  ylab("Plant height [cm]") +
  scale_fill_manual(values = c("red3", "#0072B2", "#F0E442")) +
  geom_jitter(position = position_jitter(0.2), show.legend = FALSE) +
  theme_bw() +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10))

#dry matter
w <- ggplot(tm, aes(x = Treatment, y = TM_Ganze_Pflanze, fill = Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("B") +
  ylab("Dry matter per plant [g]") +
  scale_fill_manual(values = c("red3", "#0072B2", "#F0E442")) +
  geom_jitter(position = position_jitter(0.2), show.legend = FALSE) +
  theme_bw() +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10))

#chlorophyll lab
r <- ggplot(chl, aes(x = Treatment, y = Chl_Labor, fill = Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("C") +
  ylab("Chlorophyll content [nmol" ~ g^-1 ~ "]") +
  scale_fill_manual(values = c("red3", "#0072B2", "#F0E442")) +
  geom_jitter(position = position_jitter(0.2), show.legend = FALSE) +
  theme_bw() +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 11), 
        plot.title = element_text(size = 15), strip.text.y = element_text(size = 10), 
        strip.text.x = element_text(size = 10))

ggarrange(q, w, r, ncol = 3, nrow = 1)

