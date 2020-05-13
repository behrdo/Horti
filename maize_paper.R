library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(lme4)

maize <- read_excel("Maize dataset.xlsx")

#1. checking for normal distribution
#1.1 without treatment differentiation
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

#plotting the correlation matrix (whole dataset)
#for now i used "DX_Chl_MW", "SFR_R" and "ChlNDI"

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
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  ggtitle("Correlation Matrix") +
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

#SPAD
a <- ggscatter(maize, x = "SPAD", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "spearman", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title = "SPAD",         
               shape = "Treatment")

e <- ggplot(maize, aes(x = SPAD, y = Chl_Labor, color = Treatment, shape = Treatment)) +
  geom_point() + 
  geom_smooth(method = lm) +
  ggtitle("SPAD") +
  theme_bw()

#dualex
b <- ggscatter(maize, x = "DX_Chl_MW", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "spearman", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title = "Dualex",           
               shape = "Treatment")

f <- ggplot(maize, aes(x = Chl, y = Chl_Labor, color = Treatment, shape = Treatment)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  ggtitle("Dualex") +
  theme_bw()

#multiplex
c <- ggscatter(maize, x = "SFR_R", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "spearman", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title = "Multiplex",          
               shape = "Treatment")

g <- ggplot(maize, aes(x = SFR_R, y = Chl_Labor, color = Treatment, shape = Treatment)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  ggtitle("Multiplex") +
  theme_bw()

#FieldSpec
d <- ggscatter(maize, x = "ChlNDI", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "spearman", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title = "Fieldspec",           
               shape = "Treatment")

h <- ggplot(maize, aes(x = ChlNDI, y = Chl_Labor, color = Treatment, shape = Treatment)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  ggtitle("Fieldspec") +
  theme_bw()

ggarrange(a, b, c, d)
###ggarrange(e, f, g, h)###
#not sure how to change the correlation coefficient in ggplot

#phenological plots
height <- maize[!duplicated(maize$Pflanzennummer), ]

q <- ggplot(height, aes(x = Treatment, y = Pflanzenhoehe, fill = Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("A") +
  ylab("Plant height [cm]") +
  geom_jitter(position = position_jitter(0.2), show.legend = FALSE) +
  theme_bw() 
q


#leafe density
dens <- maize[!duplicated(maize$Blattdichte), ]

w <- ggplot(dens, aes(x = Treatment, y = Blattdichte, fill = Blattalter)) +
  geom_boxplot(show.legend = TRUE) +
  ggtitle("B") +
  ylab("Leafe density [g/m^3 ?]") +
  geom_jitter(position = position_jitter(0.2), show.legend = FALSE) +
  theme_bw() 
w

w <- ggplot(dens, aes(x = Treatment, y = Blattdichte, fill = Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("B") +
  ylab("Leafe density [g/m^3 ?]") +
  geom_jitter(position = position_jitter(0.2), show.legend = FALSE) +
  theme_bw() 
w

#dry matter
tm <- slice(height, 10:30)
tm <- transform(tm, TM_Ganze_Pflanze = as.numeric(TM_Ganze_Pflanze))

e <- ggplot(tm, aes(x = Treatment, y = TM_Ganze_Pflanze, fill = Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("C") +
  ylab("Dry Matter Content") +
  geom_jitter(position = position_jitter(0.2), show.legend = FALSE) +
  theme_bw() 
e

ggarrange(q, w, e)


#2.lm und glm nochmal anschauen (evtl assignment?)
#3.statistic teil schreiben




























########
#1.2 with treatment differentiation
mangel <- filter(maize, Treatment == "Nährstoffmangel")
trocken <- filter(maize, Treatment == "Trockenstress")
kontr <- filter(maize, Treatment == "Kontrolle")

#mangel
spad.model <- lm(FM_Rest_Ernteblatt ~ SPAD, data = mangel)
lab.model <- lm(FM_Rest_Ernteblatt ~ Chl_Labor, data = mangel)
dualex.model <- lm(FM_Rest_Ernteblatt ~ Chl, data = mangel)
multi.model1 <- lm(FM_Rest_Ernteblatt ~ SFR_R, data = mangel)
multi.model2 <- lm(FM_Rest_Ernteblatt ~ SFR_G, data = mangel)
field.model1 <- lm(FM_Rest_Ernteblatt ~ NDWI, data = mangel)
field.model2 <- lm(FM_Rest_Ernteblatt ~ ChlNDI, data = mangel)

par(mfrow = c(2, 2))
plot(spad.model)
plot(lab.model)
plot(dualex.model)
plot(multi.model1)
plot(multi.model2)
plot(field.model1)
plot(field.model2)
par(mfrow = c(1, 1))

shapiro.test(mangel$FM_Rest_Ernteblatt)
shapiro.test(mangel$Pflanzenhoehe)
shapiro.test(mangel$SPAD)
shapiro.test(mangel$Chl_Labor)
shapiro.test(mangel$Chl)
shapiro.test(mangel$SFR_R)
shapiro.test(mangel$SFR_G)
shapiro.test(mangel$NDWI)
shapiro.test(mangel$ChlNDI)

#trocken
spad.model <- lm(FM_Rest_Ernteblatt ~ SPAD, data = trocken)
lab.model <- lm(FM_Rest_Ernteblatt ~ Chl_Labor, data = trocken)
dualex.model <- lm(FM_Rest_Ernteblatt ~ Chl, data = trocken)
multi.model1 <- lm(FM_Rest_Ernteblatt ~ SFR_R, data = trocken)
multi.model2 <- lm(FM_Rest_Ernteblatt ~ SFR_G, data = trocken)
field.model1 <- lm(FM_Rest_Ernteblatt ~ NDWI, data = trocken)
field.model2 <- lm(FM_Rest_Ernteblatt ~ ChlNDI, data = trocken)

par(mfrow = c(2, 2))
plot(spad.model)
plot(lab.model)
plot(dualex.model)
plot(multi.model1)
plot(multi.model2)
plot(field.model1)
plot(field.model2)
par(mfrow = c(1, 1))

shapiro.test(trocken$FM_Rest_Ernteblatt)
shapiro.test(trocken$Pflanzenhoehe)
shapiro.test(trocken$SPAD)
shapiro.test(trocken$Chl_Labor)
shapiro.test(trocken$Chl)
shapiro.test(trocken$SFR_R)
shapiro.test(trocken$SFR_G)
shapiro.test(trocken$NDWI)
shapiro.test(trocken$ChlNDI)

#kontrolle
spad.model <- lm(FM_Rest_Ernteblatt ~ SPAD, data = kontr)
lab.model <- lm(FM_Rest_Ernteblatt ~ Chl_Labor, data = kontr)
dualex.model <- lm(FM_Rest_Ernteblatt ~ Chl, data = kontr)
multi.model1 <- lm(FM_Rest_Ernteblatt ~ SFR_R, data = kontr)
multi.model2 <- lm(FM_Rest_Ernteblatt ~ SFR_G, data = kontr)
field.model1 <- lm(FM_Rest_Ernteblatt ~ NDWI, data = kontr)
field.model2 <- lm(FM_Rest_Ernteblatt ~ ChlNDI, data = kontr)

par(mfrow = c(2, 2))
plot(spad.model)
plot(lab.model)
plot(dualex.model)
plot(multi.model1)
plot(multi.model2)
plot(field.model1)
plot(field.model2)
par(mfrow = c(1, 1))

shapiro.test(kontr$FM_Rest_Ernteblatt)
shapiro.test(kontr$Pflanzenhoehe)
shapiro.test(kontr$SPAD)
shapiro.test(kontr$Chl_Labor)
shapiro.test(kontr$Chl)
shapiro.test(kontr$SFR_R)
shapiro.test(kontr$SFR_G)
shapiro.test(kontr$NDWI)
shapiro.test(kontr$ChlNDI)

#2.2 with treatment differentiation 
#mangel
cor.df <- select(mangel, Chl_Labor, SPAD, Chl, SFR_R, ChlNDI)

cormat <- round(cor(cor.df),2)

lt <- get_upper_tri(cormat)
lt

lt <- melt(lt, na.rm = TRUE)

ggheatmap <- ggplot(lt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  ggtitle("Mangel") +
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
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#spad
a <- ggscatter(mangel, x = "SPAD", y = "Chl_Labor", 
          add = "reg.line",                        
          conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
          color = "Treatment", palette = "jco", title = "SPAD",          
          shape = "Treatment")

#dualex
b <- ggscatter(mangel, x = "Chl", y = "Chl_Labor", 
          add = "reg.line",                        
          conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
          color = "Treatment", palette = "jco", title =  "Dualex",          
          shape = "Treatment")

#multiplex
c <- ggscatter(mangel, x = "SFR_R", y = "Chl_Labor", 
          add = "reg.line",                        
          conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
          color = "Treatment", palette = "jco", title = "Multiplex",          
          shape = "Treatment")

#fieldspec
d <- ggscatter(mangel, x = "ChlNDI", y = "Chl_Labor", 
          add = "reg.line",                        
          conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
          color = "Treatment", palette = "jco", title = "Fieldspec",            
          shape = "Treatment")

ggarrange(a, b, c, d)

#trocken
cor.df <- select(trocken, Chl_Labor, SPAD, Chl, SFR_R, ChlNDI)

cormat <- round(cor(cor.df),2)

lt <- get_upper_tri(cormat)
lt

lt <- melt(lt, na.rm = TRUE)

ggheatmap <- ggplot(lt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  ggtitle("Trockenstress") +
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
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#spad
a <- ggscatter(trocken, x = "SPAD", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title = "SPAD",          
               shape = "Treatment")

#dualex
b <- ggscatter(trocken, x = "Chl", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title =  "Dualex",          
               shape = "Treatment")

#multiplex
c <- ggscatter(trocken, x = "SFR_R", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title = "Multiplex",          
               shape = "Treatment")

#fieldspec
d <- ggscatter(trocken, x = "ChlNDI", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title = "Fieldspec",            
               shape = "Treatment")

ggarrange(a, b, c, d)

#kontrolle
cor.df <- select(kontr, Chl_Labor, SPAD, Chl, SFR_R, ChlNDI)

cormat <- round(cor(cor.df),2)

lt <- get_upper_tri(cormat)
lt

lt <- melt(lt, na.rm = TRUE)

ggheatmap <- ggplot(lt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  ggtitle("Kontrolle") +
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
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#spad
a <- ggscatter(kontr, x = "SPAD", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title = "SPAD",          
               shape = "Treatment")

#dualex
b <- ggscatter(kontr, x = "Chl", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title =  "Dualex",          
               shape = "Treatment")

#multiplex
c <- ggscatter(kontr, x = "SFR_R", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title = "Multiplex",          
               shape = "Treatment")

#fieldspec
d <- ggscatter(kontr, x = "ChlNDI", y = "Chl_Labor", 
               add = "reg.line",                        
               conf.int = TRUE, cor.method = "pearson", cor.coef = TRUE,                       
               color = "Treatment", palette = "jco", title = "Fieldspec",            
               shape = "Treatment")

ggarrange(a, b, c, d)











