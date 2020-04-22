library(tidyverse)
library(readxl)

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
w <- ggplot(data = remove_missing(maize, na.rm = TRUE, vars = TM_Ganze_Pflanze), 
            aes(x = Treatment, y = TM_Ganze_Pflanze, fill = Treatment)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("C") +
  ylab("Total dry matter") +
  geom_jitter(position = position_jitter(0.2), show.legend = FALSE) +
  theme_bw() 
w









