library(tidyverse)
library(here)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(purrr)
library(extrafont)
library(zoo)
library(forcats)
library(RColorBrewer)
library(readr)

# Ändra datum och filreferenser här nedan.
f <- "Data t.o.m 2020-11-30,"
g <- "Data/SQL_BIO_TOT.csv"
h <- "2020-11-30"
data_ref2 <- paste(f, g)

#' Data för alla biokemiska analyser.
#' Använder mig av query: CMMS_prod_stat_BiokemiAnalyser som ligger
#' under H:\SQL Server Management Studio
#' 
Biokemi_tot <- read_delim("Data/SQL_BIO_TOT.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

# Create column month to hold labelled months
Biokemi <- Biokemi_tot
Biokemi$ym <- as.yearmon(Biokemi$Godkännandedatum, label = TRUE)

Biokemi$år <- year(Biokemi$Godkännandedatum)

Biokemi$månad <- month(Biokemi$Godkännandedatum)

# ggplot theme
my_theme <- theme(plot.title = element_text(hjust = 0.5),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  plot.subtitle = element_text(hjust = 0.5),
                  text = element_text(size = 12),
                  axis.text.x = element_text(angle = 90))

# Count skapar n som sedan används för att plotten! Använder variabeln h för att tidigt ändra datumrangen.
Biokemi %>%
  filter(Godkännandedatum <= h) %>%
  group_by(ym) %>% 
  count() %>% 
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="Biokemi 2015-2020 (godkända)",
       subtitle = "17 analyser",
       x = "Månad",
       y = "Antal",
       caption = data_ref2) +
  my_theme

# Ändrar formatet på columnen Godkännandedatum till Date.
Biokemi$Godkännandedatum <- as.Date(Biokemi$Godkännandedatum)

# Ändrar TESTNO för att plotta enskilda biokemianalyser.
# Vill skapa funktion här så att endast analyt behöver skrivas in!
Biokemi %>%
  filter(TESTNO == "P-Acylkarnitiner", Godkännandedatum <= h) %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="P-Acylkarnitiner 2015-2020 (godkända)",
       subtitle = "TAT(best.-distr.) = 9 dagar",
       x = "Månad",
       y = "Antal",
       caption = data_ref2) +
  my_theme

Biokemi %>%
  filter(TESTNO == "U-Organiska syror ERNDIM" | 
           TESTNO == "U-Organiska syror behandlingskontroll" |
         TESTNO == "U-Organiska syror screening", Godkännandedatum <= h) %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="Organiska syror 2015-2020 (godkända)",
       subtitle = "TAT(best.-distr.) = 9 dagar",
       x = "Månad",
       y = "Antal",
       caption = data_ref2) +
  my_theme

Biokemi %>%
  filter(TESTNO == "U-Oxalat Glycerat Glykolat" | 
           TESTNO == "U-Oxalat" |
           TESTNO == "U-Oxalat (Glycerat Glykolat) ERNDIM", Godkännandedatum <= h) %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="U-Oxalat 2015-2020 (godkända)",
       subtitle = "TAT(best.-distr.) = 24 dagar",
       x = "Månad",
       y = "Antal",
       caption = data_ref2) +
  my_theme

unique(Biokemi$TESTNO)
