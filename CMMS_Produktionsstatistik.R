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

# Reading in raw data file from Starlims Analytics where I searched "TAT_"-flik then "Filtrera"-flik and then
#' retrieve the "Order datum"-data.
#' for our "kärnanalyser" sorted by "Aktivitetsnamn" in the "Filtrera"-flik from 2015-2020. Hämtas från SQL.
#' 
#' "Kärnanalyser consists of the following "Aktivitetsnamn":
#' "M-Mitokondriell ATP-produktion"
#' "P-Acylkarnitiner ERNDIM"
#' "P-Acylkarnitiner"
#' "P-Aminosyror (¤)"
#' "P-Aminosyror ERNDIM (¤)"
#' "P-Aminosyror, kvantitativt ERNDIM (¤)"
#' "U-Kreatinin"
#' "U-Organiska syror behandlingskontroll"
#' "U-Organiska syror ERNDIM"
#' "U-Organiska syror screening"
#' "U-Porfyrinprekursorer ALA/PBG (¤)"

#' Data för alla biokemiska analyser
Biokemi_201014 <- read_delim("Data/Biokemi_201014.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)

#' Data för hur många remisser som ankommer PMT varje dag.
SQL_PMT_day <- read_delim("Data/SQL_PMT_day.csv", 
                          ";", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)
# Rename för att lägga till relevanta kolumnnamn.
SQL_PMT_day <- SQL_PMT_day %>% 
  rename("Antal" = "X1", "RegDatum" = "X2")

#' Data för hur många remisser som ankommer PMT varje dag.
SQL_PMT_tot <- read_delim("Data/SQL_PMT_tot.csv", 
                          ";", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)
# Rename för att lägga till relevanta kolumnnamn.
SQL_PMT_tot <- SQL_PMT_tot %>% 
  rename("RegDatum" = "X1")

# Create column month to hold labelled months
SQL_PMT_tot$ym <- as.yearmon(SQL_PMT_tot$RegDatum, label = TRUE)

SQL_PMT_tot <- SQL_PMT_tot %>% 
  group_by(ym) %>% 
  summarize(n = n())

a <- "Medelvärde = "
b <- round(mean(SQL_PMT_day$Antal), 0)
c <- "remisser/dag"
d <- round(mean(SQL_PMT_tot$n), 0)
e <- "remisser/månad"
f <- "Data t.o.m 2020-10-30,"
g <- " Data/SQL_PMT_day.csv"
h <- "2020-10-30"

# Create column month to hold labelled months
Biokemi <- Biokemi_201105
  Biokemi$ym <- as.yearmon(Biokemi$Godkännandedatum, label = TRUE)
  
  Biokemi$år <- year(Biokemi$Godkännandedatum)
  
  Biokemi$månad <- month(Biokemi$Godkännandedatum)

data_ref2 <- paste(f, g)

prod <- read_delim("Data/SQL_201030.csv", 
           ";", escape_double = FALSE, col_names = FALSE, 
           trim_ws = TRUE)

# Datareferense som läggs till i caption på graferna
data_ref <- "Data t.o.m 2020-10-30, Data/SQL200930.csv"
                         
prod <- prod %>% 
  rename("ID" = "X1", "Aktivitetsnamn" = "X2", "Prov_taget" = "X3", "Prov_ank" = "X4", "Prov_reg" = "X5", "Körlista_påbörjad" = "X6", "Rapp_distr" = "X7", "Patpostnr" = "X8")

#' Change character columns into date columns and measured values into numeric
#' Namnen till vänster i listan behöver ändras och överensstämma med
#' de faktiska namnen i datafilen som fås direkt från SQL.
#
prod <- prod %>%
  transmute(ID = as.character(prod$ID),
          Aktivitetsnamn = as.character(prod$Aktivitetsnamn), 
          Körlista_påbörjad = as.Date(prod$Körlista_påbörjad),
          Prov_ank = as.Date(prod$Prov_ank),
          Prov_taget = as.Date(prod$Prov_taget),
          Prov_reg = as.Date(prod$Prov_reg),
          Rapp_distr = as.Date(prod$Rapp_distr),
          Patpostnr = as.numeric(prod$Patpostnr))

#' Changes names on variables in Aktivitetsnamn column so that for example all organic
#' acids gets the same name etc.
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "P-Acylkarnitiner ERNDIM"] <- "P-AC"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "P-Acylkarnitiner"] <- "P-AC"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "U-Organiska syror behandlingskontroll"] <- "OS"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "U-Organiska syror ERNDIM"] <- "OS"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "U-Organiska syror screening"] <- "OS"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "P-Aminosyror (¤)"] <- "AA"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "P-Aminosyror ERNDIM (¤)"] <- "AA"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "P-Aminosyror, kvantitativt ERNDIM (¤)"] <- "AA"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "Skicka prover till SciLife (WGS)"] <- "WGS"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "Skicka prover till SciLife låg konc (WGS)"] <- "WGS"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "U-Porfyrinprekursorer ALA/PBG (¤)"] <- "5-ALA/PBG"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "M-Mitokondriell ATP-produktion"] <- "ATPprod"
prod$Aktivitetsnamn[prod$Aktivitetsnamn == "U-Kreatinin"] <- "Krea"

# Rapport distribuerad!
# Här kan tidsperioden ändras för de kommande graferna!
res_distr <- prod %>%
  select(Aktivitetsnamn, Rapp_distr) %>% 
  na.omit() %>% 
  filter(Rapp_distr >= "2015-01-01", Rapp_distr <= "2020-10-30")

# Use month() to tabulate sample arrival per month
month(res_distr$Rapp_distr) %>% table()

# Add label = TRUE to make table more readable
month(res_distr$Rapp_distr, label = TRUE) %>% table()

# Create column month to hold labelled months
res_distr$månad <- month(res_distr$Rapp_distr, label = TRUE)

res_distr$ym <- as.yearmon(res_distr$Rapp_distr) 

# ggplot theme
my_theme <- theme(plot.title = element_text(hjust = 0.5),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
      plot.subtitle = element_text(hjust = 0.5),
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 90))

# Timeseries plot för PMT remisser/dag.
ggplot(SQL_PMT_day, aes(x = RegDatum, y = Antal)) +
  geom_point(aes(x = RegDatum , y = Antal), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="Antal remisser/dag 2015-2020 (ankomstdatum)",
       subtitle = paste(a, b, c),
       x = "Dag",
       y = "Antal",
       caption = data_ref2) +
  my_theme

# Timeseries plot för PMT remisser/månad.
  ggplot(SQL_PMT_tot, aes(x = ym, y = n)) +
    geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
    geom_line() +
    geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
    labs(title ="Antal remisser/månad 2015-2020 (ankomstdatum)",
        subtitle = paste(a, d, e),
        x = "Dag",
        y = "Antal",
        caption = data_ref2) +
    my_theme

# Här nedan följer graferna för våra kärnanalyser totalt samt var och en för sig.
res_distr %>%
  group_by(ym) %>% 
  summarize(n = n()) %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="Kärnanalyser månadsvy 2015-2020 (utsvarade)",
       subtitle = "AA, OS, ALA/PBG, P-AC, ATPprod & Krea",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  my_theme

res_distr %>%
  filter(Aktivitetsnamn == "5-ALA/PBG") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="5-ALA/PBG månadsvy 2015-2020 (utsvarade)",
       subtitle = "TAT(best.-distr.) = 6 dagar",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  my_theme

# funkar inte än...
# aapngfile <- download.file("H:/R/Projects/CMMS-Statistics", destfile = "AA.png", mode = "wb")
# library(png)
# aapng <- readPNG("H:/R/Projects/CMMS-Statistics/AA.png")

res_distr %>%
  filter(Aktivitetsnamn == "AA") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="Aminosyror månadsvy 2015-2020 (utsvarade)",
       subtitle = "TAT(best.-distr.) = 9 dagar",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  my_theme

res_distr %>%
  filter(Aktivitetsnamn == "OS") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="OS månadsvy 2015-2020 (utsvarade)",
       subtitle = "TAT(best.-distr.) = 9 dagar",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  my_theme

res_distr %>%
  filter(Aktivitetsnamn == "P-AC") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="Plasma Acylkarnitiner månadsvy 2015-2020 (utsvarade)",
       subtitle = "TAT(best.-distr.) = 9 dagar",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  my_theme

res_distr %>%
  filter(Aktivitetsnamn == "ATPprod") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="ATPproduktion månadsvy 2015-2020 (utsvarade)",
       subtitle = "TAT(best.-distr.) = 47 dagar",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  my_theme

res_distr %>%
  filter(Aktivitetsnamn == "Krea") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="Kreatinin månadsvy 2015-2020 (utsvarade)",
       subtitle = "TAT(best.-distr.) = 9 dagar",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  my_theme
# Uppgift: Uppdatera med kurva för TAT från "registrerat" till utsvarat resultat. Använd
# difftime(df$x, df$y, unit = c("days"))

Biokemi_tot %>%
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

Biokemi$Godkännandedatum <- as.Date(Biokemi$Godkännandedatum)

Biokemi %>%
  filter(TESTNO == "P-Acylkarnitiner", Godkännandedatum <= "2020-10-30") %>% 
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
