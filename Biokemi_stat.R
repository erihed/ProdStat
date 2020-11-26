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

SQL_BIO_TOT <- read_delim("Data/SQL_BIO_TOT.csv", 
                          ";", escape_double = FALSE,
                          trim_ws = TRUE)

SQL_BIO_TOT$Godkännandedatum <- as.Date(as.character(SQL_BIO_TOT$Godkännandedatum), format = "%Y-%m-%d")
SQL_BIO_TOT$TESTNO <- as.numeric(as.double(SQL_BIO_TOT$TESTNO))


SQL_BIO_TOT$år <- year(SQL_BIO_TOT$Godkännandedatum) %>% 
  summarize(TESTNO)
