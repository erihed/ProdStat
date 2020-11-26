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

# Reading in raw data file from Starlims Analytics where I searched "TAT_"-flik then "Filtrera"-flik and then
#' retrieve the "Order datum"-data.
#' for our "kärnanalyser" sorted by "Aktivitetsnamn" in the "Filtrera"-flik from 2015-2020.
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



prod <- read_csv2("Data/Order_datum_ws_data.csv")

# "Data t.o.m 2020-09-29, Data/Order_datum_ws_data.csv"

#' Change character columns into date columns and measured values into numeric
#' 
prod <- prod %>% 
  transmute(Aktivitetsnamn = as.character(prod$Aktivitetsnamn), 
      Körlista_påbörjad = as.Date(prod$`Körlista påbörjad`),
       Paket_kontrollerat = as.Date(prod$`Paket  kontrollerat`),
       Paket_frisläppt = as.Date(prod$`Paket frisläppt`),
       Paket_klart = as.Date(prod$`Paket klart`),
       Paket_medbed = as.Date(prod$`Paket medicinskt bedömt`),
       Prov_ank = as.Date(prod$`Prov ankom`),
       Prov_taget = as.Date(prod$`Prov taget`),
       Rapp_distr = as.Date(prod$`Rapport distribuerad`),
       Rapp_gen = as.Date(prod$`Rapport genererad`),
       Rapp_korr= as.Date(prod$`Rapport korrekturläst`))

#' Changes names on variables in Aktivitetsnamn column so that all organic
#' acids gets the same name etc.
prod$Aktivitetsnamn[prod$Aktivitetsnamn 
                              == "P-Acylkarnitiner ERNDIM"] <- "P-AC"
prod$Aktivitetsnamn[prod$Aktivitetsnamn 
                              == "P-Acylkarnitiner"] <- "P-AC"
prod$Aktivitetsnamn[prod$Aktivitetsnamn 
                              == "U-Organiska syror behandlingskontroll"] <- "OS"
prod$Aktivitetsnamn[prod$Aktivitetsnamn 
                              == "U-Organiska syror ERNDIM"] <- "OS"
prod$Aktivitetsnamn[prod$Aktivitetsnamn 
                              == "U-Organiska syror screening"] <- "OS"
prod$Aktivitetsnamn[prod$Aktivitetsnamn 
                              == "P-Aminosyror (¤)"] <- "AA"
prod$Aktivitetsnamn[prod$Aktivitetsnamn 
                              == "P-Aminosyror ERNDIM (¤)"] <- "AA"
prod$Aktivitetsnamn[prod$Aktivitetsnamn 
                              == "P-Aminosyror, kvantitativt ERNDIM (¤)"] <- "AA"
prod$Aktivitetsnamn[prod$Aktivitetsnamn
                    == "Skicka prover till SciLife (WGS)"] <- "WGS"
prod$Aktivitetsnamn[prod$Aktivitetsnamn
                    == "Skicka prover till SciLife låg konc (WGS)"] <- "WGS"
prod$Aktivitetsnamn[prod$Aktivitetsnamn
                    == "U-Porfyrinprekursorer ALA/PBG (¤)"] <- "5-ALA/PBG"
prod$Aktivitetsnamn[prod$Aktivitetsnamn
                    == "M-Mitokondriell ATP-produktion"] <- "ATPprod"
prod$Aktivitetsnamn[prod$Aktivitetsnamn
                    == "U-Kreatinin"] <- "Krea"
---------------------------------------------------------------------------------------
# Rapport distribuerad!
# Här kan tidsperioden ändras för de kommande graferna!
res_distr <- prod %>%
  select(Aktivitetsnamn, Rapp_distr) %>% 
  na.omit() %>% 
  filter(Rapp_distr >= "2015-01-01", Rapp_distr <= "2020-07-31")

data_ref <- "Data t.o.m 2020-07-31, Data/Order_datum_ws_data.csv"

# Use month() to tabulate sample arrival per month
month(res_distr$Rapp_distr) %>% table()

# Add label = TRUE to make table more readable
month(res_distr$Rapp_distr, label = TRUE) %>% table()

# Create column month to hold labelled months
res_distr$månad <- month(res_distr$Rapp_distr, label = TRUE)

res_distr$år <- year(res_distr$Rapp_distr)

res_distr$pa <-  as.Date(cut(res_distr$Rapp_distr, breaks = "month"))

res_distr$ym <- as.yearmon(res_distr$Rapp_distr) 


---------------------------------------------------------------------------

# Use week() to tabulate sample arrival per week cumsumplot!
vecka <- week(res_distr$Rapp_distr) %>%
  table()
vecka <- as.data.frame.table(vecka)
vecka %>%
  mutate(cumsum = cumsum(Freq)) %>%
  ggplot(aes(x = ., y = cumsum)) +
  geom_point() +
  labs(title = "Kärnanalyser utveckling 2015.01.01-2020.08.14 rapport distrib.") +
  theme(panel.grid.major = element_line(color = "white"), 
        panel.background = element_rect(fill = "grey90"), 
        axis.line.y = element_line(color = "blue", size = 1), 
        axis.line.x = element_line(color = "blue", size = 1), 
        axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12))

# För att få år/månad för cumsumplot!

ym_table <- as.data.frame(table(res_distr$ym))

cumsumplot <- as.data.frame.table(ym_table)
ym_table %>%
  mutate(cumsum = cumsum(Freq)) %>%
  ggplot(aes(x = Var1, y = cumsum)) +
  geom_point(color = "#4535AA", alpha = 0.5, size = 3) +
  #geom_smooth(aes(x = as.numeric(ym_table$Var1))) +
  geom_path(aes(group = 1), size = 0.75) +
  scale_y_log10() +
  labs(title ="Kärnanalyser utveckling med logaritmisk skala 2015.01.01-2020.08.14 rapport distrib.",
       x = "När?",
       y = "Kumulativt antal",
       caption = "StarLimsData") +
  theme(panel.grid.major = element_line(color = "white"), 
        panel.background = element_rect(fill = "grey90"), 
        axis.line.y = element_line(color = "blue", size = 1), 
        axis.line.x = element_line(color = "blue", size = 1), 
        axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12))

ym_table %>%
  mutate(cumsum = cumsum(Freq)) %>%
  
  ggplot(aes(x = Var1, y = cumsum)) +
  geom_point(color = "#4535AA", alpha = 0.5, size = 3) +
  #geom_smooth(aes(x = as.numeric(ym_table$Var1))) +
  geom_path(aes(group = 1), size = 0.75) +
  #scale_y_log10() +
  labs(title ="Kärnanalyser utveckling 2015.01.01-2020.08.14 rapport distrib.",
       x = "När?",
       y = "Kumulativt antal",
       caption = "StarLimsData") +
  theme(panel.grid.major = element_line(color = "white"), 
        panel.background = element_rect(fill = "grey90"), 
        axis.line.y = element_line(color = "blue", size = 1), 
        axis.line.x = element_line(color = "blue", size = 1), 
        axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12))


# Create column week to hold labelled weeks
res_distr$vecka <- week(res_distr$Rapp_distr)

res_distr$år <- year(res_distr$Rapp_distr)

# Use week() to tabulate sample arrival per month
vecka <- week(res_distr$Rapp_distr) %>% table()
vecka <- as.data.frame.table(vecka)

# Create column week to hold labelled weeks
res_distr$vecka <- week(res_distr$Rapp_distr)

res_distr$år <- year(res_distr$Rapp_distr)

ggplot(data = res_distr, aes(x = Rapp_distr)) +
  geom_histogram(bins = 65, colour = "black", fill = "darkgreen") +
  scale_x_date(breaks = "1 year", limits = as.Date(c("2015-01-01", "2020-08-14"))) +
  labs(title ="Kärnanalyser månadsvy 2015-2020",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12))
#________________________________________________________________________

prov_ankomst <- prod %>%
  select(Aktivitetsnamn, Prov_ank) %>% 
  na.omit() %>% 
  filter(Prov_ank >= "2015-01-01", Prov_ank <= "2020-08-14")

# Use month() to tabulate sample arrival per month
month(prov_ankomst$Prov_ank) %>% table()

# Add label = TRUE to make table more readable
month(prov_ankomst$Prov_ank, label = TRUE) %>% table()

# Create column month to hold labelled months
prov_ankomst$månad <- month(prov_ankomst$Prov_ank, label = TRUE)

prov_ankomst$år <- year(prov_ankomst$Prov_ank)

prov_ankomst$pa <-  as.Date(cut(prov_ankomst$Prov_ank, breaks = "month"))
  
# Use week() to tabulate sample arrival per week cumsumplot!
vecka <- week(prov_ankomst$Prov_ank) %>%
  table()
vecka <- as.data.frame.table(vecka)
vecka %>%
  mutate(cumsum = cumsum(Freq)) %>%
  ggplot(aes(x = ., y = cumsum)) +
  geom_point() +
  labs(title = "Kärnanalyser baserat på ankomstdatum",
       x = "Veckonummer",
       y = "Ackumulerat antal",
       caption = data_ref) +
  theme(panel.grid.major = element_line(color = "white"), 
        panel.background = element_rect(fill = "grey90"), 
        axis.line.y = element_line(color = "blue", size = 1), 
        axis.line.x = element_line(color = "blue", size = 1), 
        axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12))
  

# För att få år/månad för cumsumplot!

prov_ankomst$ym <- as.yearmon(prov_ankomst$Prov_ank) 
ym_table <- as.data.frame(table(prov_ankomst$ym))

cumsumplot <- as.data.frame.table(ym_table)
ym_table %>%
  mutate(cumsum = cumsum(Freq)) %>%
  ggplot(aes(x = Var1, y = cumsum)) +
  geom_point(color = "#4535AA", alpha = 0.5, size = 3) +
  #geom_smooth(aes(x = as.numeric(ym_table$Var1))) +
  geom_path(aes(group = 1), size = 0.75) +
  scale_y_log10() +
  labs(title ="Kärnanalyser utveckling 2015.01.01-2020.08.14 prov ankom",
       x = "När?",
       y = "Kumulativt antal",
       caption = "StarLimsData") +
  theme(panel.grid.major = element_line(color = "white"), 
        panel.background = element_rect(fill = "grey90"), 
        axis.line.y = element_line(color = "blue", size = 1), 
        axis.line.x = element_line(color = "blue", size = 1), 
        axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12))

ym_table %>%
  mutate(cumsum = cumsum(Freq)) %>%
  
ggplot(aes(x = Var1, y = cumsum)) +
  geom_point(color = "#4535AA", alpha = 0.5, size = 3) +
  #geom_smooth(aes(x = as.numeric(ym_table$Var1))) +
  geom_path(aes(group = 1), size = 0.75) +
  #scale_y_log10() +
  labs(title ="Kärnanalyser utveckling 2015.01.01-2020.08.14 prov ankom",
       x = "När?",
       y = "Kumulativt antal",
       caption = "StarLimsData") +
  theme(panel.grid.major = element_line(color = "white"), 
        panel.background = element_rect(fill = "grey90"), 
        axis.line.y = element_line(color = "blue", size = 1), 
        axis.line.x = element_line(color = "blue", size = 1), 
        axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12))


# Create column week to hold labelled weeks
prov_ankomst$vecka <- week(prov_ankomst$Prov_ank)

prov_ankomst$år <- year(prov_ankomst$Prov_ank)

# Use week() to tabulate sample arrival per month
vecka <- week(prov_ankomst$Prov_ank) %>% table()
vecka <- as.data.frame.table(vecka)

# Create column week to hold labelled weeks
prov_ankomst$vecka <- week(prov_ankomst$Prov_ank)

prov_ankomst$år <- year(prov_ankomst$Prov_ank)

data_ref <- "Data t.o.m 2020-08-14, Data/Order_datum_ws_data.csv"

ggplot(data = prov_ankomst, aes(x = Prov_ank)) +
  geom_histogram(bins = 65, colour = "black", fill = "darkgreen") +
  scale_x_date(breaks = "1 year", limits = as.Date(c("2015-01-01", "2020-08-14"))) +
  labs(title ="Kärnanalyser månadsvy 2015-2020 pran",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12))

# create graphing function
method.graph.pa <- function(df, caption, na.rm = TRUE) {
  
  # create list of counties in data to loop over 
  method_list <- unique(df$Aktivitetsnamn)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(method_list)) {
    
    # create plot for each county in df 
    plot <- 
      ggplot(subset(df, df$Aktivitetsnamn == method_list[i]),
             aes(x = Prov_ank)) + 
      
      geom_histogram(bins = 65, colour = "black", aes(fill = ..count..)) +
      scale_fill_gradient("Antal", low = "#D6D1F5", high = "#4535AA") +
      scale_x_date(breaks = "1 year", limits = as.Date(c("2015-01-01", "2020-08-14"))) +
      labs(title = paste(method_list[i],
                         sep=""),
      x = "Månad",
      y = "Antal",
      caption = data_ref) +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5), text = element_text(size = 12, face = "bold", family="Helvetica"))
    
      ggtitle(paste(method_list[i],
                    sep=""))
    
    # save plots as .png
#    ggsave(plot, file = paste(results, 
#                            'projection_graphs/county_graphs/',
#                            method_list[i], ".png", sep=""), scale=2)
    
# save plots as .pdf
     ##ggsave(plot, file=paste(results, 
                            #'projection_graphs/county_graphs/',
                            #county_list[i], ".pdf", sep=''), scale=2)
    
    # print plots to screen
    print(plot)
    }
}

method.graph.rd <- function(df, caption, na.rm = TRUE) {
  
  # create list of counties in data to loop over 
  method_list <- unique(df$Aktivitetsnamn)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(method_list)) {
    
    # create plot for each county in df 
    plot <- 
      ggplot(subset(df, df$Aktivitetsnamn == method_list[i]),
             aes(x = Rapp_distr)) + 
      
      geom_histogram(bins = 65, colour = "black", aes(fill = ..count..)) +
      scale_fill_gradient("Antal", low = "#D6D1F5", high = "#4535AA") +
      scale_x_date(breaks = "1 year", limits = as.Date(c("2015-01-01", "2020-08-14"))) +
      labs(title = paste(method_list[i],
                         sep=""),
           x = "Månad",
           y = "Antal",
           caption = data_ref) +
      theme(axis.text.x = element_text(angle = 90),plot.title = element_text(hjust = 0.5), text = element_text(size = 12, face = "bold", family="Helvetica"))
    
    ggtitle(paste(method_list[i],
                  sep=""))
    
    # save plots as .png
    #    ggsave(plot, file = paste(results, 
    #                            'projection_graphs/county_graphs/',
    #                            method_list[i], ".png", sep=""), scale=2)
    
    # save plots as .pdf
    ##ggsave(plot, file=paste(results, 
    #'projection_graphs/county_graphs/',
    #county_list[i], ".pdf", sep=''), scale=2)
    
    # print plots to screen
    print(plot)
  }
}
# run graphing function on long df
method.graph.pa(prov_ankomst, data_ref)
method.graph.rd(res_distr, data_ref)

# create graphing function
ymwrapped.graph <- function(df, caption, na.rm = TRUE) {
  
  # create list of counties in data to loop over 
  method_list2 <- unique(df$Aktivitetsnamn)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(method_list2)) {
    
    # create plot for each county in df 
    plot2 <- 
      ggplot(subset(df, df$Aktivitetsnamn == method_list2[i]),
             aes(x = månad)) + 
      geom_bar(colour = "black", aes(fill = ..count..)) +
      scale_fill_gradient("Antal", low = "#D6D1F5", high = "#4535AA") +
      facet_wrap(~ år, ncol = 1) +
      labs(title = paste(method_list2[i],
                         sep=""),
           x = "Månad",
           y = "Antal",
           caption = data_ref) +
      theme(plot.title = element_text(hjust = 0.5), 
            text = element_text(size = 12, family = "Arial"))
    
    # save plots as .png
    #    ggsave(plot, file = paste(results, 
    #                            'projection_graphs/county_graphs/',
    #                            method_list[i], ".png", sep=""), scale=2)
    
    # save plots as .pdf
    ##ggsave(plot, file=paste(results, 
    #'projection_graphs/county_graphs/',
    #county_list[i], ".pdf", sep=''), scale=2)
    
    # print plots to screen
    print(plot2)
  }
}

ymwrapped.graph(prov_ankomst, data_ref)
ymwrapped.graph(res_distr, data_ref)

# Plot barchart of monthly by year of sample arrival
prov_ankomst %>% 
  ggplot(aes(månad)) +
  geom_bar(colour = "black", fill = "#4535AA") +
  facet_wrap(~ år, ncol = 1) +
  labs(title ="Kärnanalyser månadsvy 2015-2020 pr ank",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12))

prov_ankomst %>% 
  ggplot(aes(månad)) +
  geom_bar(colour = "black", fill = "#4535AA") +
  facet_wrap(~ år, ncol = 6) +
  labs(title ="Kärnanalyser månadsvy 2015-2020 pr ank",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))
# Plot barchart of monthly by year of sample arrival
prod_akt %>% 
  ggplot(aes(vecka)) +
  geom_bar(colour = "black", fill = "darkgreen") +
  facet_wrap(~ år, ncol = 1) +
  labs(title ="Kärnanalyser månadsvy 2015-2020",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12))

# Snyggt att använda labs som nedan. Enkelt att ändra titlar.
week %>% 
  ggplot(aes(x = ., y = Freq)) +
  geom_point(colour = "blue") +
  geom_line(group = 1) +
  labs(title ="Kärnanalyser veckovy 2015-2020",
       x = "Vecka",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12))

res_distr$år <- as.factor(res_distr$år)
res_distr$Aktivitetsnamn <- as.factor(res_distr$Aktivitetsnamn)

diffdotplot <- res_distr %>%
  filter(år %in% c("2018", "2019")) %>% 
  group_by(Aktivitetsnamn, år) %>% 
  summarize(n()) %>% 
  ungroup()

# För att få fram enkla aktivitetsnamn som kan plottas som labels längre ner...
meandiffyear <- diffdotplot %>% 
  group_by(Aktivitetsnamn) %>% 
  summarize(meandiff = mean(`n()`)) %>% 
  ungroup()

# Importerar fonter från extrafonts()
font_import()
fonts()
loadfonts()

diffdotplot <- diffdotplot  %>%
  arrange(år) %>% 
  mutate(Aktivitetsnamn = fct_reorder(Aktivitetsnamn, `n()`, last))

# Kategoriserar `n()` till Low resp. High beroende på om värdet för varje gruppering av aktivitetsnamn ligger
# under eller över medelvärdet.
diffdotplot <- diffdotplot %>% 
  group_by(Aktivitetsnamn) %>% 
  mutate(bin = cut(`n()`, breaks = c(-Inf, mean(`n()`), Inf), labels = c("Low","High")))


dotx <- ggplot(data = diffdotplot) +
  geom_path(aes(x = `n()`, y = Aktivitetsnamn),
            arrow = arrow(length = unit(2, "mm"), type = "closed")) +
    labs(x = "Antal",
         y = NULL) +
    labs(title = "Störst minskning för OS bland kärnanalyser mellan 2018-2019",
         subtitle = "Baserat på datum för distribuerat resultat",
         caption = data_ref) +
    geom_label(
      aes(x = `n()`,
          y = Aktivitetsnamn,
          label = `n()`,
          fill = år,
          hjust = ifelse(diffdotplot$bin == "High", -0.6, 1.5 ))) +
    
    coord_cartesian(xlim = c(-500, 5500)) +
    theme_gray() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 10, face = "bold", family = "Bookman Old Style"),
          title = element_text(family = "Bookman Old Style")) +
  geom_text(data = meandiffyear,
            aes(x = meandiff, 
            y = Aktivitetsnamn,
            label = Aktivitetsnamn),
            vjust = -1.5,
            family = "Bookman Old Style",
            color = "gray25",
            fontface = "bold") +
  scale_fill_discrete(name = NULL, labels = c("2018", "2019")) +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(.05, .95),
    legend.text = element_text(size = 12, face = "bold"),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(2, 6, 6, 6)
  )
# Tar bort text ur legend!
dotx + guides(
    fill = guide_legend(
      title = NULL,
      override.aes = aes(label = "")
    )
  )

OS_year_ank <- prov_ankomst %>%
  filter(Aktivitetsnamn == "OS", år) %>% 
  group_by(Aktivitetsnamn, ym, år, månad, vecka) %>% 
  tally() 

OS_year_ank$år <- as.factor(OS_year_ank$år)

prov_ankomst$år <- as.factor(prov_ankomst$år)
  
ggplot(data = OS_year_ank, aes(x = vecka, y = n, group = år, colour = år)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,52,2)) +
  theme(legend.title = element_blank())
  
ggplot(data = OS_year_ank, aes(x = vecka, y = n)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,52,2)) +
  theme(legend.title = element_blank())

#Flytande medelvärde över facetplot
# cma = centered moving average
# tma = trailing moving average
ym_table_ma <- ym_table %>%
  mutate(cma = rollmean(Freq, k = 3, fill = NA)) %>%
  mutate(tma = rollmean(Freq, k = 3, fill = NA, align = "right"))

# Hur kan jag kombinera dessa båda? Summarize prov_ankomst och sedan plotta? Plottar med rullande medelvärde nedan.
#' först med ankomstdatum sedan med distribueringsdatum.

prov_ankomst %>%
  group_by(ym) %>% 
  summarize(n = n()) %>%
  mutate(cma = rollmean(n, k = 5, fill = NA)) %>%
  mutate(tma = rollmean(n, k = 5, fill = NA, align = "right")) %>% 
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 2) +
  geom_line(aes(x = ym, y = cma), colour = "red", size = 2) +
  geom_line(aes(x = ym, y = tma), colour = "blue", size = 2) +
  labs(title ="Kärnanalyser månadsvy 2015-2020, prov_ankomst",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  group_by(ym) %>% 
  summarize(n = n()) %>%
  mutate(cma = rollmean(n, k = 5, fill = NA)) %>%
  mutate(tma = rollmean(n, k = 5, fill = NA, align = "right")) %>% 
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 2) +
  geom_line(aes(x = ym, y = cma), colour = "red", size = 2) +
  geom_line(aes(x = ym, y = tma), colour = "blue", size = 2) +
  labs(title ="Kärnanalyser månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

unique(res_distr$Aktivitetsnamn)
#' Plott där rullande medelvärde ersatts av geom_smooth!

res_distr %>%
  group_by(ym) %>% 
  summarize(n = n()) %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="Kärnanalyser månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "OS") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="OS månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "AA") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="AA månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "P-AC") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="P-AC månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "ATPprod") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="ATPprod månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "Krea") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="Krea månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "5-ALA/PBG") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="5-ALA/PBG månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "5-ALA/PBG") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="5-ALA/PBG månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

# Rapport distribuerad!
# Här kan tidsperioden ändras för de kommande graferna!
   


res_distr %>%
  filter(Aktivitetsnamn == "5-ALA/PBG") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="5-ALA/PBG månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "AA") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="AA månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "OS") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="OS månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "P-AC") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="P-AC månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "ATPprod") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="ATPprod månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

res_distr %>%
  filter(Aktivitetsnamn == "Krea") %>% 
  group_by(ym) %>% 
  count() %>%
  ggplot(aes(x = ym, y = n)) +
  geom_point(aes(x = ym, y = n), colour = "black", fill = "#4535AA", size = 1.5) +
  geom_smooth(method = "loess", formula = y ~ x, size = 1, alpha = 0.5) +
  labs(title ="Krea månadsvy 2015-2020, res_distr",
       x = "Månad",
       y = "Antal",
       caption = data_ref) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))

# Plot barchart of monthly by year of approved Biokemi

# Add label = TRUE to make table more readable
month(Biokemi$Godkännandedatum, label = TRUE) %>% table()

# Create column month to hold labelled months
Biokemi$månad <- month(Biokemi$Godkännandedatum, label = TRUE)

data_ref2 = "data t.o.m 2020-09-30, Data/Biokemi_201014.csv"
Biokemi %>% 
  ggplot(aes(månad)) +
  geom_bar(colour = "black", fill = "#4535AA") +
  facet_wrap(~ år, ncol = 1) +
  labs(title ="Biokemi 2015-2020 pr gk",
       x = "Månad",
       y = "Antal",
       caption = data_ref2) +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 90))
