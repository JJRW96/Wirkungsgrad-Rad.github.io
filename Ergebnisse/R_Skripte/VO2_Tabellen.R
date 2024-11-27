library(flextable)
library(dplyr)
library(officer)
library(sysfonts)

Energieanteile_data <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Energieanteile_data.rds")
EPOC_data_df <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/EPOC_data_df.rds")
EPOC_data_df_short <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/EPOC_data_df_short.rds")
Bedingungen_data <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Bedingungen_data.rds")
VO2_on_data_df <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/VO2_on_data_df.rds")

# EPOC_data_df_VO2 erstellen
EPOC_data_df_VO2 <- EPOC_data_df[, c("Proband", "Nr", "Bedingung", "Intensität", 
                                     "P_Tot", "P_Tot_kg", "Masse", "VO2_Ruhe", "tau_on",
                                     "VO2_SS_percent", "R_squared_on", "VO2_gross_SS", "VO2_net_SS",
                                     "delta_VO2_SS", "VO2_on_start", "VO2_Referenz",
                                     "VO2_avg", "VO2_SS_avg", "VCO2_avg", "VCO2_SS_avg",
                                     "RQ_avg","tau_on_min", "RQ_SS_avg","HR_percent")]

# t_delay aus VO2_on_data_df zu EPOC_data_df_VO2 hinzufügen
EPOC_data_df_VO2 <- merge(EPOC_data_df_VO2, VO2_on_data_df[, c("Proband", "Nr", "t_delay")],by = c("Proband", "Nr"), all.x = TRUE)
# EPOC_data_df_VO2 nach Proband und Nr sortieren
EPOC_data_df_VO2 <- EPOC_data_df_VO2[order(EPOC_data_df_VO2$Proband, EPOC_data_df_VO2$Nr),]

names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "t_delay"] <- "t_delay [s]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "P_Tot"] <- "P_Tot [W]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "P_Tot_kg"] <- "P_Tot_kg [W·kg⁻¹]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "Masse"] <- "Masse [kg]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "VO2_Ruhe"] <- "VO2_Ruhe [l·min⁻¹]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "tau_on"] <- "tau_on [s]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "VO2_SS_percent"] <- "VO2_SS_percent [%]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "R_squared_on"] <- "R2_on"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "VO2_gross_SS"] <- "VO2_Brutto_SS [l·min⁻¹]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "VO2_net_SS"] <- "VO2_Netto_SS [l·min⁻¹]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "delta_VO2_SS"] <- "delta_VO2 [l·min⁻¹]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "VO2_on_start"] <- "VO2_on_Start [l·min⁻¹]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "VO2_Referenz"] <- "VO2_Referenz [l·min⁻¹]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "VO2_avg"] <- "VO2_avg [l·min⁻¹]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "VO2_SS_avg"] <- "VO2_SS_avg [l·min⁻¹]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "VCO2_avg"] <- "VCO2_avg [l·min⁻¹]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "VCO2_SS_avg"] <- "VCO2_SS_avg [l·min⁻¹]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "RQ_avg"] <- "RQ_avg"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "RQ_SS_avg"] <- "RQ_SS_avg"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "tau_on_min"] <- "tau_on_min [min]"
names(EPOC_data_df_VO2)[names(EPOC_data_df_VO2) == "HR_percent"] <- "HR_percent [%]"


################################################################################################

# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("tau_on [s]",
               "delta_VO2 [l·min⁻¹]",
               "VO2_on_Start [l·min⁻¹]",                
               "t_delay [s]",                
               "R2_on",
               "VO2_Brutto_SS [l·min⁻¹]",
               "VO2_Netto_SS [l·min⁻¹]",
               "VO2_avg [l·min⁻¹]",
               "VCO2_avg [l·min⁻¹]",
               "RQ_avg",
               "VO2_Ruhe [l·min⁻¹]",
               "VO2_SS_percent [%]",
               "HR_percent [%]")
)

# Berechnung von Mittelwert und SD
stats$`Mittelwert ± SD` <- paste(
  case_when(
    stats$Variable %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                          "VO2_Netto_SS [l·min⁻¹]",
                          "VO2_on_Start [l·min⁻¹]",
                          "VO2_Ruhe [l·min⁻¹]",
                          "VO2_avg [l·min⁻¹]", 
                          "VO2_SS_avg [l·min⁻¹]",
                          "VCO2_avg [l·min⁻¹]", 
                          "VCO2_SS_avg [l·min⁻¹]",
                          "delta_VO2 [l·min⁻¹]") ~ 
      sprintf("%.3f ± %.3f", 
              sapply(EPOC_data_df_VO2[stats$Variable], mean, na.rm=TRUE),
              sapply(EPOC_data_df_VO2[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable %in% c("VO2_SS_percent [%]", "HR_percent [%]") ~
      sprintf("%.1f ± %.1f",
              sapply(EPOC_data_df_VO2[stats$Variable], mean, na.rm=TRUE),
              sapply(EPOC_data_df_VO2[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable %in% c("R2_on", "RQ_avg", "RQ_SS_avg") ~
      sprintf("%.2f ± %.2f",
              sapply(EPOC_data_df_VO2[stats$Variable], mean, na.rm=TRUE),
              sapply(EPOC_data_df_VO2[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable %in% c("tau_on [s]", "t_delay [s]") ~
      sprintf("%.1f ± %.1f",
              sapply(EPOC_data_df_VO2[stats$Variable], mean, na.rm=TRUE),
              sapply(EPOC_data_df_VO2[stats$Variable], sd, na.rm=TRUE))
  )
)

# Minimum und Maximum
stats$Minimum <- case_when(
  stats$Variable %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                        "VO2_Netto_SS [l·min⁻¹]",
                        "VO2_Ruhe [l·min⁻¹]",
                        "VO2_on_Start [l·min⁻¹]",
                        "VO2_avg [l·min⁻¹]", 
                        "VO2_SS_avg [l·min⁻¹]",
                        "VCO2_avg [l·min⁻¹]", 
                        "VCO2_SS_avg [l·min⁻¹]",
                        "delta_VO2 [l·min⁻¹]") ~
    sprintf("%.3f", sapply(EPOC_data_df_VO2[stats$Variable], min, na.rm=TRUE)),
  stats$Variable %in% c("VO2_SS_percent [%]", "tau_on [s]", "HR_percent [%]") ~
    sprintf("%.1f", sapply(EPOC_data_df_VO2[stats$Variable], min, na.rm=TRUE)),
  stats$Variable %in% c("R2_on", "RQ_avg", "RQ_SS_avg","t_delay [s]") ~
    sprintf("%.2f", sapply(EPOC_data_df_VO2[stats$Variable], min, na.rm=TRUE))
)

stats$Maximum <- case_when(
  stats$Variable %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                        "VO2_Netto_SS [l·min⁻¹]",
                        "VO2_Ruhe [l·min⁻¹]",
                        "VO2_on_Start [l·min⁻¹]",
                        "VO2_avg [l·min⁻¹]", 
                        "VO2_SS_avg [l·min⁻¹]",
                        "VCO2_avg [l·min⁻¹]", 
                        "VCO2_SS_avg [l·min⁻¹]",
                        "delta_VO2 [l·min⁻¹]") ~
    sprintf("%.3f", sapply(EPOC_data_df_VO2[stats$Variable], max, na.rm=TRUE)),
  stats$Variable %in% c("VO2_SS_percent [%]", "tau_on [s]","t_delay [s]") ~
    sprintf("%.1f", sapply(EPOC_data_df_VO2[stats$Variable], max, na.rm=TRUE)),
  stats$Variable %in% c("R2_on", "RQ_avg", "RQ_SS_avg", "HR_percent [%]") ~
    sprintf("%.2f", sapply(EPOC_data_df_VO2[stats$Variable], max, na.rm=TRUE))
)

# Nach allen Berechnungen
names(stats) <- c("Parameter", "Mittelwert ± SD", "Min", "Max")

# Tabelle erstellen
ft_VO2_stats_mean <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_Brutto_SS [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Brutto,SS"), " [l·min⁻¹]")
  ) %>%   
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_Netto_SS [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Netto,SS"), " [l·min⁻¹]")
  ) %>%
  compose(
      j = "Parameter",
      i = ~ Parameter == "VO2_Ruhe [l·min⁻¹]",
      value = as_paragraph("V̇O", as_sub("2,Ruhe"), " [l·min⁻¹]")
    ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_on_Start [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,on,Start"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_avg [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,avg"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_SS_avg [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,SS,avg"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VCO2_avg [l·min⁻¹]",
    value = as_paragraph("V̇CO", as_sub("2,avg"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VCO2_SS_avg [l·min⁻¹]",
    value = as_paragraph("V̇CO", as_sub("2,SS,avg"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "RQ_avg",
    value = as_paragraph("RQ", as_sub("avg"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "RQ_SS_avg",
    value = as_paragraph("RQ", as_sub("SS,avg"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "tau_on [s]",
    value = as_paragraph("τ", as_sub("on"), " [s]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "t_delay [s]",
    value = as_paragraph("t", as_sub("delay"), " [s]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "R2_on",
    value = as_paragraph("R", as_sub("2,on"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_SS_percent [%]",
    value = as_paragraph("V̇O", as_sub("2,SS,percent"), " [%]")
  ) %>% 
  compose(
    j = "Parameter",
    i = ~ Parameter == "delta_VO2 [l·min⁻¹]",
    value = as_paragraph("ΔV̇O", as_sub("2"), " [l·min⁻¹]") 
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "HR_percent [%]",
    value = as_paragraph("HR", as_sub("percent"), " [%]")
  )%>%
  theme_zebra(odd_header = "grey92", 
              even_header = "#EFEFEF", 
              odd_body = "#F9F9F9", 
              even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_row(
    values = as_paragraph(
      "τ", as_sub("on"), " [s]: Zeitkonstante der monoexponentiellen Anpassung des Sauerstoffvolumenstroms der Modellfunktion; ",
      "ΔV̇O", as_sub("2"), " [l·min⁻¹]: Amplitude der monoexponentiellen V̇O", as_sub("2"), "-Modellfunktion; ",
      "V̇O", as_sub("2,on,Start"), " [l·min⁻¹]: V̇O", as_sub("2"), " zu Beginn der Kinetik-Modellanpassung; " ,
      "t", as_sub("delay"), " [s]: Verzögerungszeit zwischen Belastungsbeginn und Start der monoexponentiellen Anpassung; ",
      "R", as_sub("2,on"), ": Bestimtheitsmaß der monoexponentiellen Modellfunktion; ",
      "V̇O", as_sub("2,Brutto,SS"), " [l·min⁻¹]: Brutto-V̇O", as_sub("2"), " im Steady-State (bei 4τ); ",
      "V̇O", as_sub("2,Netto,SS"), " [l·min⁻¹]: Netto-V̇O", as_sub("2"), " im Steady-State; ",
      "V̇O", as_sub("2,avg"), " [l·min⁻¹]: mittlerer V̇O", as_sub("2"), " während der gesamten Belastungsphase; ",
      "V̇CO", as_sub("2,avg"), " [l·min⁻¹]: mittlerer V̇CO", as_sub("2"), " während der gesamten Belastungsphase; ",
      "RQ", as_sub("avg"), ": mittlerer respiratorischer Quotient während der gesamten Belastungsphase; ",
      "V̇O", as_sub("2,Ruhe"), " [l·min⁻¹]: V̇O", as_sub("2"), " in Ruhe; ",
      "V̇O", as_sub("2,SS,percent"), " [%]: relativer Anteil des V̇O", as_sub("2"), " im Steady-State zum maximalen V̇O", as_sub("2"), " im Stufentest; ",
      "HR", as_sub("percent"), " [%]: relative mittlere Herzfrequenz während der Belastung zur maximalen Herzfrequenz im Stufentest"
    ),
    colwidths = 4
  ) %>%
  line_spacing(space = 1.2, part = "footer") %>%
  font(fontname = "Source Sans Pro", part = "all") %>%
  fontsize(size = 13, part = "header") %>%
  fontsize(size = 13, part = "body") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5))

# Setzen der Tabelle auf volle Breite
ft_VO2_stats_mean <- set_table_properties(ft_VO2_stats_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_VO2_stats_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_VO2_stats_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_VO2_stats_mean.rds")
ft_VO2_stats_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_VO2_stats_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_VO2_stats_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_VO2_stats_mean.rds")
ft_VO2_stats_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_VO2_stats_mean.rds")




################################################################################################

# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("tau_on [s]",
               "delta_VO2 [l·min⁻¹]",
               "VO2_on_Start [l·min⁻¹]",                
               "t_delay [s]",                
               "R2_on",
               "VO2_Brutto_SS [l·min⁻¹]",
               "VO2_Netto_SS [l·min⁻¹]",
               "VO2_avg [l·min⁻¹]",
               "VCO2_avg [l·min⁻¹]",
               "RQ_avg",
               "VO2_Ruhe [l·min⁻¹]",
               "VO2_SS_percent [%]",
               "HR_percent [%]")
)

# Berechnung für sitzend
stats$`Sitzen (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_sit <- filter(EPOC_data_df_VO2, Bedingung == "sitzen")
  
  if(var %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                "VO2_Netto_SS [l·min⁻¹]",
                "VO2_on_Start [l·min⁻¹]",
                "VO2_Ruhe [l·min⁻¹]",
                "VO2_avg [l·min⁻¹]",
                "VCO2_avg [l·min⁻¹]",
                "delta_VO2 [l·min⁻¹]")) {
    sprintf("%.3f ± %.3f", mean(data_sit[[var]], na.rm=TRUE), sd(data_sit[[var]], na.rm=TRUE))
  } else if(var %in% c("VO2_SS_percent [%]", "HR_percent [%]")) {
    sprintf("%.1f ± %.1f", mean(data_sit[[var]], na.rm=TRUE), sd(data_sit[[var]], na.rm=TRUE))
  } else if(var %in% c("R2_on", "RQ_avg","t_delay [s]")) {
    sprintf("%.2f ± %.2f", mean(data_sit[[var]], na.rm=TRUE), sd(data_sit[[var]], na.rm=TRUE))
  } else if(var %in% c("tau_on [s]")) {
    sprintf("%.1f ± %.1f", mean(data_sit[[var]], na.rm=TRUE), sd(data_sit[[var]], na.rm=TRUE))
  }
})

# Berechnung für stehen
stats$`Stehen (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_stand <- filter(EPOC_data_df_VO2, Bedingung == "stehen")
  
  if(var %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                "VO2_Netto_SS [l·min⁻¹]",
                "VO2_on_Start [l·min⁻¹]",
                "VO2_Ruhe [l·min⁻¹]",
                "VO2_avg [l·min⁻¹]",
                "VCO2_avg [l·min⁻¹]",
                "delta_VO2 [l·min⁻¹]")) {
    sprintf("%.3f ± %.3f", mean(data_stand[[var]], na.rm=TRUE), sd(data_stand[[var]], na.rm=TRUE))
  } else if(var %in% c("VO2_SS_percent [%]", "HR_percent [%]")) {
    sprintf("%.1f ± %.1f", mean(data_stand[[var]], na.rm=TRUE), sd(data_stand[[var]], na.rm=TRUE))
  } else if(var %in% c("R2_on", "RQ_avg","t_delay [s]")) {
    sprintf("%.2f ± %.2f", mean(data_stand[[var]], na.rm=TRUE), sd(data_stand[[var]], na.rm=TRUE))
  } else if(var %in% c("tau_on [s]")) {
    sprintf("%.1f ± %.1f", mean(data_stand[[var]], na.rm=TRUE), sd(data_stand[[var]], na.rm=TRUE))
  }
})

# Namen der Spalten anpassen
names(stats) <- c("Parameter", "Sitzen (MW ± SD)", "Stehen (MW ± SD)")

# Tabelle erstellen
ft_VO2_stats_Bedingung_mean <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_Brutto_SS [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Brutto,SS"), " [l·min⁻¹]")
  ) %>%   
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_Netto_SS [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Netto,SS"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_Ruhe [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Ruhe"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_on_Start [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,on,Start"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "t_delay [s]",
    value = as_paragraph("t", as_sub("delay"), " [s]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_avg [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,avg"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VCO2_avg [l·min⁻¹]",
    value = as_paragraph("V̇CO", as_sub("2,avg"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "RQ_avg",
    value = as_paragraph("RQ", as_sub("avg"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "tau_on [s]",
    value = as_paragraph("τ", as_sub("on"), " [s]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "R2_on",
    value = as_paragraph("R", as_sub("2,on"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_SS_percent [%]",
    value = as_paragraph("V̇O", as_sub("2,SS,percent"), " [%]")
  ) %>% 
  compose(
    j = "Parameter",
    i = ~ Parameter == "delta_VO2 [l·min⁻¹]",
    value = as_paragraph("ΔV̇O", as_sub("2"), " [l·min⁻¹]") 
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "HR_percent [%]",
    value = as_paragraph("HR", as_sub("percent"), " [%]")
  ) %>%
  theme_zebra(odd_header = "grey92", 
              even_header = "#EFEFEF", 
              odd_body = "#F9F9F9", 
              even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_row(
    values = as_paragraph(
      "τ", as_sub("on"), " [s]: Zeitkonstante der monoexponentiellen Anpassung des Sauerstoffvolumenstroms der Modellfunktion; ",
      "ΔV̇O", as_sub("2"), " [l·min⁻¹]: Amplitude der monoexponentiellen V̇O", as_sub("2"), "-Modellfunktion; ",
      "V̇O", as_sub("2,on,Start"), " [l·min⁻¹]: V̇O", as_sub("2"), " zu Beginn der Kinetik-Modellanpassung; " ,
      "t", as_sub("delay"), " [s]: Verzögerungszeit zwischen Belastungsbeginn und Start der monoexponentiellen Anpassung; ",
      "R", as_sub("2,on"), ": Bestimtheitsmaß der monoexponentiellen Modellfunktion; ",
      "V̇O", as_sub("2,Brutto,SS"), " [l·min⁻¹]: Brutto-V̇O", as_sub("2"), " im Steady-State (bei 4τ); ",
      "V̇O", as_sub("2,Netto,SS"), " [l·min⁻¹]: Netto-V̇O", as_sub("2"), " im Steady-State; ",
      "V̇O", as_sub("2,avg"), " [l·min⁻¹]: mittlerer V̇O", as_sub("2"), " während der gesamten Belastungsphase; ",
      "V̇CO", as_sub("2,avg"), " [l·min⁻¹]: mittlerer V̇CO", as_sub("2"), " während der gesamten Belastungsphase; ",
      "RQ", as_sub("avg"), ": mittlerer respiratorischer Quotient während der gesamten Belastungsphase; ",
      "V̇O", as_sub("2,Ruhe"), " [l·min⁻¹]: V̇O", as_sub("2"), " in Ruhe; ",
      "V̇O", as_sub("2,SS,percent"), " [%]: relativer Anteil des V̇O", as_sub("2"), " im Steady-State zum maximalen V̇O", as_sub("2"), " im Stufentest; ",
      "HR", as_sub("percent"), " [%]: relative mittlere Herzfrequenz während der Belastung zur maximalen Herzfrequenz im Stufentest"
    ),
    colwidths = 3
  ) %>%
  line_spacing(space = 1.2, part = "footer") %>%
  font(fontname = "Source Sans Pro", part = "all") %>%
  fontsize(size = 13, part = "header") %>%
  fontsize(size = 13, part = "body") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5))

# Setzen der Tabelle auf volle Breite
ft_VO2_stats_Bedingung_mean <- set_table_properties(ft_VO2_stats_Bedingung_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_VO2_stats_Bedingung_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_VO2_stats_Bedingung_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_VO2_stats_Bedingung_mean.rds")
ft_VO2_stats_Bedingung_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_VO2_stats_Bedingung_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_VO2_stats_Bedingung_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_VO2_stats_Bedingung_mean.rds")
ft_VO2_stats_Bedingung_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_VO2_stats_Bedingung_mean.rds")





######################################################################################################################


# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("tau_on [s]",
               "delta_VO2 [l·min⁻¹]",
               "VO2_on_Start [l·min⁻¹]",                
               "t_delay [s]",                
               "R2_on",
               "VO2_Brutto_SS [l·min⁻¹]",
               "VO2_Netto_SS [l·min⁻¹]",
               "VO2_avg [l·min⁻¹]",
               "VCO2_avg [l·min⁻¹]",
               "RQ_avg",
               "VO2_Ruhe [l·min⁻¹]",
               "VO2_SS_percent [%]",
               "HR_percent [%]")
)

# Funktion zur Formatierung von Mittelwert ± SD
format_mean_sd <- function(data, var, precision = 2) {
  mean_val <- mean(data[[var]], na.rm = TRUE)
  sd_val <- sd(data[[var]], na.rm = TRUE)
  sprintf(paste0("%.", precision, "f ± %.", precision, "f"), mean_val, sd_val)
}

# Berechnung für leicht
stats$`Leicht (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_light <- filter(EPOC_data_df_VO2, Intensität == "leicht")
  
  if(var %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                "VO2_Netto_SS [l·min⁻¹]",
                "VO2_on_Start [l·min⁻¹]",
                "VO2_Ruhe [l·min⁻¹]",
                "VO2_avg [l·min⁻¹]",
                "VCO2_avg [l·min⁻¹]",
                "delta_VO2 [l·min⁻¹]")) {
    format_mean_sd(data_light, var, 3)
  } else if(var %in% c("VO2_SS_percent [%]", "HR_percent [%]")) {
    format_mean_sd(data_light, var, 1)
  } else if(var %in% c("R2_on", "RQ_avg", "t_delay [s]")) {
    format_mean_sd(data_light, var, 2)
  } else if(var %in% c("tau_on [s]")) {
    format_mean_sd(data_light, var, 1)
  }
})

# Berechnung für moderat
stats$`Moderat (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_moderate <- filter(EPOC_data_df_VO2, Intensität == "moderat")
  
  if(var %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                "VO2_Netto_SS [l·min⁻¹]",
                "VO2_on_Start [l·min⁻¹]",
                "VO2_Ruhe [l·min⁻¹]",
                "VO2_avg [l·min⁻¹]",
                "VCO2_avg [l·min⁻¹]",
                "delta_VO2 [l·min⁻¹]")) {
    format_mean_sd(data_moderate, var, 3)
  } else if(var %in% c("VO2_SS_percent [%]", "HR_percent [%]")) {
    format_mean_sd(data_moderate, var, 1)
  } else if(var %in% c("R2_on", "RQ_avg", "t_delay [s]")) {
    format_mean_sd(data_moderate, var, 2)
  } else if(var %in% c("tau_on [s]")) {
    format_mean_sd(data_moderate, var, 1)
  }
})

# Berechnung für schwer
stats$`Schwer (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_heavy <- filter(EPOC_data_df_VO2, Intensität == "schwer")
  
  if(var %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                "VO2_Netto_SS [l·min⁻¹]",
                "VO2_on_Start [l·min⁻¹]",
                "VO2_Ruhe [l·min⁻¹]",
                "VO2_avg [l·min⁻¹]",
                "VCO2_avg [l·min⁻¹]",
                "delta_VO2 [l·min⁻¹]")) {
    format_mean_sd(data_heavy, var, 3)
  } else if(var %in% c("VO2_SS_percent [%]", "HR_percent [%]")) {
    format_mean_sd(data_heavy, var, 1)
  } else if(var %in% c("R2_on", "RQ_avg", "t_delay [s]")) {
    format_mean_sd(data_heavy, var, 2)
  } else if(var %in% c("tau_on [s]")) {
    format_mean_sd(data_heavy, var, 1)
  }
})

# Namen der Spalten anpassen
names(stats) <- c("Parameter", "Leicht (MW ± SD)", "Moderat (MW ± SD)", "Schwer (MW ± SD)")

# Tabelle erstellen
ft_VO2_stats_Intensitaet_mean <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_Brutto_SS [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Brutto,SS"), " [l·min⁻¹]")
  ) %>%   
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_Netto_SS [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Netto,SS"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_Ruhe [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Ruhe"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_on_Start [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,on,Start"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_avg [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,avg"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "t_delay [s]",
    value = as_paragraph("t", as_sub("delay"), " [s]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VCO2_avg [l·min⁻¹]",
    value = as_paragraph("V̇CO", as_sub("2,avg"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "RQ_avg",
    value = as_paragraph("RQ", as_sub("avg"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "tau_on [s]",
    value = as_paragraph("τ", as_sub("on"), " [s]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "R2_on",
    value = as_paragraph("R", as_sub("2,on"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_SS_percent [%]",
    value = as_paragraph("V̇O", as_sub("2,SS,percent"), " [%]")
  ) %>% 
  compose(
    j = "Parameter",
    i = ~ Parameter == "delta_VO2 [l·min⁻¹]",
    value = as_paragraph("ΔV̇O", as_sub("2"), " [l·min⁻¹]") 
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "HR_percent [%]",
    value = as_paragraph("HR", as_sub("percent"), " [%]")
  ) %>%
  theme_zebra(odd_header = "grey92", 
              even_header = "#EFEFEF", 
              odd_body = "#F9F9F9", 
              even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_row(
    values = as_paragraph(
      "τ", as_sub("on"), " [s]: Zeitkonstante der monoexponentiellen Anpassung des Sauerstoffvolumenstroms der Modellfunktion; ",
      "ΔV̇O", as_sub("2"), " [l·min⁻¹]: Amplitude der monoexponentiellen V̇O", as_sub("2"), "-Modellfunktion; ",
      "V̇O", as_sub("2,on,Start"), " [l·min⁻¹]: V̇O", as_sub("2"), " zu Beginn der Kinetik-Modellanpassung; " ,
      "t", as_sub("delay"), " [s]: Verzögerungszeit zwischen Belastungsbeginn und Start der monoexponentiellen Anpassung; ",
      "R", as_sub("2,on"), ": Bestimtheitsmaß der monoexponentiellen Modellfunktion; ",
      "V̇O", as_sub("2,Brutto,SS"), " [l·min⁻¹]: Brutto-V̇O", as_sub("2"), " im Steady-State (bei 4τ); ",
      "V̇O", as_sub("2,Netto,SS"), " [l·min⁻¹]: Netto-V̇O", as_sub("2"), " im Steady-State; ",
      "V̇O", as_sub("2,avg"), " [l·min⁻¹]: mittlerer V̇O", as_sub("2"), " während der gesamten Belastungsphase; ",
      "V̇CO", as_sub("2,avg"), " [l·min⁻¹]: mittlerer V̇CO", as_sub("2"), " während der gesamten Belastungsphase; ",
      "RQ", as_sub("avg"), ": mittlerer respiratorischer Quotient während der gesamten Belastungsphase; ",
      "V̇O", as_sub("2,Ruhe"), " [l·min⁻¹]: V̇O", as_sub("2"), " in Ruhe; ",
      "V̇O", as_sub("2,SS,percent"), " [%]: relativer Anteil des V̇O", as_sub("2"), " im Steady-State zum maximalen V̇O", as_sub("2"), " im Stufentest; ",
      "HR", as_sub("percent"), " [%]: relative mittlere Herzfrequenz während der Belastung zur maximalen Herzfrequenz im Stufentest"
    ),
    colwidths = 4
  ) %>%
  line_spacing(space = 1.2, part = "footer") %>%
  font(fontname = "Source Sans Pro", part = "all") %>%
  fontsize(size = 13, part = "header") %>%
  fontsize(size = 13, part = "body") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5))

# Setzen der Tabelle auf volle Breite
ft_VO2_stats_Intensitaet_mean <- set_table_properties(ft_VO2_stats_Intensitaet_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_VO2_stats_Intensitaet_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_VO2_stats_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_VO2_stats_Intensitaet_mean.rds")
ft_VO2_stats_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_VO2_stats_Intensitaet_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_VO2_stats_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_VO2_stats_Intensitaet_mean.rds")
ft_VO2_stats_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_VO2_stats_Intensitaet_mean.rds")


###########################################################################################################

# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("tau_on [s]",
               "delta_VO2 [l·min⁻¹]",
               "VO2_on_Start [l·min⁻¹]",                
               "t_delay [s]",                
               "R2_on",
               "VO2_Brutto_SS [l·min⁻¹]",
               "VO2_Netto_SS [l·min⁻¹]",
               "VO2_avg [l·min⁻¹]",
               "VCO2_avg [l·min⁻¹]",
               "RQ_avg",
               "VO2_Ruhe [l·min⁻¹]",
               "VO2_SS_percent [%]",
               "HR_percent [%]")
)

# Funktion zur Formatierung von Mittelwert ± SD
format_mean_sd <- function(data, var, precision = 2) {
  mean_val <- mean(data[[var]], na.rm = TRUE)
  sd_val <- sd(data[[var]], na.rm = TRUE)
  sprintf(paste0("%.", precision, "f ± %.", precision, "f"), mean_val, sd_val)
}

# Daten für Bedingung und Intensität zusammenstellen
stats_bed_int <- data.frame(
  Variable = stats$Variable,
  `Sitzen Leicht (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(EPOC_data_df_VO2, Bedingung == "sitzen", Intensität == "leicht")
    if(var %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                  "VO2_Netto_SS [l·min⁻¹]",
                  "VO2_on_Start [l·min⁻¹]",
                  "VO2_Ruhe [l·min⁻¹]",
                  "VO2_avg [l·min⁻¹]",
                  "VCO2_avg [l·min⁻¹]",
                  "delta_VO2 [l·min⁻¹]")) {
      format_mean_sd(data_subset, var, 3)
    } else if(var %in% c("VO2_SS_percent [%]", "HR_percent [%]")) {
      format_mean_sd(data_subset, var, 1)
    } else if(var %in% c("R2_on", "RQ_avg", "t_delay [s]")) {
      format_mean_sd(data_subset, var, 2)
    } else if(var %in% c("tau_on [s]")) {
      format_mean_sd(data_subset, var, 1)
    }
  }),
  `Sitzen Moderat (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(EPOC_data_df_VO2, Bedingung == "sitzen", Intensität == "moderat")
    if(var %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                  "VO2_Netto_SS [l·min⁻¹]",
                  "VO2_on_Start [l·min⁻¹]",
                  "VO2_Ruhe [l·min⁻¹]",
                  "VO2_avg [l·min⁻¹]",
                  "VCO2_avg [l·min⁻¹]",
                  "delta_VO2 [l·min⁻¹]")) {
      format_mean_sd(data_subset, var, 3)
    } else if(var %in% c("VO2_SS_percent [%]", "HR_percent [%]")) {
      format_mean_sd(data_subset, var, 1)
    } else if(var %in% c("R2_on", "RQ_avg", "t_delay [s]")) {
      format_mean_sd(data_subset, var, 2)
    } else if(var %in% c("tau_on [s]")) {
      format_mean_sd(data_subset, var, 1)
    }
  }),
  `Sitzen Schwer (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(EPOC_data_df_VO2, Bedingung == "sitzen", Intensität == "schwer")
    if(var %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                  "VO2_Netto_SS [l·min⁻¹]",
                  "VO2_on_Start [l·min⁻¹]",
                  "VO2_Ruhe [l·min⁻¹]",
                  "VO2_avg [l·min⁻¹]",
                  "VCO2_avg [l·min⁻¹]",
                  "delta_VO2 [l·min⁻¹]")) {
      format_mean_sd(data_subset, var, 3)
    } else if(var %in% c("VO2_SS_percent [%]", "HR_percent [%]")) {
      format_mean_sd(data_subset, var, 1)
    } else if(var %in% c("R2_on", "RQ_avg", "t_delay [s]")) {
      format_mean_sd(data_subset, var, 2)
    } else if(var %in% c("tau_on [s]")) {
      format_mean_sd(data_subset, var, 1)
    }
  }),
  `Stehen Leicht (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(EPOC_data_df_VO2, Bedingung == "stehen", Intensität == "leicht")
    if(var %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                  "VO2_Netto_SS [l·min⁻¹]",
                  "VO2_on_Start [l·min⁻¹]",
                  "VO2_Ruhe [l·min⁻¹]",
                  "VO2_avg [l·min⁻¹]",
                  "VCO2_avg [l·min⁻¹]",
                  "delta_VO2 [l·min⁻¹]")) {
      format_mean_sd(data_subset, var, 3)
    } else if(var %in% c("VO2_SS_percent [%]", "HR_percent [%]")) {
      format_mean_sd(data_subset, var, 1)
    } else if(var %in% c("R2_on", "RQ_avg", "t_delay [s]")) {
      format_mean_sd(data_subset, var, 2)
    } else if(var %in% c("tau_on [s]")) {
      format_mean_sd(data_subset, var, 1)
    }
  }),
  `Stehen Moderat (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(EPOC_data_df_VO2, Bedingung == "stehen", Intensität == "moderat")
    if(var %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                  "VO2_Netto_SS [l·min⁻¹]",
                  "VO2_on_Start [l·min⁻¹]",
                  "VO2_Ruhe [l·min⁻¹]",
                  "VO2_avg [l·min⁻¹]",
                  "VCO2_avg [l·min⁻¹]",
                  "delta_VO2 [l·min⁻¹]")) {
      format_mean_sd(data_subset, var, 3)
    } else if(var %in% c("VO2_SS_percent [%]", "HR_percent [%]")) {
      format_mean_sd(data_subset, var, 1)
    } else if(var %in% c("R2_on", "RQ_avg", "t_delay [s]")) {
      format_mean_sd(data_subset, var, 2)
    } else if(var %in% c("tau_on [s]")) {
      format_mean_sd(data_subset, var, 1)
    }
  }),
  `Stehen Schwer (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(EPOC_data_df_VO2, Bedingung == "stehen", Intensität == "schwer")
    if(var %in% c("VO2_Brutto_SS [l·min⁻¹]", 
                  "VO2_Netto_SS [l·min⁻¹]",
                  "VO2_on_Start [l·min⁻¹]",
                  "VO2_Ruhe [l·min⁻¹]",
                  "VO2_avg [l·min⁻¹]",
                  "VCO2_avg [l·min⁻¹]",
                  "delta_VO2 [l·min⁻¹]")) {
      format_mean_sd(data_subset, var, 3)
    } else if(var %in% c("VO2_SS_percent [%]", "HR_percent [%]")) {
      format_mean_sd(data_subset, var, 1)
    } else if(var %in% c("R2_on", "RQ_avg", "t_delay [s]")) {
      format_mean_sd(data_subset, var, 2)
    } else if(var %in% c("tau_on [s]")) {
      format_mean_sd(data_subset, var, 1)
    }
  })
)

# Tabelle erstellen
ft_VO2_stats_Bedingung_Intensitaet_mean <- flextable(stats_bed_int) %>%
  set_header_labels(
    Variable = "Parameter",
    `Sitzen.Leicht..MW...SD.` = "Leicht",
    `Sitzen.Moderat..MW...SD.` = "Moderat",
    `Sitzen.Schwer..MW...SD.` = "Schwer",
    `Stehen.Leicht..MW...SD.` = "Leicht",
    `Stehen.Moderat..MW...SD.` = "Moderat",
    `Stehen.Schwer..MW...SD.` = "Schwer"
  ) %>%
  add_header_row(values = c("", "Sitzen (MW ± SD)", "Stehen (MW ± SD)"),
                 colwidths = c(1, 3, 3)) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "VO2_Brutto_SS [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Brutto,SS"), " [l·min⁻¹]")
  ) %>%   
  compose(
    j = "Variable",  
    i = ~ Variable == "t_delay [s]", 
    value = as_paragraph("t", as_sub("delay"), " [s]")
  ) %>% 
  compose(
    j = "Variable",
    i = ~ Variable == "VO2_Netto_SS [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Netto,SS"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "VO2_Ruhe [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Ruhe"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "VO2_on_Start [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,on,Start"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "VO2_avg [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,avg"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "VCO2_avg [l·min⁻¹]",
    value = as_paragraph("V̇CO", as_sub("2,avg"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "RQ_avg",
    value = as_paragraph("RQ", as_sub("avg"))
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "tau_on [s]",
    value = as_paragraph("τ", as_sub("on"), " [s]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "R2_on",
    value = as_paragraph("R", as_sub("2,on"))
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "VO2_SS_percent [%]",
    value = as_paragraph("V̇O", as_sub("2,SS,percent"), " [%]")
  ) %>% 
  compose(
    j = "Variable",
    i = ~ Variable == "delta_VO2 [l·min⁻¹]",
    value = as_paragraph("ΔV̇O", as_sub("2"), " [l·min⁻¹]") 
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "HR_percent [%]",
    value = as_paragraph("HR", as_sub("percent"), " [%]")
  ) %>%
  theme_zebra(odd_header = "grey92", 
              even_header = "grey92", 
              odd_body = "#F9F9F9", 
              even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_row(
    values = as_paragraph(
      "τ", as_sub("on"), " [s]: Zeitkonstante der monoexponentiellen Anpassung des Sauerstoffvolumenstroms der Modellfunktion; ",
      "ΔV̇O", as_sub("2"), " [l·min⁻¹]: Amplitude der monoexponentiellen V̇O", as_sub("2"), "-Modellfunktion; ",
      "V̇O", as_sub("2,on,Start"), " [l·min⁻¹]: V̇O", as_sub("2"), " zu Beginn der Kinetik-Modellanpassung; " ,
      "t", as_sub("delay"), " [s]: Verzögerungszeit zwischen Belastungsbeginn und Start der monoexponentiellen Anpassung; ",
      "R", as_sub("2,on"), ": Bestimtheitsmaß der monoexponentiellen Modellfunktion; ",
      "V̇O", as_sub("2,Brutto,SS"), " [l·min⁻¹]: Brutto-V̇O", as_sub("2"), " im Steady-State (bei 4τ); ",
      "V̇O", as_sub("2,Netto,SS"), " [l·min⁻¹]: Netto-V̇O", as_sub("2"), " im Steady-State; ",
      "V̇O", as_sub("2,avg"), " [l·min⁻¹]: mittlerer V̇O", as_sub("2"), " während der gesamten Belastungsphase; ",
      "V̇CO", as_sub("2,avg"), " [l·min⁻¹]: mittlerer V̇CO", as_sub("2"), " während der gesamten Belastungsphase; ",
      "RQ", as_sub("avg"), ": mittlerer respiratorischer Quotient während der gesamten Belastungsphase; ",
      "V̇O", as_sub("2,Ruhe"), " [l·min⁻¹]: V̇O", as_sub("2"), " in Ruhe; ",
      "V̇O", as_sub("2,SS,percent"), " [%]: relativer Anteil des V̇O", as_sub("2"), " im Steady-State zum maximalen V̇O", as_sub("2"), " im Stufentest; ",
      "HR", as_sub("percent"), " [%]: relative mittlere Herzfrequenz während der Belastung zur maximalen Herzfrequenz im Stufentest"
    ),
    colwidths = 7
  ) %>%
  line_spacing(space = 1.2, part = "footer") %>%
  font(fontname = "Source Sans Pro", part = "all") %>%
  fontsize(size = 13, part = "header") %>%
  fontsize(size = 13, part = "body") %>%
  fontsize(size = 11, part = "footer") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline(i = 1, part = "header", j = 2:7, border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5))

# Setzen der Tabelle auf volle Breite
ft_VO2_stats_Bedingung_Intensitaet_mean <- set_table_properties(ft_VO2_stats_Bedingung_Intensitaet_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_VO2_stats_Bedingung_Intensitaet_mean


# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_VO2_stats_Bedingung_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_VO2_stats_Bedingung_Intensitaet_mean.rds")
ft_VO2_stats_Bedingung_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_VO2_stats_Bedingung_Intensitaet_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_VO2_stats_Bedingung_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_VO2_stats_Bedingung_Intensitaet_mean.rds")
ft_VO2_stats_Bedingung_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_VO2_stats_Bedingung_Intensitaet_mean.rds")
