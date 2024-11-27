library(flextable)
library(dplyr)
library(officer)

EPOC_data_df_short <- readRDS("rds/EPOC_data_df_short.rds")

# Spaltennamen ändern
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "R_squared_off"] <- "R2_off"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "aktive_Muskelmasse"] <- "MM_akt [kg]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "WPCR_corrected_calc"] <- "WPCR_corrected [kJ]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "P_Tot"] <- "P_Tot [W]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "P_Tot_kg"] <- "P_Tot_kg [W·kg⁻¹]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "A"] <- "A [l·min⁻¹]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "TauA"] <- "TauA [s]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "B"] <- "B [l·min⁻¹]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "TauB"] <- "TauB [s]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "VO2_Referenz"] <- "VO2_Referenz [l·min⁻¹]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "Masse"] <- "Masse [kg]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "VO2_Ruhe"] <- "VO2_Ruhe [l·min⁻¹]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "VO2_gross_SS"] <- "VO2_gross_SS [l·min⁻¹]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "VO2_net_SS"] <- "VO2_net_SS [l·min⁻¹]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "O2_Speicher"] <- "O2_Speicher [l]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "PCr_used"] <- "PCr_used [mmol·kg⁻¹]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "VO2_fast [l]"] <- "EPOC_fast [l]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "VO2_fast [ml·kg⁻¹]"] <- "EPOC_fast [ml·kg⁻¹]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "VO2_PCr [l]"] <- "EPOC_PCr [l]"
names(EPOC_data_df_short)[names(EPOC_data_df_short) == "VO2_PCr [ml·kg⁻¹]"] <- "EPOC_PCr [ml·kg⁻¹]"

# Umrechnung von TauA und TauB von Sekunden in Minuten
EPOC_data_df_short$"TauA [min]" <- EPOC_data_df_short$"TauA [s]" / 60
EPOC_data_df_short$"TauB [min]" <- EPOC_data_df_short$"TauB [s]" / 60

################################################################################################

# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("A [l·min⁻¹]", "TauA [min]",
               "B [l·min⁻¹]", "TauB [min]", 
               "VO2_Referenz [l·min⁻¹]", "R2_off",
               "EPOC_fast [l]", "EPOC_fast [ml·kg⁻¹]",
               "EPOC_PCr [l]", "EPOC_PCr [ml·kg⁻¹]", 
               "O2_Speicher [l]", "WPCR [kJ]",
               "WPCR_corrected [kJ]", 
               "PCr_used [mmol·kg⁻¹]")
)

# Berechnung von Mittelwert und SD
stats$`Mittelwert ± SD` <- paste(
  case_when(
    stats$Variable %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]","VO2_Referenz [l·min⁻¹]") ~ 
      sprintf("%.3f ± %.3f", 
              sapply(EPOC_data_df_short[gsub(" \\[min\\]", " [s]", stats$Variable)], mean, na.rm=TRUE),
              sapply(EPOC_data_df_short[gsub(" \\[min\\]", " [s]", stats$Variable)], sd, na.rm=TRUE)),
    stats$Variable %in% c("EPOC_fast [ml·kg⁻¹]", "EPOC_PCr [ml·kg⁻¹]") ~
      sprintf("%.2f ± %.2f",
              sapply(EPOC_data_df_short[stats$Variable], mean, na.rm=TRUE),
              sapply(EPOC_data_df_short[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable %in% c("A [l·min⁻¹]", "B [l·min⁻¹]", 
                          "R2_off","WPCR [kJ]", "WPCR_corrected [kJ]", 
                          "PCr_used [mmol·kg⁻¹]") ~
      sprintf("%.2f ± %.2f",
              sapply(EPOC_data_df_short[stats$Variable], mean, na.rm=TRUE),
              sapply(EPOC_data_df_short[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable %in% c("TauA [min]", "TauB [min]") ~
      sprintf("%.2f ± %.2f",
              sapply(EPOC_data_df_short[gsub(" \\[min\\]", " [s]", stats$Variable)], mean, na.rm=TRUE) / 60,
              sapply(EPOC_data_df_short[gsub(" \\[min\\]", " [s]", stats$Variable)], sd, na.rm=TRUE) / 60)
  ) 
)

# Minimum und Maximum anpassen
stats$Minimum <- case_when(
  stats$Variable %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]","VO2_Referenz [l·min⁻¹]") ~
    sprintf("%.3f", sapply(EPOC_data_df_short[stats$Variable], min, na.rm=TRUE)),
  stats$Variable %in% c("EPOC_fast [ml·kg⁻¹]", "EPOC_PCr [ml·kg⁻¹]", 
                        "A [l·min⁻¹]", "B [l·min⁻¹]", 
                        "R2_off","WPCR [kJ]", "WPCR_corrected [kJ]", 
                        "PCr_used [mmol·kg⁻¹]") ~
    sprintf("%.2f", sapply(EPOC_data_df_short[stats$Variable], min, na.rm=TRUE)),
  stats$Variable %in% c("TauA [min]", "TauB [min]") ~
    sprintf("%.2f", sapply(EPOC_data_df_short[gsub(" \\[min\\]", " [s]", stats$Variable)], min, na.rm=TRUE) / 60)
)

stats$Maximum <- case_when(
  stats$Variable %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]","VO2_Referenz [l·min⁻¹]") ~
    sprintf("%.3f", sapply(EPOC_data_df_short[stats$Variable], max, na.rm=TRUE)),
  stats$Variable %in% c("EPOC_fast [ml·kg⁻¹]", "EPOC_PCr [ml·kg⁻¹]", 
                        "A [l·min⁻¹]", "B [l·min⁻¹]", 
                        "R2_off","WPCR [kJ]", "WPCR_corrected [kJ]", 
                        "PCr_used [mmol·kg⁻¹]") ~
    sprintf("%.2f", sapply(EPOC_data_df_short[stats$Variable], max, na.rm=TRUE)),
  stats$Variable %in% c("TauA [min]", "TauB [min]") ~
    sprintf("%.2f", sapply(EPOC_data_df_short[gsub(" \\[min\\]", " [s]", stats$Variable)], max, na.rm=TRUE) / 60)
)

# Nach allen Berechnungen
names(stats) <- c("Parameter", "Mittelwert ± SD", "Min", "Max")

# Tabelle erstellen
ft_EPOC_stats_mean <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_Referenz [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Referenz"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "R2_off",
    value = as_paragraph("R", as_sub("2,off"))
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "PCr_used [mmol·kg⁻¹]",
    value = as_paragraph("PCr", as_sub("used"), " [mmol·kg⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "EPOC_PCr [l]",
    value = as_paragraph("EPOC", as_sub("PCr"), " [l]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "EPOC_PCr [ml·kg⁻¹]",
    value = as_paragraph("EPOC", as_sub("PCr"), " [ml·kg⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "EPOC_fast [l]",
    value = as_paragraph("EPOC", as_sub("fast"), " [l]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "EPOC_fast [ml·kg⁻¹]",
    value = as_paragraph("EPOC", as_sub("fast"), " [ml·kg⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "O2_Speicher [l]",
    value = as_paragraph("O", as_sub("2"),"-Speicher [l]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "WPCR [kJ]",
    value = as_paragraph("W", as_sub("PCr"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "WPCR_corrected [kJ]",
    value = as_paragraph("W", as_sub("PCr,corrected"), " [kJ]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "TauA [min]",
    value = as_paragraph("τ", as_sub("A"), " [min]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "TauB [min]",
    value = as_paragraph("τ", as_sub("B"), " [min]")
  ) %>%
  set_header_labels(
    Parameter = "Parameter",
    `Mittelwert ± SD` = "Mittelwert ± SD",
    Min = "Min",
    Max = "Max"
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
      "A [l·min⁻¹]: Amplitude der schnellen EPOC-Komponente; τ", as_sub("A"), " [min]: Zeitkonstante der schnellen EPOC-Komponente; ",
      "B [l·min⁻¹]: Amplitude der langsamen EPOC-Komponente; τ", as_sub("B"), " [min]: Zeitkonstante der langsamen EPOC-Komponente; ",
      "V̇O", as_sub("2,Referenz"), " [l·min⁻¹]: Sauerstoffvolumenstrom während der Referenzphase; R", as_sub("2,off"), ": Bestimmtheitsmaß der EPOC-Modellanpassungen; ",
      "EPOC", as_sub("fast"), " [l]: EPOC die der Rephosphorylierung von PCr zuzuordnen ist; ",
      "EPOC", as_sub("fast"), " [ml·kg⁻¹]: auf Körpermasse normierte EPOC die der Rephosphorylierung von PCr zuzuordnen ist; ",
      "EPOC", as_sub("PCr"), " [l]: EPOC die der Rephosphorylierung von PCr zuzuordnen ist, abzüglich der Sauerstoffspeicher; ",
      "EPOC", as_sub("PCr"), " [ml·kg⁻¹]: auf Körpermasse normierte EPOC die der Rephosphorylierung von PCr zuzuordnen ist, abzüglich der Sauerstoffspeicher; ",
      "O", as_sub("2"), "-Speicher [l]: Sauerstoffspeicher; ", "W", as_sub("PCr"), " [kJ]: Berechnete anaerobe- alaktazide Energiekomponente; ",
      "W", as_sub("PCr,corrected"), " [kJ]: Berechnete anaerobe- alaktazide Energiekomponente abzüglich der Sauerstoffspeicher; ",
      "PCr", as_sub("used"), " [mmol·kg⁻¹]: Umgesetzte PCr-Menge pro kg Muskelfeuchmasse "
    ),
    colwidths = 4
  ) %>%
  line_spacing(space = 1.2, part = "footer") %>%  # Hier wird der Zeilenabstand definiert
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
ft_EPOC_stats_mean <- set_table_properties(ft_EPOC_stats_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_EPOC_stats_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_EPOC_stats_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC_stats_mean.rds")
ft_EPOC_stats_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC_stats_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_EPOC_stats_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC_stats_mean.rds")
ft_EPOC_stats_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC_stats_mean.rds")






################################################################################################

# Berechnung der Statistiken gruppiert nach Bedingung
stats <- data.frame(
  Variable = c("A [l·min⁻¹]", "TauA [min]",
               "B [l·min⁻¹]", "TauB [min]", 
               "VO2_Referenz [l·min⁻¹]", "R2_off",
               "EPOC_fast [l]", "EPOC_fast [ml·kg⁻¹]",
               "EPOC_PCr [l]", "EPOC_PCr [ml·kg⁻¹]", 
               "O2_Speicher [l]", "WPCR [kJ]",
               "WPCR_corrected [kJ]", 
               "PCr_used [mmol·kg⁻¹]")
)

# Funktion zur Formatierung von Mittelwert ± SD
format_mean_sd <- function(data, var, precision = 2) {
  mean_val <- mean(data[[var]], na.rm = TRUE)
  sd_val <- sd(data[[var]], na.rm = TRUE)
  sprintf(paste0("%.", precision, "f ± %.", precision, "f"), mean_val, sd_val)
}

# Berechnung für sitzend
stats$`Sitzen (MW ± SD)` <- sapply(stats$Variable, function(var) {
  var_name <- gsub(" \\[min\\]", " [s]", var)
  data_sit <- filter(EPOC_data_df_short, Bedingung == "sitzen")
  
  if(var %in% c("TauA [min]", "TauB [min]")) {
    format_mean_sd(data_sit, var_name, 2)
  } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
    format_mean_sd(data_sit, var_name, 3)
  } else {
    format_mean_sd(data_sit, var_name, 2)
  }
})

# Berechnung für Stehen
stats$`Stehen (MW ± SD)` <- sapply(stats$Variable, function(var) {
  var_name <- gsub(" \\[min\\]", " [s]", var)
  data_stand <- filter(EPOC_data_df_short, Bedingung == "stehen")
  
  if(var %in% c("TauA [min]", "TauB [min]")) {
    format_mean_sd(data_stand, var_name, 2)
  } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
    format_mean_sd(data_stand, var_name, 3)
  } else {
    format_mean_sd(data_stand, var_name, 2)
  }
})

# Tabelle erstellen
ft_EPOC_stats_Bedingung_mean <- flextable(stats) %>%
  set_header_labels(
    Variable = "Parameter",
    `Sitzen (MW ± SD)` = "Sitzen (MW ± SD)",
    `Stehen (MW ± SD)` = "Stehen (MW ± SD)"
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "VO2_Referenz [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Referenz"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "R2_off",
    value = as_paragraph("R", as_sub("2,off"))
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "PCr_used [mmol·kg⁻¹]",
    value = as_paragraph("PCr", as_sub("used"), " [mmol·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "EPOC_PCr [l]",
    value = as_paragraph("EPOC", as_sub("PCr"), " [l]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "EPOC_PCr [ml·kg⁻¹]",
    value = as_paragraph("EPOC", as_sub("PCr"), " [ml·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "EPOC_fast [l]",
    value = as_paragraph("EPOC", as_sub("fast"), " [l]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "EPOC_fast [ml·kg⁻¹]",
    value = as_paragraph("EPOC", as_sub("fast"), " [ml·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "O2_Speicher [l]",
    value = as_paragraph("O", as_sub("2"),"-Speicher [l]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "WPCR [kJ]",
    value = as_paragraph("W", as_sub("PCr"), " [kJ]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "WPCR_corrected [kJ]",
    value = as_paragraph("W", as_sub("PCr,corrected"), " [kJ]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "TauA [min]",
    value = as_paragraph("τ", as_sub("A"), " [min]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "TauB [min]",
    value = as_paragraph("τ", as_sub("B"), " [min]")
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
      "A [l·min⁻¹]: Amplitude der schnellen EPOC-Komponente; τ", as_sub("A"), " [min]: Zeitkonstante der schnellen EPOC-Komponente; ",
      "B [l·min⁻¹]: Amplitude der langsamen EPOC-Komponente; τ", as_sub("B"), " [min]: Zeitkonstante der langsamen EPOC-Komponente; ",
      "V̇O", as_sub("2,Referenz"), " [l·min⁻¹]: Sauerstoffvolumenstrom während der Referenzphase; R", as_sub("2,off"), ": Bestimmtheitsmaß der EPOC-Modellanpassungen; ",
      "EPOC", as_sub("fast"), " [l]: EPOC die der Rephosphorylierung von PCr zuzuordnen ist; ",
      "EPOC", as_sub("fast"), " [ml·kg⁻¹]: auf Körpermasse normierte EPOC die der Rephosphorylierung von PCr zuzuordnen ist; ",
      "EPOC", as_sub("PCr"), " [l]: EPOC die der Rephosphorylierung von PCr zuzuordnen ist, abzüglich der Sauerstoffspeicher; ",
      "EPOC", as_sub("PCr"), " [ml·kg⁻¹]: auf Körpermasse normierte EPOC die der Rephosphorylierung von PCr zuzuordnen ist, abzüglich der Sauerstoffspeicher; ",
      "O", as_sub("2"), "-Speicher [l]: Sauerstoffspeicher; ", "W", as_sub("PCr"), " [kJ]: Berechnete anaerobe- alaktazide Energiekomponente; ",
      "W", as_sub("PCr,corrected"), " [kJ]: Berechnete anaerobe- alaktazide Energiekomponente abzüglich der Sauerstoffspeicher; ",
      "PCr", as_sub("used"), " [mmol·kg⁻¹]: Umgesetzte PCr-Menge pro kg Muskelfeuchmasse "
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
ft_EPOC_stats_Bedingung_mean <- set_table_properties(ft_EPOC_stats_Bedingung_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_EPOC_stats_Bedingung_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_EPOC_stats_Bedingung_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC_stats_Bedingung_mean.rds")
ft_EPOC_stats_Bedingung_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC_stats_Bedingung_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_EPOC_stats_Bedingung_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC_stats_Bedingung_mean.rds")
ft_EPOC_stats_Bedingung_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC_stats_Bedingung_mean.rds")




######################################################################################################################


# Berechnung der Statistiken gruppiert nach Intensität
stats <- data.frame(
  Variable = c("A [l·min⁻¹]", "TauA [min]",
               "B [l·min⁻¹]", "TauB [min]", 
               "VO2_Referenz [l·min⁻¹]", "R2_off",
               "EPOC_fast [l]", "EPOC_fast [ml·kg⁻¹]",
               "EPOC_PCr [l]", "EPOC_PCr [ml·kg⁻¹]", 
               "O2_Speicher [l]", "WPCR [kJ]",
               "WPCR_corrected [kJ]", 
               "PCr_used [mmol·kg⁻¹]")
)

# Funktion zur Formatierung von Mittelwert ± SD
format_mean_sd <- function(data, var, precision = 2) {
  mean_val <- mean(data[[var]], na.rm = TRUE)
  sd_val <- sd(data[[var]], na.rm = TRUE)
  sprintf(paste0("%.", precision, "f ± %.", precision, "f"), mean_val, sd_val)
}

# Berechnung für leicht
stats$`Leicht (MW ± SD)` <- sapply(stats$Variable, function(var) {
  var_name <- gsub(" \\[min\\]", " [s]", var)
  data_light <- filter(EPOC_data_df_short, Intensität == "leicht")
  
  if(var %in% c("TauA [min]", "TauB [min]")) {
    format_mean_sd(data_light, var_name, 2)
  } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
    format_mean_sd(data_light, var_name, 3)
  } else {
    format_mean_sd(data_light, var_name, 2)
  }
})

# Berechnung für moderat
stats$`Moderat (MW ± SD)` <- sapply(stats$Variable, function(var) {
  var_name <- gsub(" \\[min\\]", " [s]", var)
  data_moderate <- filter(EPOC_data_df_short, Intensität == "moderat")
  
  if(var %in% c("TauA [min]", "TauB [min]")) {
    format_mean_sd(data_moderate, var_name, 2)
  } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
    format_mean_sd(data_moderate, var_name, 3)
  } else {
    format_mean_sd(data_moderate, var_name, 2)
  }
})

# Berechnung für schwer
stats$`Schwer (MW ± SD)` <- sapply(stats$Variable, function(var) {
  var_name <- gsub(" \\[min\\]", " [s]", var)
  data_heavy <- filter(EPOC_data_df_short, Intensität == "schwer")
  
  if(var %in% c("TauA [min]", "TauB [min]")) {
    format_mean_sd(data_heavy, var_name, 2)
  } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
    format_mean_sd(data_heavy, var_name, 3)
  } else {
    format_mean_sd(data_heavy, var_name, 2)
  }
})

# Tabelle erstellen
ft_EPOC_stats_Intensitaet_mean <- flextable(stats) %>%
  set_header_labels(
    Variable = "Parameter",
    `Leicht (MW ± SD)` = "Leicht (MW ± SD)",
    `Moderat (MW ± SD)` = "Moderat (MW ± SD)",
    `Schwer (MW ± SD)` = "Schwer (MW ± SD)"
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "VO2_Referenz [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Referenz"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "R2_off",
    value = as_paragraph("R", as_sub("2,off"))
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "PCr_used [mmol·kg⁻¹]",
    value = as_paragraph("PCr", as_sub("used"), " [mmol·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "EPOC_PCr [l]",
    value = as_paragraph("EPOC", as_sub("PCr"), " [l]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "EPOC_PCr [ml·kg⁻¹]",
    value = as_paragraph("EPOC", as_sub("PCr"), " [ml·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "EPOC_fast [l]",
    value = as_paragraph("EPOC", as_sub("fast"), " [l]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "EPOC_fast [ml·kg⁻¹]",
    value = as_paragraph("EPOC", as_sub("fast"), " [ml·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "O2_Speicher [l]",
    value = as_paragraph("O", as_sub("2"),"-Speicher [l]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "WPCR [kJ]",
    value = as_paragraph("W", as_sub("PCr"), " [kJ]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "WPCR_corrected [kJ]",
    value = as_paragraph("W", as_sub("PCr,corrected"), " [kJ]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "TauA [min]",
    value = as_paragraph("τ", as_sub("A"), " [min]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "TauB [min]",
    value = as_paragraph("τ", as_sub("B"), " [min]")
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
      "A [l·min⁻¹]: Amplitude der schnellen EPOC-Komponente; τ", as_sub("A"), " [min]: Zeitkonstante der schnellen EPOC-Komponente; ",
      "B [l·min⁻¹]: Amplitude der langsamen EPOC-Komponente; τ", as_sub("B"), " [min]: Zeitkonstante der langsamen EPOC-Komponente; ",
      "V̇O", as_sub("2,Referenz"), " [l·min⁻¹]: Sauerstoffvolumenstrom während der Referenzphase; R", as_sub("2,off"), ": Bestimmtheitsmaß der EPOC-Modellanpassungen; ",
      "EPOC", as_sub("fast"), " [l]: EPOC die der Rephosphorylierung von PCr zuzuordnen ist; ",
      "EPOC", as_sub("fast"), " [ml·kg⁻¹]: auf Körpermasse normierte EPOC die der Rephosphorylierung von PCr zuzuordnen ist; ",
      "EPOC", as_sub("PCr"), " [l]: EPOC die der Rephosphorylierung von PCr zuzuordnen ist, abzüglich der Sauerstoffspeicher; ",
      "EPOC", as_sub("PCr"), " [ml·kg⁻¹]: auf Körpermasse normierte EPOC die der Rephosphorylierung von PCr zuzuordnen ist, abzüglich der Sauerstoffspeicher; ",
      "O", as_sub("2"), "-Speicher [l]: Sauerstoffspeicher; ", "W", as_sub("PCr"), " [kJ]: Berechnete anaerobe- alaktazide Energiekomponente; ",
      "W", as_sub("PCr,corrected"), " [kJ]: Berechnete anaerobe- alaktazide Energiekomponente abzüglich der Sauerstoffspeicher; ",
      "PCr", as_sub("used"), " [mmol·kg⁻¹]: Umgesetzte PCr-Menge pro kg Muskelfeuchmasse "
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
ft_EPOC_stats_Intensitaet_mean <- set_table_properties(ft_EPOC_stats_Intensitaet_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_EPOC_stats_Intensitaet_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_EPOC_stats_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC_stats_Intensitaet_mean.rds")
ft_EPOC_stats_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC_stats_Intensitaet_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_EPOC_stats_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC_stats_Intensitaet_mean.rds")
ft_EPOC_stats_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC_stats_Intensitaet_mean.rds")

###########################################################################################################

# Daten für Bedingung und Intensität zusammenstellen
stats_bed_int <- data.frame(
  Variable = stats$Variable,
  `Sitzen Leicht (MW ± SD)` = sapply(stats$Variable, function(var) {
    var_name <- gsub(" \\[min\\]", " [s]", var)
    data_subset <- filter(EPOC_data_df_short, Bedingung == "sitzen", Intensität == "leicht")
    if(var %in% c("TauA [min]", "TauB [min]")) {
      format_mean_sd(data_subset, var_name, 2)
    } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
      format_mean_sd(data_subset, var_name, 3)
    } else {
      format_mean_sd(data_subset, var_name, 2)
    }
  }),
  `Sitzen Moderat (MW ± SD)` = sapply(stats$Variable, function(var) {
    var_name <- gsub(" \\[min\\]", " [s]", var)
    data_subset <- filter(EPOC_data_df_short, Bedingung == "sitzen", Intensität == "moderat")
    if(var %in% c("TauA [min]", "TauB [min]")) {
      format_mean_sd(data_subset, var_name, 2)
    } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
      format_mean_sd(data_subset, var_name, 3)
    } else {
      format_mean_sd(data_subset, var_name, 2)
    }
  }),
  `Sitzen Schwer (MW ± SD)` = sapply(stats$Variable, function(var) {
    var_name <- gsub(" \\[min\\]", " [s]", var)
    data_subset <- filter(EPOC_data_df_short, Bedingung == "sitzen", Intensität == "schwer")
    if(var %in% c("TauA [min]", "TauB [min]")) {
      format_mean_sd(data_subset, var_name, 2)
    } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
      format_mean_sd(data_subset, var_name, 3)
    } else {
      format_mean_sd(data_subset, var_name, 2)
    }
  }),
  `Stehen Leicht (MW ± SD)` = sapply(stats$Variable, function(var) {
    var_name <- gsub(" \\[min\\]", " [s]", var)
    data_subset <- filter(EPOC_data_df_short, Bedingung == "stehen", Intensität == "leicht")
    if(var %in% c("TauA [min]", "TauB [min]")) {
      format_mean_sd(data_subset, var_name, 2)
    } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
      format_mean_sd(data_subset, var_name, 3)
    } else {
      format_mean_sd(data_subset, var_name, 2)
    }
  }),
  `Stehen Moderat (MW ± SD)` = sapply(stats$Variable, function(var) {
    var_name <- gsub(" \\[min\\]", " [s]", var)
    data_subset <- filter(EPOC_data_df_short, Bedingung == "stehen", Intensität == "moderat")
    if(var %in% c("TauA [min]", "TauB [min]")) {
      format_mean_sd(data_subset, var_name, 2)
    } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
      format_mean_sd(data_subset, var_name, 3)
    } else {
      format_mean_sd(data_subset, var_name, 2)
    }
  }),
  `Stehen Schwer (MW ± SD)` = sapply(stats$Variable, function(var) {
    var_name <- gsub(" \\[min\\]", " [s]", var)
    data_subset <- filter(EPOC_data_df_short, Bedingung == "stehen", Intensität == "schwer")
    if(var %in% c("TauA [min]", "TauB [min]")) {
      format_mean_sd(data_subset, var_name, 2)
    } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
      format_mean_sd(data_subset, var_name, 3)
    } else {
      format_mean_sd(data_subset, var_name, 2)
    }
  })
)

# Tabelle erstellen
ft_EPOC_stats_Bedingung_Intensitaet_mean <- flextable(stats_bed_int) %>%
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
    i = ~ Variable == "VO2_Referenz [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,Referenz"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Variable", 
    i = ~ Variable == "R2_off",
    value = as_paragraph("R", as_sub("2,off"))
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "PCr_used [mmol·kg⁻¹]",
    value = as_paragraph("PCr", as_sub("used"), " [mmol·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "EPOC_PCr [l]",
    value = as_paragraph("EPOC", as_sub("PCr"), " [l]")
  ) %>%
  compose(
    j = "Variable", 
    i = ~ Variable == "EPOC_PCr [ml·kg⁻¹]",
    value = as_paragraph("EPOC", as_sub("PCr"), " [ml·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "EPOC_fast [l]", 
    value = as_paragraph("EPOC", as_sub("fast"), " [l]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "EPOC_fast [ml·kg⁻¹]",
    value = as_paragraph("EPOC", as_sub("fast"), " [ml·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "O2_Speicher [l]",
    value = as_paragraph("O", as_sub("2"),"-Speicher [l]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "WPCR [kJ]",
    value = as_paragraph("W", as_sub("PCr"), " [kJ]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "WPCR_corrected [kJ]",
    value = as_paragraph("W", as_sub("PCr,corrected"), " [kJ]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "TauA [min]",
    value = as_paragraph("τ", as_sub("A"), " [min]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "TauB [min]",
    value = as_paragraph("τ", as_sub("B"), " [min]")
  ) %>%
  theme_zebra(odd_header = "grey92", 
              even_header = "grey92", 
              odd_body = "#F9F9F9", 
              even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline(i = 1, border = fp_border(color = "darkgrey", width = 0.5)) %>%
  padding(padding = 4, part = "all") %>%
  add_footer_row(
    values = as_paragraph(
      "A [l·min⁻¹]: Amplitude der schnellen EPOC-Komponente; τ", as_sub("A"), " [min]: Zeitkonstante der schnellen EPOC-Komponente; ",
      "B [l·min⁻¹]: Amplitude der langsamen EPOC-Komponente; τ", as_sub("B"), " [min]: Zeitkonstante der langsamen EPOC-Komponente; ",
      "V̇O", as_sub("2,Referenz"), " [l·min⁻¹]: Sauerstoffvolumenstrom während der Referenzphase; R", as_sub("2,off"), ": Bestimmtheitsmaß der EPOC-Modellanpassungen; ",
      "EPOC", as_sub("fast"), " [l]: EPOC die der Rephosphorylierung von PCr zuzuordnen ist; ",
      "EPOC", as_sub("fast"), " [ml·kg⁻¹]: auf Körpermasse normierte EPOC die der Rephosphorylierung von PCr zuzuordnen ist; ",
      "EPOC", as_sub("PCr"), " [l]: EPOC die der Rephosphorylierung von PCr zuzuordnen ist, abzüglich der Sauerstoffspeicher; ",
      "EPOC", as_sub("PCr"), " [ml·kg⁻¹]: auf Körpermasse normierte EPOC die der Rephosphorylierung von PCr zuzuordnen ist, abzüglich der Sauerstoffspeicher; ",
      "O", as_sub("2"), "-Speicher [l]: Sauerstoffspeicher; ", "W", as_sub("PCr"), " [kJ]: Berechnete anaerobe- alaktazide Energiekomponente; ",
      "W", as_sub("PCr,corrected"), " [kJ]: Berechnete anaerobe- alaktazide Energiekomponente abzüglich der Sauerstoffspeicher; ",
      "PCr", as_sub("used"), " [mmol·kg⁻¹]: Umgesetzte PCr-Menge pro kg Muskelfeuchmasse "
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
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5))

# Setzen der Tabelle auf volle Breite
ft_EPOC_stats_Bedingung_Intensitaet_mean <- set_table_properties(ft_EPOC_stats_Bedingung_Intensitaet_mean, width = 1, layout = "autofit")

ft_EPOC_stats_Bedingung_Intensitaet_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_EPOC_stats_Bedingung_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC_stats_Bedingung_Intensitaet_mean.rds")
ft_EPOC_stats_Bedingung_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC_stats_Bedingung_Intensitaet_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_EPOC_stats_Bedingung_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC_stats_Bedingung_Intensitaet_mean.rds")
ft_EPOC_stats_Bedingung_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC_stats_Bedingung_Intensitaet_mean.rds")
