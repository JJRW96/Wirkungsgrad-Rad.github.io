library(flextable)
library(dplyr)
library(officer)
library(sysfonts)

Erg_data_komplett <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/Erg_data_komplett.rds")

# Anpassen des Wertes für Proband 13 unter den spezifischen Bedingungen
Erg_data_komplett$nD_Vorgabe[
  Erg_data_komplett$Proband == 13 & 
    Erg_data_komplett$Nr == 2 & 
    Erg_data_komplett$Intensität == "leicht" &
    Erg_data_komplett$nD_Vorgabe != 100
] <- 100

saveRDS(Erg_data_komplett, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Erg_data_komplett.rds")
saveRDS(Erg_data_komplett, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/Erg_data_komplett.rds")

Erg_data_komplett <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/Erg_data_komplett.rds")


# Erg_data_short erstellen
Erg_data_short <- Erg_data_komplett[, c("Proband", "Nr", "Bedingung", "Intensität",
                                        "nD_Vorgabe","nD", "P_mech_Vorgabe", "P_mech", 
                                        "W_kg", "Efficiency", "P_mean_abs", "P_max",
                                        "P_max_kg", "Pedal_Smoothness", "P_L_percent", 
                                        "P_R_percent", "PInt_Kinematik", "PInt_Modell",
                                        "PInt_Kinematik_Modell", "P_Int_Min", "Koerperlaenge",
                                        "Masse", "lBein", "lOS", "lUS", "uOS", "uUS",
                                        "lKurbel")]

names(Erg_data_short)[names(Erg_data_short) == "nD"] <- "nD [U·min⁻¹]"
names(Erg_data_short)[names(Erg_data_short) == "nD_Vorgabe"] <- "nD_Vorgabe [U·min⁻¹]"
names(Erg_data_short)[names(Erg_data_short) == "P_mech_Vorgabe"] <- "P_mech_Vorgabe [W]"
names(Erg_data_short)[names(Erg_data_short) == "P_mech"] <- "P_mech [W]"
names(Erg_data_short)[names(Erg_data_short) == "W_kg"] <- "P_mech_kg [W·kg⁻¹]"
names(Erg_data_short)[names(Erg_data_short) == "Efficiency"] <- "Torque Efficiency [%]"
names(Erg_data_short)[names(Erg_data_short) == "P_mean_abs"] <- "P_mech_abs [W]"
names(Erg_data_short)[names(Erg_data_short) == "P_max"] <- "P_mech_max [W]"
names(Erg_data_short)[names(Erg_data_short) == "P_max_kg"] <- "P_mech_max_kg [W·kg⁻¹]"
names(Erg_data_short)[names(Erg_data_short) == "Pedal_Smoothness"] <- "Pedal_Smoothness [%]"
names(Erg_data_short)[names(Erg_data_short) == "P_L_percent"] <- "P_L_percent [%]"
names(Erg_data_short)[names(Erg_data_short) == "P_R_percent"] <- "P_R_percent [%]"
names(Erg_data_short)[names(Erg_data_short) == "PInt_Kinematik"] <- "P_Int_Kinematik [W]"
names(Erg_data_short)[names(Erg_data_short) == "PInt_Modell"] <- "P_Int_Modell [W]"
names(Erg_data_short)[names(Erg_data_short) == "PInt_Kinematik_Modell"] <- "P_Int_Kinematik_Modell [W]"
names(Erg_data_short)[names(Erg_data_short) == "P_Int_Min"] <- "P_Int_Minetti [W]"
names(Erg_data_short)[names(Erg_data_short) == "Koerperlaenge"] <- "Koerperlaenge [m]"
names(Erg_data_short)[names(Erg_data_short) == "Masse"] <- "Masse [kg]"
names(Erg_data_short)[names(Erg_data_short) == "lBein"] <- "lBein [m]"
names(Erg_data_short)[names(Erg_data_short) == "lOS"] <- "lOS [m]"
names(Erg_data_short)[names(Erg_data_short) == "lUS"] <- "lUS [m]"
names(Erg_data_short)[names(Erg_data_short) == "uOS"] <- "uOS [m]"
names(Erg_data_short)[names(Erg_data_short) == "uUS"] <- "uUS [m]"
names(Erg_data_short)[names(Erg_data_short) == "lKurbel"] <- "lKurbel [m]"


################################################################################################
# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("nD_Vorgabe [U·min⁻¹]",
               "nD [U·min⁻¹]",
               "P_mech_Vorgabe [W]",
               "P_mech [W]",
               "P_mech_kg [W·kg⁻¹]",
               "Torque Efficiency [%]", 
               "P_mech_abs [W]",
               "P_mech_max [W]",
               "P_mech_max_kg [W·kg⁻¹]",
               "Pedal_Smoothness [%]",
               "P_L_percent [%]",
               "P_R_percent [%]",
               "P_Int_Kinematik [W]",
               "P_Int_Modell [W]",
               "P_Int_Kinematik_Modell [W]",
               "P_Int_Minetti [W]")
)

# Berechnung von Mittelwert und SD
stats$`Mittelwert ± SD` <- paste(
  case_when(
    stats$Variable %in% c("nD_Vorgabe [U·min⁻¹]", "P_mech_Vorgabe [W]") ~
      sprintf("%.0f ± %.0f", 
              sapply(Erg_data_short[stats$Variable], mean, na.rm=TRUE),
              sapply(Erg_data_short[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable %in% c("nD [U·min⁻¹]", "P_mech [W]", "P_mech_abs [W]",
                          "P_mech_max [W]", "P_Int_Kinematik [W]", "P_Int_Modell [W]",
                          "P_Int_Kinematik_Modell [W]", "P_Int_Minetti [W]",
                          "Torque Efficiency [%]", "Pedal_Smoothness [%]",
                          "P_L_percent [%]", "P_R_percent [%]") ~
      sprintf("%.1f ± %.1f",
              sapply(Erg_data_short[stats$Variable], mean, na.rm=TRUE),
              sapply(Erg_data_short[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable %in% c("P_mech_kg [W·kg⁻¹]", "P_mech_max_kg [W·kg⁻¹]") ~
      sprintf("%.2f ± %.2f",
              sapply(Erg_data_short[stats$Variable], mean, na.rm=TRUE),
              sapply(Erg_data_short[stats$Variable], sd, na.rm=TRUE))
  )
)

# Minimum und Maximum
stats$Minimum <- case_when(
  stats$Variable %in% c("nD_Vorgabe [U·min⁻¹]", "P_mech_Vorgabe [W]") ~
    sprintf("%.0f", sapply(Erg_data_short[stats$Variable], min, na.rm=TRUE)),
  stats$Variable %in% c("nD [U·min⁻¹]", "P_mech [W]", "P_mech_abs [W]",
                        "P_mech_max [W]", "P_Int_Kinematik [W]", "P_Int_Modell [W]",
                        "P_Int_Kinematik_Modell [W]", "P_Int_Minetti [W]",
                        "Torque Efficiency [%]", "Pedal_Smoothness [%]",
                        "P_L_percent [%]", "P_R_percent [%]") ~
    sprintf("%.1f", sapply(Erg_data_short[stats$Variable], min, na.rm=TRUE)),
  stats$Variable %in% c("P_mech_kg [W·kg⁻¹]", "P_mech_max_kg [W·kg⁻¹]") ~
    sprintf("%.2f", sapply(Erg_data_short[stats$Variable], min, na.rm=TRUE))
)

stats$Maximum <- case_when(
  stats$Variable %in% c("nD_Vorgabe [U·min⁻¹]", "P_mech_Vorgabe [W]") ~
    sprintf("%.0f", sapply(Erg_data_short[stats$Variable], max, na.rm=TRUE)),
  stats$Variable %in% c("nD [U·min⁻¹]", "P_mech [W]", "P_mech_abs [W]",
                        "P_mech_max [W]", "P_Int_Kinematik [W]", "P_Int_Modell [W]",
                        "P_Int_Kinematik_Modell [W]", "P_Int_Minetti [W]",
                        "Torque Efficiency [%]", "Pedal_Smoothness [%]",
                        "P_L_percent [%]", "P_R_percent [%]") ~
    sprintf("%.1f", sapply(Erg_data_short[stats$Variable], max, na.rm=TRUE)),
  stats$Variable %in% c("P_mech_kg [W·kg⁻¹]", "P_mech_max_kg [W·kg⁻¹]") ~
    sprintf("%.2f", sapply(Erg_data_short[stats$Variable], max, na.rm=TRUE))
)

# Nach allen Berechnungen
names(stats) <- c("Parameter", "Mittelwert ± SD", "Min", "Max")

# Tabelle erstellen
ft_Ergometer_stats_mean <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "nD_Vorgabe [U·min⁻¹]",
    value = as_paragraph("nD", as_sub("Vorgabe"), " [U·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "nD [U·min⁻¹]",
    value = as_paragraph("nD", " [U·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_Vorgabe [W]",
    value = as_paragraph("P", as_sub("mech,Vorgabe"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech [W]",
    value = as_paragraph("P", as_sub("mech"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_kg [W·kg⁻¹]",
    value = as_paragraph("P", as_sub("mech,kg"), " [W·kg⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_abs [W]",
    value = as_paragraph("P", as_sub("mech,abs"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_max [W]",
    value = as_paragraph("P", as_sub("mech,max"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_max_kg [W·kg⁻¹]",
    value = as_paragraph("P", as_sub("mech,max,kg"), " [W·kg⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_L_percent [%]",
    value = as_paragraph("P", as_sub("L,percent"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_R_percent [%]",
    value = as_paragraph("P", as_sub("R,percent"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Kinematik [W]",
    value = as_paragraph("P", as_sub("Int,Kinematik"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Modell [W]",
    value = as_paragraph("P", as_sub("Int,Modell"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Kinematik_Modell [W]",
    value = as_paragraph("P", as_sub("Int,Kinematik,Modell"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Minetti [W]",
    value = as_paragraph("P", as_sub("Int,Minetti"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Torque Efficiency [%]",
    value = as_paragraph("Torque Efficiency [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Pedal_Smoothness [%]",
    value = as_paragraph("Pedal Smoothness [%]")
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
      "nD", as_sub("Vorgabe"), " [U·min⁻¹]: Vorgegebene Trittrate; ",
      "nD", " [U·min⁻¹]: Gemessene Trittrate; ",
      "P", as_sub("mech,Vorgabe"), " [W]: Vorgegebene mechanische Leistung; ",
      "P", as_sub("mech"), " [W]: Gemessene mechanische Leistung; ",
      "P", as_sub("mech,kg"), " [W·kg⁻¹]: Gewichtsspezifische ", "P", as_sub("mech"), "; ",
      "Torque Efficiency [%]: Prozentualer Anteil des effektiven Antriebsmoments am Gesamtdrehmoment der Kurbel; ",
      "P", as_sub("mech,abs"), " [W]: Durchschnittliche absolute mechanische Gesamtleistung (Summation der Beträge von pos. und neg. Leistung); ",
      "P", as_sub("max"), " [W]: Durchschnittliche maximale ", "P", as_sub("mech"), " pro Pedalzyklus; ",
      "P", as_sub("max,kg"), " [W·kg⁻¹]: Gewichtsspezifische P", as_sub("max"), "; ",
      "Pedal Smoothness [%]: Verhältnis von mittlerer zu maximaler Leistung während einer Pedalumdrehung ", "P", as_sub("mech"), "; ",
      "P", as_sub("L,percent"), " [%]: prozentualer Leistungsanteil des linken Beins; ",
      "P", as_sub("R,percent"), " [%]: prozentualer Leistungsanteil des rechten Beins; ",
      "P", as_sub("Int,Kinematik"), " [W]: Interne Leistung basierend auf kinematischer 3D-Bewegungsanalyse; ",
      "P", as_sub("Int,Modell"), " [W]: Interne Leistung basierend auf biomechanischer Modellsimulation; ",
      "P", as_sub("Int,Kinematik,Modell"), " [W]: Modellierte interne Leistung basierend auf mittleren Differenzen zwischen kinematischer und berechneter interner Leistung; ",
      "P", as_sub("Int,Minetti"), " [W]: Interne Leistung nach biomechanischem Berechnungsmodell von Minetti et al. (2001)"
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
ft_Ergometer_stats_mean <- set_table_properties(ft_Ergometer_stats_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_Ergometer_stats_mean


# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_Ergometer_stats_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Ergometer_stats_mean.rds")
ft_Ergometer_stats_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Ergometer_stats_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_Ergometer_stats_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Ergometer_stats_mean.rds")
ft_Ergometer_stats_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Ergometer_stats_mean.rds")




################################################################################################
# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("nD_Vorgabe [U·min⁻¹]",
               "nD [U·min⁻¹]",
               "P_mech_Vorgabe [W]",
               "P_mech [W]",
               "P_mech_kg [W·kg⁻¹]",
               "Torque Efficiency [%]", 
               "P_mech_abs [W]",
               "P_mech_max [W]",
               "P_mech_max_kg [W·kg⁻¹]",
               "Pedal_Smoothness [%]",
               "P_L_percent [%]",
               "P_R_percent [%]",
               "P_Int_Kinematik [W]",
               "P_Int_Modell [W]",
               "P_Int_Kinematik_Modell [W]",
               "P_Int_Minetti [W]")
)

# Berechnung für sitzend
stats$`Sitzen (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_sit <- filter(Erg_data_short, Bedingung == "sitzen")
  
  if(var %in% c("nD_Vorgabe [U·min⁻¹]", "P_mech_Vorgabe [W]")) {
    sprintf("%.0f ± %.0f", mean(data_sit[[var]], na.rm=TRUE), sd(data_sit[[var]], na.rm=TRUE))
  } else if(var %in% c("nD [U·min⁻¹]", "P_mech [W]", "P_mech_abs [W]",
                       "P_mech_max [W]", "P_Int_Kinematik [W]", "P_Int_Modell [W]",
                       "P_Int_Kinematik_Modell [W]", "P_Int_Minetti [W]",
                       "Torque Efficiency [%]", "Pedal_Smoothness [%]",
                       "P_L_percent [%]", "P_R_percent [%]")) {
    sprintf("%.1f ± %.1f", mean(data_sit[[var]], na.rm=TRUE), sd(data_sit[[var]], na.rm=TRUE))
  } else if(var %in% c("P_mech_kg [W·kg⁻¹]", "P_mech_max_kg [W·kg⁻¹]")) {
    sprintf("%.2f ± %.2f", mean(data_sit[[var]], na.rm=TRUE), sd(data_sit[[var]], na.rm=TRUE))
  }
})

# Berechnung für stehen
stats$`Stehen (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_stand <- filter(Erg_data_short, Bedingung == "stehen")
  
  if(var %in% c("nD_Vorgabe [U·min⁻¹]", "P_mech_Vorgabe [W]")) {
    sprintf("%.0f ± %.0f", mean(data_stand[[var]], na.rm=TRUE), sd(data_stand[[var]], na.rm=TRUE))
  } else if(var %in% c("nD [U·min⁻¹]", "P_mech [W]", "P_mech_abs [W]",
                       "P_mech_max [W]", "P_Int_Kinematik [W]", "P_Int_Modell [W]",
                       "P_Int_Kinematik_Modell [W]", "P_Int_Minetti [W]",
                       "Torque Efficiency [%]", "Pedal_Smoothness [%]",
                       "P_L_percent [%]", "P_R_percent [%]")) {
    sprintf("%.1f ± %.1f", mean(data_stand[[var]], na.rm=TRUE), sd(data_stand[[var]], na.rm=TRUE))
  } else if(var %in% c("P_mech_kg [W·kg⁻¹]", "P_mech_max_kg [W·kg⁻¹]")) {
    sprintf("%.2f ± %.2f", mean(data_stand[[var]], na.rm=TRUE), sd(data_stand[[var]], na.rm=TRUE))
  }
})

# Namen der Spalten anpassen
names(stats) <- c("Parameter", "Sitzen (MW ± SD)", "Stehen (MW ± SD)")

# Tabelle erstellen
ft_Ergometer_stats_Bedingung_mean <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "nD_Vorgabe [U·min⁻¹]",
    value = as_paragraph("nD", as_sub("Vorgabe"), " [U·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "nD [U·min⁻¹]",
    value = as_paragraph("nD", " [U·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_Vorgabe [W]",
    value = as_paragraph("P", as_sub("mech,Vorgabe"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech [W]",
    value = as_paragraph("P", as_sub("mech"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_kg [W·kg⁻¹]",
    value = as_paragraph("P", as_sub("mech,kg"), " [W·kg⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_abs [W]",
    value = as_paragraph("P", as_sub("mech,abs"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_max [W]",
    value = as_paragraph("P", as_sub("mech,max"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_max_kg [W·kg⁻¹]",
    value = as_paragraph("P", as_sub("mech,max,kg"), " [W·kg⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_L_percent [%]",
    value = as_paragraph("P", as_sub("L,percent"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_R_percent [%]",
    value = as_paragraph("P", as_sub("R,percent"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Kinematik [W]",
    value = as_paragraph("P", as_sub("Int,Kinematik"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Modell [W]",
    value = as_paragraph("P", as_sub("Int,Modell"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Kinematik_Modell [W]",
    value = as_paragraph("P", as_sub("Int,Kinematik,Modell"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Minetti [W]",
    value = as_paragraph("P", as_sub("Int,Minetti"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Torque Efficiency [%]",
    value = as_paragraph("Torque Efficiency [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Pedal_Smoothness [%]",
    value = as_paragraph("Pedal Smoothness [%]")
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
      "nD", as_sub("Vorgabe"), " [U·min⁻¹]: Vorgegebene Trittrate; ",
      "nD", " [U·min⁻¹]: Gemessene Trittrate; ",
      "P", as_sub("mech,Vorgabe"), " [W]: Vorgegebene mechanische Leistung; ",
      "P", as_sub("mech"), " [W]: Gemessene mechanische Leistung; ",
      "P", as_sub("mech,kg"), " [W·kg⁻¹]: Gewichtsspezifische ", "P", as_sub("mech"), "; ",
      "Torque Efficiency [%]: Prozentualer Anteil des effektiven Antriebsmoments am Gesamtdrehmoment der Kurbel; ",
      "P", as_sub("mech,abs"), " [W]: Durchschnittliche absolute mechanische Gesamtleistung (Summation der Beträge von pos. und neg. Leistung); ",
      "P", as_sub("mech,max"), " [W]: Durchschnittliche maximale ", "P", as_sub("mech"), " pro Pedalzyklus; ",
      "P", as_sub("mech,max,kg"), " [W·kg⁻¹]: Gewichtsspezifische P", as_sub("mech,max"), "; ",
      "Pedal Smoothness [%]: Verhältnis von mittlerer zu maximaler Leistung während einer Pedalumdrehung ", "P", as_sub("mech"), "; ",
      "P", as_sub("L,percent"), " [%]: prozentualer Leistungsanteil des linken Beins; ",
      "P", as_sub("R,percent"), " [%]: prozentualer Leistungsanteil des rechten Beins; ",
      "P", as_sub("Int,Kinematik"), " [W]: Interne Leistung basierend auf kinematischer 3D-Bewegungsanalyse; ",
      "P", as_sub("Int,Modell"), " [W]: Interne Leistung basierend auf biomechanischer Modellsimulation; ",
      "P", as_sub("Int,Kinematik,Modell"), " [W]: Modellierte interne Leistung basierend auf mittleren Differenzen zwischen kinematischer und berechneter interner Leistung; ",
      "P", as_sub("Int,Minetti"), " [W]: Interne Leistung nach biomechanischem Berechnungsmodell von Minetti et al. (2001)"
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
ft_Ergometer_stats_Bedingung_mean <- set_table_properties(ft_Ergometer_stats_Bedingung_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_Ergometer_stats_Bedingung_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_Ergometer_stats_Bedingung_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Ergometer_stats_Bedingung_mean.rds")
ft_Ergometer_stats_Bedingung_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Ergometer_stats_Bedingung_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_Ergometer_stats_Bedingung_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Ergometer_stats_Bedingung_mean.rds")
ft_Ergometer_stats_Bedingung_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Ergometer_stats_Bedingung_mean.rds")





######################################################################################################################
# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("nD_Vorgabe [U·min⁻¹]",
               "nD [U·min⁻¹]",
               "P_mech_Vorgabe [W]",
               "P_mech [W]",
               "P_mech_kg [W·kg⁻¹]",
               "Torque Efficiency [%]", 
               "P_mech_abs [W]",
               "P_mech_max [W]",
               "P_mech_max_kg [W·kg⁻¹]",
               "Pedal_Smoothness [%]",
               "P_L_percent [%]",
               "P_R_percent [%]",
               "P_Int_Kinematik [W]",
               "P_Int_Modell [W]",
               "P_Int_Kinematik_Modell [W]",
               "P_Int_Minetti [W]")
)

# Berechnung für leicht
stats$`Leicht (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_light <- filter(Erg_data_short, Intensität == "leicht")
  
  # Prüfe zuerst auf NA-Werte
  if(all(is.na(data_light[[var]]))) {
    return("keine Daten")
  }
  
  if(var %in% c("nD_Vorgabe [U·min⁻¹]", "P_mech_Vorgabe [W]")) {
    sprintf("%.0f ± %.0f", mean(data_light[[var]], na.rm=TRUE), sd(data_light[[var]], na.rm=TRUE))
  } else if(var %in% c("nD [U·min⁻¹]", "P_mech [W]", "P_mech_abs [W]",
                       "P_mech_max [W]", "P_Int_Kinematik [W]", "P_Int_Modell [W]",
                       "P_Int_Kinematik_Modell [W]", "P_Int_Minetti [W]",
                       "Torque Efficiency [%]", "Pedal_Smoothness [%]",
                       "P_L_percent [%]", "P_R_percent [%]")) {
    sprintf("%.1f ± %.1f", mean(data_light[[var]], na.rm=TRUE), sd(data_light[[var]], na.rm=TRUE))
  } else if(var %in% c("P_mech_kg [W·kg⁻¹]", "P_mech_max_kg [W·kg⁻¹]")) {
    sprintf("%.2f ± %.2f", mean(data_light[[var]], na.rm=TRUE), sd(data_light[[var]], na.rm=TRUE))
  }
})

# Berechnung für moderat
stats$`Moderat (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_moderate <- filter(Erg_data_short, Intensität == "moderat")
  
  if(all(is.na(data_moderate[[var]]))) {
    return("keine Daten")
  }
  
  if(var %in% c("nD_Vorgabe [U·min⁻¹]", "P_mech_Vorgabe [W]")) {
    sprintf("%.0f ± %.0f", mean(data_moderate[[var]], na.rm=TRUE), sd(data_moderate[[var]], na.rm=TRUE))
  } else if(var %in% c("nD [U·min⁻¹]", "P_mech [W]", "P_mech_abs [W]",
                       "P_mech_max [W]", "P_Int_Kinematik [W]", "P_Int_Modell [W]",
                       "P_Int_Kinematik_Modell [W]", "P_Int_Minetti [W]",
                       "Torque Efficiency [%]", "Pedal_Smoothness [%]",
                       "P_L_percent [%]", "P_R_percent [%]")) {
    sprintf("%.1f ± %.1f", mean(data_moderate[[var]], na.rm=TRUE), sd(data_moderate[[var]], na.rm=TRUE))
  } else if(var %in% c("P_mech_kg [W·kg⁻¹]", "P_mech_max_kg [W·kg⁻¹]")) {
    sprintf("%.2f ± %.2f", mean(data_moderate[[var]], na.rm=TRUE), sd(data_moderate[[var]], na.rm=TRUE))
  }
})

# Berechnung für schwer
stats$`Schwer (MW ± SD)` <- sapply(stats$Variable, function(var) {
  data_heavy <- filter(Erg_data_short, Intensität == "schwer")
  
  if(all(is.na(data_heavy[[var]]))) {
    return("keine Daten")
  }
  
  if(var %in% c("nD_Vorgabe [U·min⁻¹]", "P_mech_Vorgabe [W]")) {
    sprintf("%.0f ± %.0f", mean(data_heavy[[var]], na.rm=TRUE), sd(data_heavy[[var]], na.rm=TRUE))
  } else if(var %in% c("nD [U·min⁻¹]", "P_mech [W]", "P_mech_abs [W]",
                       "P_mech_max [W]", "P_Int_Kinematik [W]", "P_Int_Modell [W]",
                       "P_Int_Kinematik_Modell [W]", "P_Int_Minetti [W]",
                       "Torque Efficiency [%]", "Pedal_Smoothness [%]",
                       "P_L_percent [%]", "P_R_percent [%]")) {
    sprintf("%.1f ± %.1f", mean(data_heavy[[var]], na.rm=TRUE), sd(data_heavy[[var]], na.rm=TRUE))
  } else if(var %in% c("P_mech_kg [W·kg⁻¹]", "P_mech_max_kg [W·kg⁻¹]")) {
    sprintf("%.2f ± %.2f", mean(data_heavy[[var]], na.rm=TRUE), sd(data_heavy[[var]], na.rm=TRUE))
  }
})
# Namen der Spalten anpassen
names(stats) <- c("Parameter", "Leicht (MW ± SD)", "Moderat (MW ± SD)", "Schwer (MW ± SD)")

# Tabelle erstellen
ft_Ergometer_stats_Intensitaet_mean <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "nD_Vorgabe [U·min⁻¹]",
    value = as_paragraph("nD", as_sub("Vorgabe"), " [U·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "nD [U·min⁻¹]",
    value = as_paragraph("nD", " [U·min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_Vorgabe [W]",
    value = as_paragraph("P", as_sub("mech,Vorgabe"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech [W]",
    value = as_paragraph("P", as_sub("mech"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_kg [W·kg⁻¹]",
    value = as_paragraph("P", as_sub("mech,kg"), " [W·kg⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_abs [W]",
    value = as_paragraph("P", as_sub("mech,abs"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_max [W]",
    value = as_paragraph("P", as_sub("mech,max"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_mech_max_kg [W·kg⁻¹]",
    value = as_paragraph("P", as_sub("mech,max,kg"), " [W·kg⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_L_percent [%]",
    value = as_paragraph("P", as_sub("L,percent"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_R_percent [%]",
    value = as_paragraph("P", as_sub("R,percent"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Kinematik [W]",
    value = as_paragraph("P", as_sub("Int,Kinematik"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Modell [W]",
    value = as_paragraph("P", as_sub("Int,Modell"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Kinematik_Modell [W]",
    value = as_paragraph("P", as_sub("Int,Kinematik,Modell"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Minetti [W]",
    value = as_paragraph("P", as_sub("Int,Minetti"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Torque Efficiency [%]",
    value = as_paragraph("Torque Efficiency [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "Pedal_Smoothness [%]",
    value = as_paragraph("Pedal Smoothness [%]")
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
      "nD", as_sub("Vorgabe"), " [U·min⁻¹]: Vorgegebene Trittrate; ",
      "nD", " [U·min⁻¹]: Gemessene Trittrate; ",
      "P", as_sub("mech,Vorgabe"), " [W]: Vorgegebene mechanische Leistung; ",
      "P", as_sub("mech"), " [W]: Gemessene mechanische Leistung; ",
      "P", as_sub("mech,kg"), " [W·kg⁻¹]: Gewichtsspezifische ", "P", as_sub("mech"), "; ",
      "Torque Efficiency [%]: Prozentualer Anteil des effektiven Antriebsmoments am Gesamtdrehmoment der Kurbel; ",
      "P", as_sub("mech,abs"), " [W]: Durchschnittliche absolute mechanische Gesamtleistung (Summation der Beträge von pos. und neg. Leistung); ",
      "P", as_sub("mech,max"), " [W]: Durchschnittliche maximale ", "P", as_sub("mech"), " pro Pedalzyklus; ",
      "P", as_sub("mech,max,kg"), " [W·kg⁻¹]: Gewichtsspezifische P", as_sub("mech,max"), "; ",
      "Pedal Smoothness [%]: Verhältnis von mittlerer zu maximaler Leistung während einer Pedalumdrehung ", "P", as_sub("mech"), "; ",
      "P", as_sub("L,percent"), " [%]: prozentualer Leistungsanteil des linken Beins; ",
      "P", as_sub("R,percent"), " [%]: prozentualer Leistungsanteil des rechten Beins; ",
      "P", as_sub("Int,Kinematik"), " [W]: Interne Leistung basierend auf kinematischer 3D-Bewegungsanalyse; ",
      "P", as_sub("Int,Modell"), " [W]: Interne Leistung basierend auf biomechanischer Modellsimulation; ",
      "P", as_sub("Int,Kinematik,Modell"), " [W]: Modellierte interne Leistung basierend auf mittleren Differenzen zwischen kinematischer und berechneter interner Leistung; ",
      "P", as_sub("Int,Minetti"), " [W]: Interne Leistung nach biomechanischem Berechnungsmodell von Minetti et al. (2001)"
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
ft_Ergometer_stats_Intensitaet_mean <- set_table_properties(ft_Ergometer_stats_Intensitaet_mean, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_Ergometer_stats_Intensitaet_mean


# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_Ergometer_stats_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Ergometer_stats_Intensitaet_mean.rds")
ft_Ergometer_stats_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Ergometer_stats_Intensitaet_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_Ergometer_stats_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Ergometer_stats_Intensitaet_mean.rds")
ft_Ergometer_stats_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Ergometer_stats_Intensitaet_mean.rds")


###########################################################################################################
# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("nD_Vorgabe [U·min⁻¹]",
               "nD [U·min⁻¹]",
               "P_mech_Vorgabe [W]",
               "P_mech [W]",
               "P_mech_kg [W·kg⁻¹]",
               "Torque Efficiency [%]", 
               "P_mech_abs [W]",
               "P_mech_max [W]",
               "P_mech_max_kg [W·kg⁻¹]",
               "Pedal_Smoothness [%]",
               "P_L_percent [%]",
               "P_R_percent [%]",
               "P_Int_Kinematik [W]",
               "P_Int_Modell [W]",
               "P_Int_Kinematik_Modell [W]",
               "P_Int_Minetti [W]")
)

# Funktion zur Formatierung
format_value <- function(data, var) {
  if(all(is.na(data[[var]]))) {
    return("keine Daten")
  }
  
  if(var %in% c("nD_Vorgabe [U·min⁻¹]", "P_mech_Vorgabe [W]")) {
    sprintf("%.0f ± %.0f", mean(data[[var]], na.rm=TRUE), sd(data[[var]], na.rm=TRUE))
  } else if(var %in% c("P_mech_kg [W·kg⁻¹]", "P_mech_max_kg [W·kg⁻¹]")) {
    sprintf("%.2f ± %.2f", mean(data[[var]], na.rm=TRUE), sd(data[[var]], na.rm=TRUE))
  } else {
    sprintf("%.1f ± %.1f", mean(data[[var]], na.rm=TRUE), sd(data[[var]], na.rm=TRUE))
  }
}

# Berechnung für beide Bedingungen und alle Intensitäten
stats_bed_int <- data.frame(
  Variable = stats$Variable,
  `Sitzen Leicht (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Erg_data_short, Bedingung == "sitzen", Intensität == "leicht")
    format_value(data_subset, var)
  }),
  `Sitzen Moderat (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Erg_data_short, Bedingung == "sitzen", Intensität == "moderat")
    format_value(data_subset, var)
  }),
  `Sitzen Schwer (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Erg_data_short, Bedingung == "sitzen", Intensität == "schwer")
    format_value(data_subset, var)
  }),
  `Stehen Leicht (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Erg_data_short, Bedingung == "stehen", Intensität == "leicht")
    format_value(data_subset, var)
  }),
  `Stehen Moderat (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Erg_data_short, Bedingung == "stehen", Intensität == "moderat")
    format_value(data_subset, var)
  }),
  `Stehen Schwer (MW ± SD)` = sapply(stats$Variable, function(var) {
    data_subset <- filter(Erg_data_short, Bedingung == "stehen", Intensität == "schwer")
    format_value(data_subset, var)
  })
)

# Tabelle erstellen
ft_Ergometer_stats_Bedingung_Intensitaet_mean <- flextable(stats_bed_int) %>%
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
    i = ~ Variable == "nD_Vorgabe [U·min⁻¹]",
    value = as_paragraph("nD", as_sub("Vorgabe"), " [U·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "nD [U·min⁻¹]",
    value = as_paragraph("nD", " [U·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_Vorgabe [W]",
    value = as_paragraph("P", as_sub("mech,Vorgabe"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech [W]",
    value = as_paragraph("P", as_sub("mech"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_kg [W·kg⁻¹]",
    value = as_paragraph("P", as_sub("mech,kg"), " [W·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_abs [W]",
    value = as_paragraph("P", as_sub("mech,abs"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_max [W]",
    value = as_paragraph("P", as_sub("mech,max"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_max_kg [W·kg⁻¹]",
    value = as_paragraph("P", as_sub("mech,max,kg"), " [W·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_L_percent [%]",
    value = as_paragraph("P", as_sub("L,percent"), " [%]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_R_percent [%]",
    value = as_paragraph("P", as_sub("R,percent"), " [%]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_Int_Kinematik [W]",
    value = as_paragraph("P", as_sub("Int,Kinematik"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_Int_Modell [W]",
    value = as_paragraph("P", as_sub("Int,Modell"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_Int_Kinematik_Modell [W]",
    value = as_paragraph("P", as_sub("Int,Kinematik,Modell"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_Int_Minetti [W]",
    value = as_paragraph("P", as_sub("Int,Minetti"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "Torque Efficiency [%]",
    value = as_paragraph("Torque Efficiency [%]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "Pedal_Smoothness [%]",
    value = as_paragraph("Pedal Smoothness [%]")
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
      "nD", as_sub("Vorgabe"), " [U·min⁻¹]: Vorgegebene Trittrate; ",
      "nD", " [U·min⁻¹]: Gemessene Trittrate; ",
      "P", as_sub("mech,Vorgabe"), " [W]: Vorgegebene mechanische Leistung; ",
      "P", as_sub("mech"), " [W]: Gemessene mechanische Leistung; ",
      "P", as_sub("mech,kg"), " [W·kg⁻¹]: Gewichtsspezifische ", "P", as_sub("mech"), "; ",
      "Torque Efficiency [%]: Prozentualer Anteil des effektiven Antriebsmoments am Gesamtdrehmoment der Kurbel; ",
      "P", as_sub("mech,abs"), " [W]: Durchschnittliche absolute mechanische Gesamtleistung (Summation der Beträge von pos. und neg. Leistung); ",
      "P", as_sub("mech,max"), " [W]: Durchschnittliche maximale ", "P", as_sub("mech"), " pro Pedalzyklus; ",
      "P", as_sub("mech,max,kg"), " [W·kg⁻¹]: Gewichtsspezifische P", as_sub("mech,max"), "; ",
      "Pedal Smoothness [%]: Verhältnis von mittlerer zu maximaler Leistung während einer Pedalumdrehung ", "P", as_sub("mech"), "; ",
      "P", as_sub("L,percent"), " [%]: prozentualer Leistungsanteil des linken Beins; ",
      "P", as_sub("R,percent"), " [%]: prozentualer Leistungsanteil des rechten Beins; ",
      "P", as_sub("Int,Kinematik"), " [W]: Interne Leistung basierend auf kinematischer 3D-Bewegungsanalyse; ",
      "P", as_sub("Int,Modell"), " [W]: Interne Leistung basierend auf biomechanischer Modellsimulation; ",
      "P", as_sub("Int,Kinematik,Modell"), " [W]: Modellierte interne Leistung basierend auf mittleren Differenzen zwischen kinematischer und berechneter interner Leistung; ",
      "P", as_sub("Int,Minetti"), " [W]: Interne Leistung nach biomechanischem Berechnungsmodell von Minetti et al. (2001)"
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
ft_Ergometer_stats_Bedingung_Intensitaet_mean <- set_table_properties(
  ft_Ergometer_stats_Bedingung_Intensitaet_mean, 
  width = 1, 
  layout = "autofit"
)

# Anzeigen der Tabelle
ft_Ergometer_stats_Bedingung_Intensitaet_mean

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_Ergometer_stats_Bedingung_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Ergometer_stats_Bedingung_Intensitaet_mean.rds")
ft_Ergometer_stats_Bedingung_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Ergometer_stats_Bedingung_Intensitaet_mean.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_Ergometer_stats_Bedingung_Intensitaet_mean, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Ergometer_stats_Bedingung_Intensitaet_mean.rds")
ft_Ergometer_stats_Bedingung_Intensitaet_mean <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Ergometer_stats_Bedingung_Intensitaet_mean.rds")
