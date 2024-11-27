library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(DT)
library(flextable)
library(officer)

# Probanden_Daten 2.2.xlsm einlesen
relative_path <- "Probanden_Daten 2.2.xlsm"
absolute_path <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Daten 2.2.xlsm"

# Funktion, um die Datei zu laden
load_excel <- function(path) {
  read_excel(path, sheet = "Stichprobe_final", range = "A1:AA10", col_names = TRUE)  
}

# Versuche relative path, falls Fehler, dann absolute path
Stichprobe_df <- tryCatch(
  load_excel(relative_path),
  error = function(e) {
    message("Relativer Pfad konnte nicht geladen werden, versuche absoluten Pfad...")
    load_excel(absolute_path)
  }
)

# Spalte 26 aus Stichprobe_df entfernen
Stichprobe_df <- Stichprobe_df %>% select(-26)

# Alle Kommas in den Zellen durch Punkte ersetzen
Stichprobe_df <- Stichprobe_df %>% 
  mutate_all(~gsub(",", ".", .))

# Spalten außer der zweiten in numerische Spalten umwandeln
Stichprobe_df <- Stichprobe_df %>% 
  mutate_if(!colnames(.) == colnames(Stichprobe_df)[2], as.numeric)

# Spalten 8, 10, 13, 15, 17, 19, 21 auf zwei Nachkommastellen runden
Stichprobe_df <- Stichprobe_df %>%
  mutate_at(vars(8, 10, 13, 15, 17, 19, 21, 23), ~round(., 2))

VT_df <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/VT_df.rds")
Drehzahltest_df <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Drehzahltest_df.rds")
Energieanteile_data <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Energieanteile_data.rds")

# DFs zusammenfügen
VT_df$Proband <- as.numeric(VT_df$Proband)
Drehzahltest_df$Proband <- as.numeric(Drehzahltest_df$Proband)
Stichprobe_df$Proband <- as.numeric(Stichprobe_df$Proband)
Testtag_1_df <- VT_df %>%
  # Füge VT_df und Drehzahltest_df zusammen
  full_join(Drehzahltest_df, by = "Proband") %>%
  # Füge Stichprobe_df hinzu
  full_join(Stichprobe_df, by = "Proband")

Testtag_1_df <- Testtag_1_df %>%
  select( -`Niedrig (%)`, -`Moderat (%)`, -`VT1 (%)`, -Dif_VT1_T2, 
         -`Dif (%)...23`, -`VT2 (%)`, -Dif_VT2_T2, -`Dif (%)...27`, -Geschlecht, 
         -`Alter [Jahre]`, -`Größe [cm]`, -`Kurbellänge [mm]`, 
         -`V̇O2max [l/min]`, -`V̇O2max [ml/min/kg]`, -`Pmax,sitzen [Watt]`,
         -`Pmax,sitzen [W/kg]`, -`RPMmax [Umdrehungen/min]`, 
         -`Niedrig_VT1-7,5% Pmax [Watt]`, -`VT1-7,5% Pmax [%]`, 
         -`Moderat_VT1 [Watt]`, -`VT1 [%]`, -`Schwer_VT1/VT2 [Watt]`, 
         -`VT1/VT2 [%]`, -`Sehr schwer_VT2 [Watt]`, -`VT2 [%]`, 
         -`TrainingGesamt [h]`, -`TrainingRad [h]`)

saveRDS(Testtag_1_df, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Testtag_1_df.rds")

# Erstelle temporären Datensatz mit nur den ersten Werten pro Proband
unique_HR_max <- Energieanteile_data %>%
  group_by(Proband) %>%
  slice(1) %>%  # nimmt nur ersten Wert pro Proband
  select(Proband, HR_max) %>%
  ungroup()

# Füge HR_max zu Testtag_1_df hinzu
Testtag_1_df <- Testtag_1_df %>%
  left_join(unique_HR_max, by = "Proband")


#################################################################################################

# Erstelle Drehzahltest_df mit den ausgewählten Spalten
Drehzahltest_df <- Testtag_1_df %>%
  select(Proband, T1_VO2_abs, T1_VO2_rel, nD_DT_max, P_Int_Modell_DT_max, 
         VO2_DT_max, VCO2_DT_max, HR_DT_max, nD_DT_60, P_Int_Modell_DT_60, 
         VO2_DT_60, VCO2_DT_60,η_DT_netto, η_netto_DT_60, O2_Cost_of_Work_DT_60, 
         O2_Cost_of_Work_DT_Modell, nD_sitzen_Vorgabe, O2_Cost_of_nD_sitzen_Vorgabe_l, HR_max)

Drehzahltest_df$VO2_DT_percent <- (Drehzahltest_df$VO2_DT_max / Drehzahltest_df$T1_VO2_abs) * 100
Drehzahltest_df$η_DT_netto <- Drehzahltest_df$η_DT_netto *  100

names(Drehzahltest_df)[names(Drehzahltest_df) == "nD_DT_max"] <- "nD_DT_max [min⁻¹]"
names(Drehzahltest_df)[names(Drehzahltest_df) == "nD_DT_60"] <- "nD_DT_60 [min⁻¹]"
names(Drehzahltest_df)[names(Drehzahltest_df) == "P_Int_Modell_DT_max"] <- "P_Int_Modell_DT_max [W]"
names(Drehzahltest_df)[names(Drehzahltest_df) == "P_Int_Modell_DT_60"] <- "P_Int_Modell_DT_60 [W]"
names(Drehzahltest_df)[names(Drehzahltest_df) == "VO2_DT_percent"] <- "VO2_DT_percent [%]"
names(Drehzahltest_df)[names(Drehzahltest_df) == "HR_DT_max"] <- "HR_DT_max [min⁻¹]"
names(Drehzahltest_df)[names(Drehzahltest_df) == "HR_max"] <- "HR_ST_max [min⁻¹]"
names(Drehzahltest_df)[names(Drehzahltest_df) == "η_DT_netto"] <- "η_DT_netto [%]"
names(Drehzahltest_df)[names(Drehzahltest_df) == "O2_Cost_of_Work_DT_Modell"] <- "O2_Cost_of_Work_DT_Modell [ml·min⁻¹·W⁻¹]"
names(Drehzahltest_df)[names(Drehzahltest_df) == "O2_Cost_of_nD_sitzen_Vorgabe_l"] <- "O2_Cost_nD_Vorgabe [l·min⁻¹]"

#######
# Gruppierte Variablen nach Dezimalstellen
digits_0 <- c("Proband")

digits_1 <- c("nD_DT_max [min⁻¹]",
              "nD_DT_60 [min⁻¹]",
              "P_Int_Modell_DT_max [W]",
              "P_Int_Modell_DT_60 [W]",
              "VO2_DT_percent [%]",
              "HR_DT_max [min⁻¹]",
              "HR_ST_max [min⁻¹]")

digits_2 <- c("η_DT_netto [%]",
              "O2_Cost_of_Work_DT_Modell [ml·min⁻¹·W⁻¹]")

digits_3 <- c("O2_Cost_nD_Vorgabe [l·min⁻¹]")

# Auswählen der gewünschten Spalten
selected_columns <- c(digits_0, digits_1, digits_2, digits_3)
Drehzahltest_df_selected <- Drehzahltest_df[, selected_columns]

# Datentabelle erstellen und formatieren
DT_table <- datatable(Drehzahltest_df_selected,
                      options = list(
                        pageLength = 10,
                        scrollX = TRUE,
                        scrollCollapse = TRUE,
                        autoWidth = FALSE,
                        rownames = FALSE,
                        columnDefs = list(
                          list(targets = '_all', className = 'dt-nowrap')
                        )
                      )) %>%
  formatRound(columns = digits_0, digits = 0) %>%
  formatRound(columns = digits_1, digits = 1) %>%
  formatRound(columns = digits_2, digits = 2) %>%
  formatRound(columns = digits_3, digits = 3)

DT_table

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(DT_table, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/DT_table.rds")
DT_table <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/DT_table.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(DT_table, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/DT_table.rds")
DT_table<- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/DT_table.rds")

#######
# Berechnung der Statistiken
stats <- data.frame(
  Variable = c("nD_DT_max [min⁻¹]",
               "nD_DT_60 [min⁻¹]",
               "P_Int_Modell_DT_max [W]", 
               "P_Int_Modell_DT_60 [W]",
               "VO2_DT_percent [%]",
               "HR_DT_max [min⁻¹]",
               "HR_ST_max [min⁻¹]",
               "η_DT_netto [%]",
               "O2_Cost_of_Work_DT_Modell [ml·min⁻¹·W⁻¹]",
               "O2_Cost_nD_Vorgabe [l·min⁻¹]")
)

# Berechnung von Mittelwert und SD
stats$`Mittelwert ± SD` <- paste(
  case_when(
    stats$Variable %in% c("O2_Cost_nD_Vorgabe [l·min⁻¹]") ~ 
      sprintf("%.3f ± %.3f", 
              sapply(Drehzahltest_df[stats$Variable], mean, na.rm=TRUE),
              sapply(Drehzahltest_df[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable %in% c("O2_Cost_of_Work_DT_Modell [ml·min⁻¹·W⁻¹]") ~
      sprintf("%.3f", 
              sapply(Drehzahltest_df[stats$Variable], mean, na.rm=TRUE)),
    stats$Variable %in% c("η_DT_netto [%]") ~
      sprintf("%.2f ± %.2f",
              sapply(Drehzahltest_df[stats$Variable], mean, na.rm=TRUE),
              sapply(Drehzahltest_df[stats$Variable], sd, na.rm=TRUE)),
    stats$Variable %in% c("VO2_DT_percent [%]") ~
      sprintf("%.1f ± %.1f",
              sapply(Drehzahltest_df[stats$Variable], mean, na.rm=TRUE), 
              sapply(Drehzahltest_df[stats$Variable], sd, na.rm=TRUE)),
    TRUE ~
      sprintf("%.1f ± %.1f",
              sapply(Drehzahltest_df[stats$Variable], mean, na.rm=TRUE),
              sapply(Drehzahltest_df[stats$Variable], sd, na.rm=TRUE))
  )
)

# Minimum und Maximum 
stats$Minimum <- case_when(
  stats$Variable %in% c("O2_Cost_nD_Vorgabe [l·min⁻¹]") ~
    sprintf("%.3f", sapply(Drehzahltest_df[stats$Variable], min, na.rm=TRUE)),
  stats$Variable %in% c("O2_Cost_of_Work_DT_Modell [ml·min⁻¹·W⁻¹]") ~ "-",
  stats$Variable %in% c("η_DT_netto [%]") ~
    sprintf("%.2f", sapply(Drehzahltest_df[stats$Variable], min, na.rm=TRUE)),  
  stats$Variable %in% c("VO2_DT_percent [%]") ~
    sprintf("%.1f", sapply(Drehzahltest_df[stats$Variable], min, na.rm=TRUE)),
  TRUE ~
    sprintf("%.1f", sapply(Drehzahltest_df[stats$Variable], min, na.rm=TRUE))
)

stats$Maximum <- case_when(
  stats$Variable %in% c("O2_Cost_nD_Vorgabe [l·min⁻¹]") ~
    sprintf("%.3f", sapply(Drehzahltest_df[stats$Variable], max, na.rm=TRUE)),
  stats$Variable %in% c("O2_Cost_of_Work_DT_Modell [ml·min⁻¹·W⁻¹]") ~ "-",
  stats$Variable %in% c("η_DT_netto [%]") ~
    sprintf("%.2f", sapply(Drehzahltest_df[stats$Variable], max, na.rm=TRUE)),
  stats$Variable %in% c("VO2_DT_percent [%]") ~
    sprintf("%.1f", sapply(Drehzahltest_df[stats$Variable], max, na.rm=TRUE)),
  TRUE ~
    sprintf("%.1f", sapply(Drehzahltest_df[stats$Variable], max, na.rm=TRUE))
)

# Nach allen Berechnungen
names(stats) <- c("Parameter", "Mittelwert ± SD", "Min", "Max")

# Tabelle erstellen
ft_DT_stats <- flextable(stats) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "nD_DT_max [min⁻¹]",
    value = as_paragraph("nD", as_sub("DT,max"), " [min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "nD_DT_60 [min⁻¹]",
    value = as_paragraph("nD", as_sub("DT,60"), " [min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Modell_DT_max [W]",
    value = as_paragraph("P", as_sub("Int,Modell,DT,max"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "P_Int_Modell_DT_60 [W]",
    value = as_paragraph("P", as_sub("Int,Modell,DT,60"), " [W]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "VO2_DT_percent [%]",
    value = as_paragraph("V̇O", as_sub("2,DT,percent"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "HR_DT_max [min⁻¹]",
    value = as_paragraph("HR", as_sub("DT,max"), " [min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "HR_ST_max [min⁻¹]",
    value = as_paragraph("HR", as_sub("ST,max"), " [min⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "η_DT_netto [%]",
    value = as_paragraph("η", as_sub("DT,netto"), " [%]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "O2_Cost_of_Work_DT_Modell [ml·min⁻¹·W⁻¹]",
    value = as_paragraph("O", as_sub("2"), "-Cost of Work", as_sub("DT,Modell"), " [ml·min⁻¹·W⁻¹]")
  ) %>%
  compose(
    j = "Parameter",
    i = ~ Parameter == "O2_Cost_nD_Vorgabe [l·min⁻¹]",
    value = as_paragraph("O", as_sub("2"), "-Cost", as_sub("nD,Vorgabe"), " [l·min⁻¹]")
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
      "nD", as_sub("DT,max"), " [min⁻¹]: Maximale Drehzahl im DT; ",
      "nD", as_sub("DT,60"), " [min⁻¹]: Durchschnittliche Drehzahl in den letzten 60s des DT; ",
      "P", as_sub("Int,Modell,DT,max"), " [W]: Maximale interne Leistung während des DT basierend auf biomechanischer Modellsimulation; ",
      "P", as_sub("Int,Modell,DT,60"), " [W]: Durchschnittliche interne Leistung der letzten 60s des DT; ",
      "V̇O", as_sub("2,DT,percent"), " [%]: Prozentuale Auslastung der V̇O", as_sub("2"), " im DT im Vergleich zur maximalen V̇O", as_sub("2"), " im Stufentest; ",
      "HR", as_sub("DT,max"), " [min⁻¹]: Maximale Herzrate im DT; ",
      "HR", as_sub("ST,max"), " [min⁻¹]: Maximale Herzrate im ST; ",
      "η", as_sub("DT,netto"), " [%]: Netto-Wirkungsgrad im Drehzahltest ab nD > 80; ",
      "O", as_sub("2"), "-Cost of Work", as_sub("DT,Modell"), " [ml·min⁻¹·W⁻¹]: O₂-Umsatz pro Watt P", as_sub("Int,Modell"), " im DT; ",
      "O", as_sub("2"), "-Kosten Leerbewegung n", as_sub("D,Vorgabe"), " [l·min⁻¹]: O₂-Umsatz der Leerbewegung bei vorgegebener nD an Testtag 2 für die Berechnung von η", as_sub("Arbeit")
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
ft_DT_stats <- set_table_properties(ft_DT_stats, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_DT_stats

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_DT_stats, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_DT_stats.rds")
ft_DT_stats <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_DT_stats.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_DT_stats, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_DT_stats.rds")
ft_DT_stats<- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_DT_stats.rds")


###############################################################################################################
Testtag_1_df$P_max_stehen_kg <- Testtag_1_df$`Pmax(stehen)` / Testtag_1_df$`Gewicht [kg]`

# Erstelle Stufentest_df mit den ausgewählten Spalten
Stufentest_df <- Testtag_1_df %>%
  select(Proband, Pmax_sitzen, `Pmax/kg`, T1_VO2_abs, T1_VO2_rel, 
         T2_VO2_abs, T2_VO2_rel, Niedrig_T1, Moderat_T1, Schwer_T1, VT1, VT2, 
         O2_Cost_of_Work_ST, `RPMsitzen [Umdrehungen/min]`, 
         `RPMstehen [Umdrehungen/min]`, HR_max, `Pmax(stehen)`,P_max_stehen_kg)

# Umbenennen der Spalten
names(Stufentest_df)[names(Stufentest_df) == "Pmax_sitzen"] <- "P_mech_max_sitzen [W]"
names(Stufentest_df)[names(Stufentest_df) == "Pmax/kg"] <- "P_mech_max_sitzen_kg [W·kg⁻¹]"
names(Stufentest_df)[names(Stufentest_df) == "Pmax(stehen)"] <- "P_mech_max_stehen [W]"
names(Stufentest_df)[names(Stufentest_df) == "P_max_stehen_kg"] <- "P_mech_max_stehen_kg [W·kg⁻¹]"
names(Stufentest_df)[names(Stufentest_df) == "T1_VO2_abs"] <- "ST1_VO2_max_abs [l·min⁻¹]"
names(Stufentest_df)[names(Stufentest_df) == "T1_VO2_rel"] <- "ST1_VO2_max_rel [ml·min⁻¹·kg⁻¹]"
names(Stufentest_df)[names(Stufentest_df) == "T2_VO2_abs"] <- "ST2_VO2_max_abs [l·min⁻¹]"
names(Stufentest_df)[names(Stufentest_df) == "T2_VO2_rel"] <- "ST2_VO2_max_rel [ml·min⁻¹·kg⁻¹]"
names(Stufentest_df)[names(Stufentest_df) == "Niedrig_T1"] <- "P_mech_Vorgabe_leicht [W]"
names(Stufentest_df)[names(Stufentest_df) == "Moderat_T1"] <- "P_mech_Vorgabe_moderat [W]"
names(Stufentest_df)[names(Stufentest_df) == "Schwer_T1"] <- "P_mech_Vorgabe_schwer [W]"
names(Stufentest_df)[names(Stufentest_df) == "RPMsitzen [Umdrehungen/min]"] <- "nD_Vorgabe_sitzen [U·min⁻¹]"
names(Stufentest_df)[names(Stufentest_df) == "RPMstehen [Umdrehungen/min]"] <- "nD_Vorgabe_stehen [U·min⁻¹]"
names(Stufentest_df)[names(Stufentest_df) == "VT1"] <- "VT1 [W]"
names(Stufentest_df)[names(Stufentest_df) == "VT2"] <- "VT2 [W]"
names(Stufentest_df)[names(Stufentest_df) == "HR_max"] <- "HR_ST_max [min⁻¹]"
names(Stufentest_df)[names(Stufentest_df) == "O2_Cost_of_Work_ST"] <- "O2_Cost_of_Work_ST1 [ml·min⁻¹·W⁻¹]"




#################################################
# Statistiken für Stufentest erstellen
stats_ST <- data.frame(
  Variable = c(
    "P_mech_max_sitzen [W]",
    "P_mech_max_sitzen_kg [W·kg⁻¹]",
    "P_mech_max_stehen [W]",
    "P_mech_max_stehen_kg [W·kg⁻¹]",
    "ST1_VO2_max_abs [l·min⁻¹]",
    "ST1_VO2_max_rel [ml·min⁻¹·kg⁻¹]",
    "ST2_VO2_max_abs [l·min⁻¹]",
    "ST2_VO2_max_rel [ml·min⁻¹·kg⁻¹]",
    "VT1 [W]",
    "VT2 [W]",
    "P_mech_Vorgabe_leicht [W]",
    "P_mech_Vorgabe_moderat [W]",
    "P_mech_Vorgabe_schwer [W]",
    "nD_Vorgabe_sitzen [U·min⁻¹]",
    "nD_Vorgabe_stehen [U·min⁻¹]",
    "HR_ST_max [min⁻¹]",
    "O2_Cost_of_Work_ST1 [ml·min⁻¹·W⁻¹]"
  )
)

# Mittelwert und SD
stats_ST$`Mittelwert ± SD` <- case_when(
  stats_ST$Variable == "O2_Cost_of_Work_ST1 [ml·min⁻¹·W⁻¹]" ~
    sprintf("%.3f", 
            sapply(Stufentest_df[stats_ST$Variable], mean, na.rm=TRUE)),
  stats_ST$Variable %in% c("ST1_VO2_max_abs [l·min⁻¹]", "ST2_VO2_max_abs [l·min⁻¹]") ~
    sprintf("%.3f ± %.3f", 
            sapply(Stufentest_df[stats_ST$Variable], mean, na.rm=TRUE),
            sapply(Stufentest_df[stats_ST$Variable], sd, na.rm=TRUE)),
  TRUE ~
    sprintf("%.1f ± %.1f", 
            sapply(Stufentest_df[stats_ST$Variable], mean, na.rm=TRUE),
            sapply(Stufentest_df[stats_ST$Variable], sd, na.rm=TRUE))
)

# Minimum
stats_ST$Minimum <- case_when(
  stats_ST$Variable %in% c("ST1_VO2_max_abs [l·min⁻¹]", "ST2_VO2_max_abs [l·min⁻¹]") ~
    sprintf("%.3f", sapply(Stufentest_df[stats_ST$Variable], min, na.rm=TRUE)),
  stats_ST$Variable %in% c("O2_Cost_of_Work_ST1 [ml·min⁻¹·W⁻¹]") ~ "-",
  TRUE ~
    sprintf("%.1f", sapply(Stufentest_df[stats_ST$Variable], min, na.rm=TRUE))
)

# Maximum
stats_ST$Maximum <- case_when(
  stats_ST$Variable %in% c("ST1_VO2_max_abs [l·min⁻¹]", "ST2_VO2_max_abs [l·min⁻¹]") ~
    sprintf("%.3f", sapply(Stufentest_df[stats_ST$Variable], max, na.rm=TRUE)),
  stats_ST$Variable %in% c("O2_Cost_of_Work_ST1 [ml·min⁻¹·W⁻¹]") ~ "-",
  TRUE ~
    sprintf("%.1f", sapply(Stufentest_df[stats_ST$Variable], max, na.rm=TRUE))
)

# Tabelle erstellen
ft_ST_stats <- flextable(stats_ST) %>%
  set_header_labels(
    Variable = "Parameter",
    `Mittelwert ± SD` = "Mittelwert ± SD",
    Minimum = "Min",
    Maximum = "Max"
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_max_sitzen [W]",
    value = as_paragraph("P", as_sub("mech,max,sitzen"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_max_sitzen [W]",
    value = as_paragraph("P", as_sub("mech,max,sitzen"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_max_sitzen_kg [W·kg⁻¹]",
    value = as_paragraph("P", as_sub("mech,max,sitzen,kg"), " [W·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_max_stehen [W]",
    value = as_paragraph("P", as_sub("mech,max,stehen"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_max_stehen_kg [W·kg⁻¹]",
    value = as_paragraph("P", as_sub("mech,max,stehen,kg"), " [W·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "ST1_VO2_max_abs [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,max,abs,ST1"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "ST1_VO2_max_rel [ml·min⁻¹·kg⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,max,rel,ST1"), " [ml·min⁻¹·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "ST2_VO2_max_abs [l·min⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,max,abs,ST2"), " [l·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "ST2_VO2_max_rel [ml·min⁻¹·kg⁻¹]",
    value = as_paragraph("V̇O", as_sub("2,max,rel,ST2"), " [ml·min⁻¹·kg⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_Vorgabe_leicht [W]",
    value = as_paragraph("P", as_sub("mech,Vorgabe,leicht"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_Vorgabe_moderat [W]",
    value = as_paragraph("P", as_sub("mech,Vorgabe,moderat"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "P_mech_Vorgabe_schwer [W]",
    value = as_paragraph("P", as_sub("mech,Vorgabe,schwer"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "VT1 [W]",
    value = as_paragraph("VT", as_sub("1"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "VT2 [W]",
    value = as_paragraph("VT", as_sub("2"), " [W]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "nD_Vorgabe_sitzen [U·min⁻¹]",
    value = as_paragraph("nD", as_sub("Vorgabe,sitzen"), " [U·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "nD_Vorgabe_stehen [U·min⁻¹]",
    value = as_paragraph("nD", as_sub("Vorgabe,stehen"), " [U·min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "HR_ST_max [min⁻¹]",
    value = as_paragraph("HR", as_sub("ST,max"), " [min⁻¹]")
  ) %>%
  compose(
    j = "Variable",
    i = ~ Variable == "O2_Cost_of_Work_ST1 [ml·min⁻¹·W⁻¹]",
    value = as_paragraph("O", as_sub("2"), "-Cost of Work", as_sub("ST1"), " [ml·min⁻¹·W⁻¹]")
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
      "P", as_sub("mech,max,sitzen"), " [W]: Maximale mechanische Leistung der letzten vollständig absolvierten Stufe im ST1; ",
      "P", as_sub("mech,max,sitzen,kg"), " [W·kg⁻¹]: Relative maximale mechanische Leistung der letzten vollständig absolvierten Stufe im ST1; ",
      "P", as_sub("mech,max,stehen"), " [W]: ", "P", as_sub("mech,max"), " der letzten vollständig absolvierten Stufe im ST2; ",
      "P", as_sub("mech,max,stehen,kg"), " [W·kg⁻¹]: ", "P", as_sub("mech,max,kg"), " der letzten vollständig absolvierten Stufe im ST2; ",
      "V̇O", as_sub("2,max,abs"), " ST1 [l·min⁻¹]: Absolute maximale V̇O", as_sub("2"), " im ST1; ",
      "V̇O", as_sub("2,max,abs"), " ST1 [l·min⁻¹]: Relative maximale V̇O", as_sub("2"), " im ST1; ",
      "V̇O", as_sub("2,max,abs"), " ST2 [l·min⁻¹]: Absolute maximale V̇O", as_sub("2"), " im ST2; ",
      "V̇O", as_sub("2,max,abs"), " ST2 [l·min⁻¹]: Relative maximale V̇O", as_sub("2"), " im ST2; ",
      "P", as_sub("mech,Vorgabe,leicht"), " [W]: Vorgegebene ", "P", as_sub("mech"), " der leichten Belastungsintensität an Testtag 2; ",
      "P", as_sub("mech,Vorgabe,moderat"), " [W]: Vorgegebene ", "P", as_sub("mech"), " der moderaten Belastungsintensität an Testtag 2; ",
      "P", as_sub("mech,Vorgabe,schwer"), " [W]: Vorgegebene ", "P", as_sub("mech"), " der schweren Belastungsintensität an Testtag 2; ",
      "VT", as_sub("1"), " [W]: Bestimmte erste ventilatorische Schwelle im ST1; ",
      "VT", as_sub("2"), " [W]: Bestimmte zweite ventilatorische Schwelle im ST1; ",
      "nD", as_sub("Vorgabe,sitzen"), " [U·min⁻¹]: Vorgegebene Drehzahl der Belastungen im Sitzen an Testtag 2; ",
      "nD", as_sub("Vorgabe,stehen"), " [U·min⁻¹]: Vorgegebene Drehzahl der Belastungen im Stehen an Testtag 2; ",
      "HR", as_sub("ST,max"), " [min⁻¹]: Maximale Herzrate in beiden Stufentests; ",
      "O", as_sub("2"), "-Cost of Work", as_sub("ST1"), " [ml·min⁻¹·W⁻¹]: Durchschnittlicher O", as_sub("2"), "-Umsatz pro Watt im ST1, berechnet aus allen gemittelten V̇O", as_sub("2"), "-Daten der Probanden"
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
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  set_table_properties(width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_ST_stats

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_ST_stats, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_ST_stats.rds")
ft_ST_stats <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_ST_stats.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_ST_stats, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_ST_stats.rds")
ft_ST_stats<- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_ST_stats.rds")

#################################################

# Gruppierte Variablen nach Dezimalstellen
digits_0 <- c("Proband")

digits_1 <- c("P_mech_max_sitzen [W]",
              "P_mech_max_stehen [W]",
              "P_mech_Vorgabe_leicht [W]",
              "P_mech_Vorgabe_moderat [W]",
              "P_mech_Vorgabe_schwer [W]",
              "VT1 [W]",
              "VT2 [W]",
              "nD_Vorgabe_sitzen [U·min⁻¹]",
              "nD_Vorgabe_stehen [U·min⁻¹]",
              "HR_ST_max [min⁻¹]")

digits_2 <- c("P_mech_max_sitzen_kg [W·kg⁻¹]",
              "P_mech_max_stehen_kg [W·kg⁻¹]",
              "ST1_VO2_max_abs [l·min⁻¹]",
              "ST2_VO2_max_abs [l·min⁻¹]",
              "O2_Cost_of_Work_ST1 [ml·min⁻¹·W⁻¹]")

digits_3 <- c("ST1_VO2_max_rel [ml·min⁻¹·kg⁻¹]",
              "ST2_VO2_max_rel [ml·min⁻¹·kg⁻¹]")

# Auswählen der gewünschten Spalten
selected_columns <- c(digits_0, digits_1, digits_2, digits_3)
Stufentest_df_selected <- Stufentest_df[, selected_columns]

# Datentabelle erstellen und formatieren
ST_table <- datatable(Stufentest_df_selected,
                      options = list(
                        pageLength = 10,
                        scrollX = TRUE,
                        scrollCollapse = TRUE,
                        autoWidth = FALSE,
                        rownames = FALSE,
                        columnDefs = list(
                          list(targets = '_all', className = 'dt-nowrap')
                        )
                      )) %>%
  formatRound(columns = digits_0, digits = 0) %>%
  formatRound(columns = digits_1, digits = 1) %>%
  formatRound(columns = digits_2, digits = 2) %>%
  formatRound(columns = digits_3, digits = 3)

ST_table

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ST_table, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ST_table.rds")
ST_table <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ST_table.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ST_table, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ST_table.rds")
ST_table <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ST_table.rds")
