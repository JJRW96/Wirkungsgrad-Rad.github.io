library(flextable)
library(dplyr)
library(officer)

##### PCr-Werte ######
data <- data.frame(
  Autoren = c(
    "Hultman et al. (1967)", "Karlsson et al. (1971)", "Keul et al. (1972)", "Knuttgen et al. (1973)", "Harris et al. (1974)",
    "Stegemann (1991)", "Gaitanos et al. (1993)", "Bangsbo et al. (1993)","McCann et al. (1995)", "Bogdanis et al. (1996)", "Putman et al. (1998)", "Walter et al. (1999)", "Parolin et al. (1999)",
    "Parolin et al. (2000)", "de Marées (2003); Heck (2006)", "Brooks et al. (2004)", "Nelson & Cox (2012)", "Heck et al. (2022)"
  ),
  PCr_Konzentration = c(
    "16.07 ± 1.66 ≙ (67.8 ± 7.0*)", "15.7 ± 2.15", "10 - 30", "17.9 ± 1.84", "17.89 ± 1.81 ≙ (75.5 ± 7.63*)",
    "16.6 - 21.3 ≙ (70-90*)", "18.13 ± 1.71 ≙ (76.5 ± 7.2*)", "17.5 - 20.3", "23.6 ± 0.98", "17.82 ± 1.04 ≙ (75.2 ± 4.4*)", "20.52 ± 0.81 ≙ (86.6 ± 3.4*)", "37.7 ± 2.8", "20.9 ± 1.1 ≙ (88.2 ± 4.7*)",
    "ca. 19 ≙ (ca. 80*)", "15 - 20",  "28", "30", "20 - 25"
  ),
  Messungsmethode = c(
    "Muskelbiopsie", "Muskelbiopsie", "Literaturangabe", "Muskelbiopsie", "Muskelbiopsie",
    "Literaturangabe", "Muskelbiopsie", "P-MRS","P-MRS","Muskelbiopsie", "Muskelbiopsie", "P-MRS",
    "Muskelbiopsie","Muskelbiopsie", "Literaturangabe", "Literaturangabe", "Literaturangabe",  "Literaturangabe"
  ),
  stringsAsFactors = FALSE
)

# Umbenennen der Spalten
colnames(data)[2] <- "PCr-Konzentration [mmol·kg⁻¹ Trockenmuskelmasse]"
colnames(data)[3] <- "Bestimmungsmethode / Datenbasis"

# Funktion zur Auswahl der ersten verfügbaren Schriftart
select_available_font <- function(font_list) {
  available_fonts <- systemfonts::system_fonts()$family
  for (font in font_list) {
    if (font %in% available_fonts) {
      return(font)
    }
  }
  return("sans-serif")  # Fallback zu einer generischen sans-serif Schriftart
}

# Liste der bevorzugten Schriftarten
preferred_fonts <- c("Source Sans Pro", "Arial", "Helvetica")

# Auswahl der ersten verfügbaren Schriftart
chosen_font <- select_available_font(preferred_fonts)

# Erstellen der Tabelle mit flextable
ft_PCr <- flextable(data) %>%
  theme_zebra(odd_header = "grey92", even_header = "#EFEFEF", odd_body = "#F9F9F9", even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_row(
    values = "*Werte in Klammern zeigen die tatsächlich in der Studie gemessene PCr-Konzentration im Feuchtmuskel. Die Werte außerhalb der Klammern sind auf die Trockenmuskelmasse umgerechnet. Für die Umrechnung wurde nach Putman et al. (1998) ein Verhältnis von 1:4.22 verwendet.",
    colwidths = 3
  ) %>%
  line_spacing(space = 1.1, part = "footer") %>%
  font(fontname = chosen_font, part = "all") %>%
  fontsize(size = 12, part = "header") %>%
  fontsize(size = 11, part = "body") %>%
  fontsize(size = 10, part = "footer") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = officer::fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = officer::fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = officer::fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = officer::fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = officer::fp_border(color = "darkgrey", width = 0.5))

# Setzen der Tabelle auf volle Breite
ft_PCr <- set_table_properties(ft_PCr, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_PCr

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_PCr, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_PCr.rds")
ft_PCr <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_PCr.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_PCr, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_PCr.rds")
ft_PCr <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_PCr.rds")




###############################################################################################

##### EPOC-Werte #####
#Tabelle (angelehnt an @Roberts1978)
data <- data.frame(
  Quelle = c(
    "Hill et al. (1924)",
    "Margaria et al. (1933)",
    "Margaria et al. (1963)",
    "Margaria et al. (1964)",
    "Di Prampero (1971)",
    "Margaria (1972)",
    "Di Prampero et al. (1973)",
    "Shephard (1972)",
    "Katch (1973)",
    "Roberts & Morton (1978)",
    "Beneke et al. (2002)",
    "Beneke et al. (2004)",
    "Francescato et al. (2003)",
    "Dunst et al. (2023a)",
    "Dunst et al. (2023b)",
    "Langley (2024)"
  ),
  
  EPO_fast_ml = c(
    "-", "36.8", "-", "-", "45.0", "40.0", "32.0", "-", 
    "-", "37.45", "-", "32.13 ± 2.59",
    "Sehr Leicht: 11.72 ± 2.4\nLeicht: 20.72 ± 3.4\nModerat: 32.73 ± 3.5",
    "3s: 18.77 ± 4.56\n8s: 26.60 ± 6.00\n12s: 29.47 ± 6.13\n60s: 33.25 ± 7.43", 
    "3s: 16.96 ± 3.68\n8s: 24.78 ± 2.01\n12s: 27.34 ± 5.13\n60s: 28.68 ± 5.36", 
    "10s: 63.71 ± 16.06\n15s: 68.00 ± 17.48\n30s: 78.34 ± 15.02"
  ),
  
  EPO_fast_L = c(
    "-", "2.50", "-", "2.77", "-", "-", "-", "-",
    "-", "2.79", "1.904 ± 0.563", "2.48 ± 0.20", 
    "-",
    "3s: 1.44 ± 0.35\n8s: 2.04 ± 0.46\n12s: 2.26 ± 0.47\n60s: 2.55 ± 0.57", 
    "3s: 1.52 ± 0.33\n8s: 2.22 ± 0.18\n12s: 2.45 ± 0.46\n60s: 2.57 ± 0.48",
    "10s: 4.92 ± 1.24\n15s: 5.25 ± 1.35\n30s: 6.05 ± 1.16"
  ),
  
  EPO_ges_ml = c(
    "-", "-", "75.0", "-", "-", "-", "-", "67.0",
    "68.0", "70.0", "-", "-",
    "Sehr Leicht: 29.2 ± 5.9\nLeicht: 51.6 ± 8.4\nModerat: 81.5 ± 8.8",
    "-", "-", "-"
  ),
  
  EPO_ges_L = c(
    "7.5", "-", "4.65", "-", "-", "-", "-", "5.00",
    "4.89", "4.93", "-", "-",
    "-",
    "-", "-","-"
  ),
  
  stringsAsFactors = FALSE
)

# Erstellen der Tabelle
ft_EPOC <- flextable(data) %>%
  set_caption("Angaben zu EPOC-Mengen in der Fachliteratur") %>%
  set_header_labels(
    Quelle = "Quelle",
    EPO_fast_ml = "",
    EPO_fast_L = "",
    EPO_ges_ml = "",
    EPO_ges_L = ""
  ) %>%
  compose(
    part = "header",
    j = 2,
    value = as_paragraph("EPOC", as_sub("fast"), " [ml·kg⁻¹]")
  ) %>%
  compose(
    part = "header",
    j = 3,
    value = as_paragraph("EPOC", as_sub("fast"), " [l]")
  ) %>%
  compose(
    part = "header",
    j = 4,
    value = as_paragraph("EPOC", as_sub("ges"), " [ml·kg⁻¹]")
  ) %>%
  compose(
    part = "header",
    j = 5,
    value = as_paragraph("EPOC", as_sub("ges"), " [l]")
  ) %>%
  theme_zebra(odd_header = "grey92", even_header = "#EFEFEF", odd_body = "#F9F9F9", even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_row(
    values = as_paragraph(
      "EPOC", as_sub("ges"), " [ml·kg⁻¹]: auf Körpermasse normierte EPOC-Werte; ",
      "EPOC", as_sub("ges"), " [l]: EPOC der gesamten Nachbelstung; ",
      "EPOC", as_sub("fast"), " [ml·kg⁻¹]: auf Körpermasse normierte EPOC die der Rephosphorylierung von PCr zuzuordnen ist; ",
      "EPOC", as_sub("fast"), " [l]: EPOC die der Rephosphorylierung von PCr zuzuordnen ist; ",
      "Zeitangaben (3s, 8s, 10s, 12s, 15s, 30s, 60s): Dauer maximaler Sprints auf dem Radergometer; ",
      "Intensitätsangaben: Belastungsintensitäten (sehr leicht, leicht, moderat)"
    ),
    colwidths = 5
  ) %>%
  line_spacing(space = 1.1, part = "footer") %>%
  font(fontname = "Source Sans Pro", part = "all") %>%
  fontsize(size = 12, part = "header") %>%
  fontsize(size = 11, part = "body") %>%
  fontsize(size = 10, part = "footer") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5))

# Setzen der Tabelle auf volle Breite
ft_EPOC <- set_table_properties(ft_EPOC, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_EPOC

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_EPOC, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC.rds")
ft_EPOC <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_EPOC, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC.rds")
ft_EPOC <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC.rds")


#################################################################################################

##### EPOC-Modellparameter #####
#Tabelle angepasst und erweitert nach Katch et al. 1972
data <- data.frame(
  Quelle = c(
    "Margaria et al. (1933)",
    "Berg (1947)",
    "Henry & DeMoor (1950)",
    "Henry & Berg (1950)",
    "Henry et al. (1951)",
    "DeMoor (1954)",
    "Henry & DeMoor (1956)",
    "Royce (1969)",
    "Katch et al. (1972)",
    "Di Prampero et al. (1973)",
    "Katch (1973)",
    "Roberts & Morton (1978)",
    "Di Prampero (1981)",
    "Özyener et al. (2001)",
    "Beneke et al. (2002)",
    "Dunst et al. (2023a)",
    "Dunst et al. (2023b)",
    "Langley et al. (2024)"
  ),
  
  A = c(
    "-", "-", "-", "-", "-", "-", "-", "-", "-",
    "-", "2.80", "2.330", "-",
    "-", "2.777 ± 0.445", "2.49 ± 0.62",
    "3s: 1.99 ± 0.44\n8s: 2.74 ± 0.38\n12s: 2.88 ± 0.50\n60s: 3.03 ± 0.45",
    "-"
  ),
  
  tauA = c(
    "≈ 0.60", "0.75", "0.90 - 0.94", "0.66 - 0.71", "1.08 - 1.16", 
    "0.88", "1.04", "1.31", "0.66",
    "≈ 0.72", "0.69", "0.66", "0.60 - 0.72",
    "M: 0.48 ± 0.10\nH: 0.70 ± 0.18\nVH: 0.55 ± 0.08\nS: 0.58 ± 0.18",
    "0.70 ± 0.20", "0.84 ± 0.13",
    "3s: 0.82 ± 0.14\n8s: 0.84 ± 0.12\n12s: 0.84 ± 0.13\n60s: 0.85 ± 0.13",
    "-"
  ),
  
  B = c(
    "-", "-", "-", "-", "-", "-", "-", "-", "-",
    "-", "1.443", "0.367", "-",
    "-", "0.675 ± 0.257", "0.51 ± 0.34",
    "3s: 0.20 ± 0.17\n8s: 0.42 ± 0.13\n12s: 0.58 ± 0.12\n60s: 0.98 ± 0.30",
    "-"
  ),
  
  tauB = c(
    "≈ 50.00", "-", "6.94 - 8.00", "-", "-",
    "5.65", "5.92", "23.81", "14.71",
    "21.70 - 28.90", "16.94", "6.87", "21.65",
    "VH: 7.67 ± 2.05\nS: 8.98 ± 6.32", "14.10 ± 13.50", "7.34 ± 3.67",
    "3s: 5.72 ± 2.52\n8s: 6.46 ± 3.38\n12s: 8.03 ± 3.10\n60s: 9.36 ± 5.11",
    "-"
  ),
  
  W_PCR = c(
    "-", "-", "-", "-", "-", "-", "-", "-", "-",
    "...", "-", "-", "...",
    "-", "40.2 ± 10.6", "3s: 30.10 ± 7.32\n8s: 42.64 ± 9.61\n12s: 47.23 ± 9.82\n60s: 53.30 ± 11.91",
    "3s: 31.83 ± 6.98\n8s: 46.50 ± 3.79\n12s: 51.24 ± 9.52\n60s: 53.73 ± 9.96",
    "10s: 102.93 ± 25.91\n15s: 109.83 ± 28.30\n30s: 126.39 ± 24.25"
  ),
  
  stringsAsFactors = FALSE
)

# Erstellen der Tabelle mit flextable
ft_EPOC_Modell <- flextable(data) %>%
  compose(
    part = "header",
    j = 2,
    value = as_paragraph("A", " [ml·min⁻¹]")
  ) %>%
  compose(
    part = "header",
    j = 3,
    value = as_paragraph("τ", as_sub("A"), " [min]")
  ) %>%
  compose(
    part = "header",
    j = 4,
    value = as_paragraph("B", " [ml·min⁻¹]")
  ) %>%
  compose(
    part = "header",
    j = 5,
    value = as_paragraph("τ", as_sub("B"), " [min]")
  ) %>%
  compose(
    part = "header",
    j = 6,
    value = as_paragraph("W", as_sub("PCr"), " [kJ]")
  ) %>%
  set_caption("Modellparameter Bi-Exponentialfunktion") %>%
  theme_zebra(odd_header = "grey92", even_header = "#EFEFEF", odd_body = "#F9F9F9", even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  add_footer_row(
    values = as_paragraph(
      "A [ml·min⁻¹]: Amplitude der schnellen EPOC-Komponente; τA [min]: Zeitkonstante der schnellen EPOC-Komponente; ",
      "B [ml·min⁻¹]: Amplitude der langsamen EPOC-Komponente; τB [min]: Zeitkonstante der langsamen EPOC-Komponente; ",
      "W~PCr [kJ]: Berechnete anaerobe- alaktazide Energiekomponente; ",
      "Zeitangaben (3s, 8s, 10s, 12s, 15s, 30s, 60s): Dauer maximaler Sprints auf dem Radergometer; ",
      "Intensitätsangaben: M: moderate, H: heavy, VH: very heavy, S: severe exercise intensities"
    ),
    colwidths = 6
  ) %>%
  line_spacing(space = 1.1, part = "footer") %>%
  font(fontname = "Source Sans Pro", part = "all") %>%
  fontsize(size = 12, part = "header") %>%
  fontsize(size = 11, part = "body") %>%
  fontsize(size = 10, part = "footer") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  hline_top(part = "footer", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  hline_bottom(part = "footer", border = fp_border(color = "darkgrey", width = 0.5))

ft_EPOC_Modell <- set_table_properties(ft_EPOC_Modell, width = 1, layout = "autofit")

# Anzeigen der Tabelle
ft_EPOC_Modell

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_EPOC_Modell, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC_Modell.rds")
ft_EPOC_Modell <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_EPOC_Modell.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_EPOC_Modell, "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC_Modell.rds")
ft_EPOC_Modell <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_EPOC_Modell.rds")
