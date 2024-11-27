library(flextable)
library(dplyr)
library(officer)


data <- data.frame(
  Autoren = c(
    "Dickinson & Dickinson (1929)",
    "Henry & DeMoor (1950)", 
    "Seabury et al. (1977)",
    "Suzuki (1979)",
    "Gaesser & Brooks (1975)",
    "Luhtanen et al. (1987)",
    "Chavarren & Calbet (1999)", 
    "Beneke et al. (2002)",
    "Bell & Ferguson (2009)",
    "Harnish et al. (2007)",
    "Millet et al. (2002)",
    "Carlsson et al. (2024)",
    "Bouillod & Pinot (2014)",
    "Dunst et al. (2023)",
    "Zoladz et al. (2023)",
    "Francescato et al. (1995)"
  ),
  Belastungsintensitaet = c(
    "Belastungsdauer: 10 min",
    "113 | 150 \nBelastungsdauer: 6 min",
    "40.8 | 81.7 | 122.6 | 163.4 | 196.1 | 204.2 | 245.1 | 286.0 | 326.8 \nBelastungsdauer: 6 min", 
    "20-80% V̇O₂ₘₐₓ\nBelastungsdauer: 15 min",
    "32.7 | 65.4 | 98.1 | 130.8 \nBelastungsdauer: 6-8 min",
    "50% V̇O₂ₘₐₓ | AerT | AnT | AnT + 30 Watt | AnT + 60 Watt\nBelastungsdauer: 5 min", 
    "54% | 63%| 73% | 80% | 87% | 93% der V̇O₂ₘₐₓ\nBelastungsdauer: 6 min",
    "WAnT\nST: letzte Stufe bei 362 ± 41 Watt",
    "75% der VT",
    "50% | 65% | 75% | 93% des PPO", 
    "75% des PPO\nBelastungsdauer: 6 min",
    "AerT\nBelastungsdauer: 5 min",
    "3.8 | 4.2 | 4.6 [W·kg⁻¹]",
    "3s, 8s, 12s, 60s Maximalsprints",
    "30-120W\nBelastungsdauer: 6 min",
    "20-125W \nBelastungsdauer: 6 min"
  ),
  Trittrate = c(
    "28-111",
    "61",
    "42 | 46 | 50 | 54 | 58 | 59 | 61 | 62 | 64",
    "60 | 100",
    "40 | 60 | 80 | 100",
    "60",
    "60 | 80 | 100 | 120", 
    "WAnT = 80 - 170",
    "45 | 60 | 75 | 90",
    "Sitzen: 74.6-82.4\nStehen: 57.0-65.8",
    "60 | 90",
    "-",
    "-",
    "120",
    "60",
    "40 | 60 | 80 | 100"
  ),
  Teilnehmende = c(
    "N = 1 | \u2640",
    "N = 9 | \u2642",
    "N = 3 | \u2642",
    "N = 6 \n 3ST: 78% ST-Fasern & 3FT: 76% FT-Fasern | 32.0 ± 5.6 Jahre | \u2642\nLN: 2-4\nV̇O₂ₘₐₓ = 60.7 ± 5.7 [l·min⁻¹·kg⁻¹]",
    "N = 12 | 19-24 Jahre | \u2642\n",
    "N = 12 | 21-38 Jahre | \u2642\nLN: 3\nV̇O₂ₘₐₓ = 59.6 ± 2.5 [l·min⁻¹·kg⁻¹]",
    "N = 7 | 22-25 Jahre | \u2642\nLN: 4-5\nV̇O₂ₘₐₓ = 67.77 ± 5.07 [l·min⁻¹·kg⁻¹]",
    "N = 7 | 21.6 ± 3.8 Jahre | \u2642\nLN: 1-3; Sportartfremd: Rugby \nV̇O₂ₘₐₓ = 51.34 ± 9.44 [l·min⁻¹·kg⁻¹]",
    "N = 16 (8 jung / 8 alt) | 19-74 Jahre | \u2640\n",
    "N = 8 | 25.8 ± 7.2 Jahre | 7\u2642 1\u2640\nLN: 4-5\nV̇O₂ₘₐₓ = 68.8 ± 5.0 [l·min⁻¹·kg⁻¹]",
    "N = 8 | 20.7 ± 3.9 Jahre | \u2642",
    "N = 10 | 25 ± 8 Jahre | \u2642\nLN: Elite-National", 
    "N = 13\nLN: Elite",
    "N = 12 | LN: Elite-Bahnradsportler\n3♀: 18.7 ± 4.7 Jahre\n9\u2642: 22.2 ± 4.4 Jahre",
    "N = 17 | 23 ± 3 Jahre | \u2642\nLN: 1-3\nV̇O₂max = 52.3 ± 5.5 [ml·min⁻¹·kg⁻¹]",
    "N = 4 | 22.2 ± 1.5 Jahre | \u2642\nLN: 1\nV̇O₂max = 37.8 ± 4.9 [ml·min⁻¹·kg⁻¹]"
  ),
  Berechnet = c(
    "ηnetto",
    "ηnetto", 
    "ηbrutto",
    "ηbrutto",
    "ηbrutto\nηnetto\nηArbeit\n(ηDelta)",
    "ηbrutto\nηnetto\nηArbeit",
    "ηbrutto\nηDelta",
    "ηnetto",
    "ηnetto\nηmuskulär",
    "ηnetto",
    "ηbrutto",
    "ηbrutto",
    "ηbrutto",
    "ηbrutto",
    "ηbrutto\nηDelta",
    "ηDelta"
  ),
  Wirkungsgrad = c(
    "ηnetto = 19.0 ± 2.8%\n→ Optimale Trittrate bei 33.3 U/min",
    "ηnetto = 21.2% \nηnetto = 19.3% \n→ Effizienzverlust bei höherer Last",
    "→ Optimale Trittrate steigt mit Belastung\n42 U/min bei 40.8W bis 62 U/min bei 326.8W",
    "ST-Gruppe:\nηDelta = 23.3% (60 U/min)\nηDelta = 19.6% (100 U/min)\nFT-Gruppe:\nηDelta = 25.3% (60 U/min)\nηDelta = 28.8% (100 U/min)",
    "η40 = 12-20 | η60 = 12-20 | η80 = 10-19 | η100 = 8-17\nη40 = 19-24 | η60 = 19-24 | η80 = 15-22 | η100 = 10-19\nη40 = 25-26 | η60 = 26-27 | η80 = 26 | η100 = 27-28\nηdelta = 24.4-34.0 \n→ η steigt mit steigender Belastungsintensität",
    "ηbrutto = 19.7 ± 2.8 - 17.4 ± 1.0\nηnetto = 21.8 ± 2.4 - 17.5 ± 0.7\nηArbeit = 29.7 ± 4.2 - 21.0 ± 1.5\n→ η sinkt mit steigender Belastungsintensität",
    "ηbrutto = 13.02 +- 0.4 - ca. 20\n→ ηbrutto steigt mit steigender Belastungsintensität\n→ ηbrutto sinkt mit steigender Drehzahl",
    "ηnetto, WAnT = 16.2 ± 1.6\nηnetto, ST = 24.1 ± 1.7",
    "ηnetto = 15.5-31.5\nηmuskulär = 23.8-39.5",
    "ηnetto, LOW = 24.2-25.1\nηnetto, MED = 25.2-25.7\nηnetto, HIGH = 26.0-26.7\nηnetto, MAX = 31.2 ± 2.3\n→ Keine signifikanten Unterschiede zwischen Seated/Standing",
    "ηbrutto = 22.4 ± 1.5 (CV = 5.6%)\n→ Keine Unterschiede zwischen Level seated, Uphill seated und Uphill standing",
    "ηbrutto = 21.3 ± 1.2 (Seated)\nηbrutto = 21.3 ± 1.1 (Standing)\nηbrutto = 21.0 ± 1.1 (Transitions)\n→ Keine Unterschiede zwischen Positionen",
    "ηbrutto, seated = 20.6 ± 1.1 (CV = 5.5%)\nηbrutto, standing = 21.5 ± 5.2 (CV = 5.2%)\n→ Standing +4.5% höherer Wirkungsgrad",
    "ηbrutto ≈ 10% (3s) → 18.4% (60s)",
    "ηbrutto = 11.6% (30W) → 21.4% (120W)\nηDelta = 29.8 ± 1.9%",
    "ηDelta = 22.9% → 32.0%\n→ steigt mit steigender Trittfrequenz"
  )
)

# Funktion zum Extrahieren der Jahreszahl aus dem Autoren-String
extract_year <- function(author_string) {
  # Extrahiert die Zahl in Klammern mit regulärem Ausdruck
  year <- as.numeric(gsub(".*\\((\\d{4})\\).*", "\\1", author_string))
  return(year)
}

# Sortiere den Datensatz
sort_by_year <- function(data) {
  # Erstelle temporären Vektor mit Jahren
  years <- sapply(data$Autoren, extract_year)
  
  # Sortiere den Datensatz basierend auf den Jahren
  data[order(years),]
}

# Anwenden der Funktion
data_sorted <- sort_by_year(data)

# Erstellen der Flextable
ft_Wirkungsgrad <- flextable(data_sorted) %>%
  set_header_labels(
    Autoren = "Autoren",
    Belastungsintensitaet = "Belastungsintensität",
    Trittrate = "Trittrate [U·min⁻¹]",
    Teilnehmende = "Teilnehmende",
    Berechnet = "Berechneter η",
    Wirkungsgrad = "Wirkungsgrad [%]"
  ) %>%
  compose(
    j = "Berechnet", 
    i = 1, # Dickinson
    value = as_paragraph(
      "η", as_sub("netto")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 2, # Henry & DeMoor
    value = as_paragraph(
      "η", as_sub("netto")
    )
  ) %>%
  compose(
    j = "Berechnet", 
    i = 4, # Seabury
    value = as_paragraph(
      "η", as_sub("brutto")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 5, # Suzuki 
    value = as_paragraph(
      "η", as_sub("Delta")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 3, # Gaesser & Brooks
    value = as_paragraph(
      "η", as_sub("brutto"), "\nη", as_sub("netto"), "\nη", as_sub("Arbeit"), "\nη", as_sub("Delta")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 6, # Luhtanen
    value = as_paragraph(
      "η", as_sub("brutto"), "\nη", as_sub("netto"), "\nη", as_sub("Arbeit")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 7, # Francescato
    value = as_paragraph(
      "η", as_sub("Delta")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 8, # Chavarren
    value = as_paragraph(
      "η", as_sub("brutto"), "\nη", as_sub("Delta")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 9, # Beneke
    value = as_paragraph(
      "η", as_sub("netto")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 10, # Millet
    value = as_paragraph(
      "η", as_sub("brutto")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 11, # Harnish
    value = as_paragraph(
      "η", as_sub("netto")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 12, # Bell
    value = as_paragraph(
      "η", as_sub("netto"), "\nη", as_sub("muskulär")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 13, # Bouillod
    value = as_paragraph(
      "η", as_sub("brutto")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 14, # Dunst
    value = as_paragraph(
      "η", as_sub("brutto")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 15, # Zoladz
    value = as_paragraph(
      "η", as_sub("brutto"), "\nη", as_sub("Delta")
    )
  ) %>%
  compose(
    j = "Berechnet",
    i = 16, # Carlsson
    value = as_paragraph(
      "η", as_sub("brutto")
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 1, # Dickinson
    value = as_paragraph(
      "η", as_sub("netto"), " = 19.0 ± 2.8%\n",
      "→ Optimale Trittrate bei 33.3 U/min"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 2, # Henry & DeMoor
    value = as_paragraph(
      "η", as_sub("netto"), " = 21.2% | ",
      "η", as_sub("netto"), " = 19.3% \n",
      "→ Niedriger ", "η", as_sub("netto"), " bei höherer Last"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 4, # Seabury
    value = as_paragraph(
      "η", as_sub("brutto"), " = 15.4 | η", as_sub("brutto"), " = 19.2 | η", as_sub("brutto"), " = 20.9 | η", as_sub("brutto"), " = 21.9 | η", as_sub("brutto"), " = 21.4 | η", as_sub("brutto"), " = 22.6 | η", as_sub("brutto"), " = 23.0 | η", as_sub("brutto"), " = 23.4 | η", as_sub("brutto"), " = 23.6\n",
      "→ Steigender ", "η", as_sub("brutto"), " bei höherer Last"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 5, # Suzuki
    value = as_paragraph(
      "ST-Gruppe:\n",
      "η", as_sub("Delta, 60"), " = 23.3 ± 0.9\n",
      "η", as_sub("Delta, 100"), " = 19.6 ± 1.6\n",
      "FT-Gruppe:\n",
      "η", as_sub("Delta, 60"), " = 25.3 ± 2.3\n",
      "η", as_sub("Delta, 100"), " = 28.8 ± 1.0\n",
      "→ η", as_sub("Delta"), " sinkt/steigt in ST-Gruppe/FT-Gruppe mit steigender Trittrate"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 3, # Gaesser & Brooks
    value = as_paragraph(
      "η", as_sub("40,brutto"), " = 12-20 | η", as_sub("60,brutto"), " = 12-20 | η", as_sub("80,brutto"), " = 10-19 | η", as_sub("100,brutto"), " = 8-17\n",
      "η", as_sub("40,netto"), " = 19-24 | η", as_sub("60,netto"), " = 19-24 | η", as_sub("80,netto"), " = 15-22 | η", as_sub("100,netto"), " = 10-19\n",
      "η", as_sub("40,Arbeit"), " = 25-26 | η", as_sub("60,Arbeit"), " = 26-27 | η", as_sub("80,Arbeit"), " = 26 | η", as_sub("100,Arbeit"), " = 27-28\n",
      "η", as_sub("Delta"), " = 24.4-34.0\n",
      "→ η steigt mit steigender Belastungsintensität"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 6, # Luhtanen
    value = as_paragraph(
      "η", as_sub("brutto"), " = 17.4 ± 1.0 - 19.7 ± 2.8\n",
      "η", as_sub("netto"), " = 17.5 ± 0.7 - 21.8 ± 2.4\n",
      "η", as_sub("Arbeit"), " = 21.0 ± 1.5 - 29.7 ± 4.2\n",
      "→ η sinkt mit steigender Belastungsintensität"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 7, # Francescato
    value = as_paragraph(
      "η", as_sub("Delta"), " = 22.9 - 32.0\n",
      "→ η", as_sub("delta"), " steigt mit steigender Trittrate"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 8, # Chavarren
    value = as_paragraph(
      "η", as_sub("brutto"), " = 13.0 ± 0.4 - 19.9 ± 0.7\n",
      "η", as_sub("Delta"), " = 21.5 ± 0.8 - 23.9 ± 1.0\n",
      "→ η", as_sub("brutto"), " steigt mit steigender Belastungsintensität\n",
      "→ η", as_sub("brutto"), " sinkt mit steigender Drehzahl\n",
      "→ η", as_sub("Delta"), " steigt mit steigender Drehzahl"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 9, # Beneke
    value = as_paragraph(
      "η", as_sub("netto, WAnT"), " = 16.2 ± 1.6\n",
      "η", as_sub("netto, ST"), " = 24.1 ± 1.7"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 10, # Millet
    value = as_paragraph(
      "η", as_sub("brutto,sitzen"), " = 22.4 ± 0.8\n",
      "η", as_sub("brutto,stehen"), " = 22.2 ± 1.7\n",
      "→ Keine sig. Unterschiede zw. ", "η", as_sub("brutto,stehen"), " und η", as_sub("brutto,sitzen")
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 11, # Harnish
    value = as_paragraph(
      "η", as_sub("netto,sitzen"), " = 24.2 ± 1.7 - 26.0 ± 1.7 \n",
      "η", as_sub("netto,stehen"), " = 25.1 ± 3.7 - 26.7 ± 2.3 \n",
      "→ Keine sig. Unterschiede zw. Sitzen und Stehen\n",
      "→  η", as_sub("netto"), " steigt mit steigender Belastungsintensität"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 12, # Bell
    value = as_paragraph(
      "η", as_sub("netto, jung"), " = 27.5 ± 4.0 | 29.1 ± 5.1\n",
      "η", as_sub("netto, alt"), " = 22.4 ± 6.9 | 20.7 ± 5.5\n",
      "η", as_sub("muskulär, jung"), " = 32.0 ± 3.1 | 34.0 ± 5.5\n",
      "η", as_sub("muskulär, alt"), " = 30.2 ± 3.1 | 27.9 ± 4.1\n",
      "→ Muskeltemp.", as_sub("normal"), " | Muskeltemp.", as_sub("erhöht")
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 13, # Bouillod
    value = as_paragraph(
      "η", as_sub("brutto, sitzen"), " = 20.6 ± 1.1 \n",
      "η", as_sub("brutto, stehen"), " = 21.5 ± 5.2 \n",
      "→ Im Sitzen sig. höherer höherer  ", "η", as_sub("brutto")
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 14, # Dunst
    value = as_paragraph(
      "η", as_sub("brutto"), " = 10.01 ± 2.18 (3s) - 18.42 ± 1.51 (60s)\n",
      "→ Anstieg von ", "η", as_sub("brutto"), "  mit zunehmender Sprintdauer"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 15, # Zoladz
    value = as_paragraph(
      "η", as_sub("brutto"), " = 11.6 ± 1.4 - 21.4 ± 1.1\n",
      "η", as_sub("Delta"), " = 29.8 ± 1.9\n",
      "→ Steigender ", "η", as_sub("brutto"), " bei höherer Last"
    )
  ) %>%
  compose(
    j = "Wirkungsgrad",
    i = 16, # Carlsson
    value = as_paragraph(
      "η", as_sub("brutto, sitzen"), " = 21.3 ± 1.2 \n",
      "η", as_sub("brutto, stehen"), " = 21.3 ± 1.1 \n",
      "η", as_sub("brutto, sitzen und stehen"), " = 21.0 ± 1.1 \n",
      "→ Keine sig. Unterschiede zw. den Bedingungen"
    )
  ) %>%
  theme_zebra(odd_header = "grey92",
              even_header = "#EFEFEF",
              odd_body = "#F9F9F9",
              even_body = "#FFFFFF") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  font(fontname = "Source Sans Pro", part = "all") %>%
  fontsize(size = 12, part = "header") %>%
  fontsize(size = 11, part = "body") %>%
  padding(padding = 4, part = "all") %>%
  border_outer(part = "all", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "lightgrey", width = 0.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "darkgrey", width = 0.5))
ft_Wirkungsgrad <- ft_Wirkungsgrad %>%
  add_footer_row(
    values = as_paragraph(
      "WAnT: Wingate Anaerobic Test; AerT: Aerobe Schwelle; AnT: Anaerobe Schwelle; ST: Stufentest; PPO (Peak Power Output): maximale mechanische Leistung in einem Eingangstest; MAP (Maximal Aerobic Power): höchste über 1 Minute gemittelte Leistung in einem Eingangstes; ",
      "V̇O", as_sub("2max"), " [ml·min", as_sub("-1"), "·kg", as_sub("-1"), "]: maximaler Sauerstoffvolumenstrom; P", as_sub("max"), ": maximale mechanische Leistung; ","VT: Ventilatorische Schwelle; ",
      "LN: Leistungsniveau nach De Pauw et al. (2013) abhängig von der V̇O", as_sub("2max"), " [ml·min", as_sub("-1"), "·kg", as_sub("-1"), "] eingeteilt in LN1: <45, LN2: 45-54.9, LN3: 55-64.9, LN4: 65-71, LN5: >71"
    ),
    colwidths = 6
  ) %>%
  fontsize(size = 10, part = "footer") %>%
  padding(padding = 4, part = "footer") %>%
  border_outer(part = "footer", border = fp_border(color = "darkgrey", width = 0.5)) %>%
  border_inner_h(part = "footer", border = fp_border(color = "lightgrey", width = 0.5))

# Zeilenasbtand
ft_Wirkungsgrad <- ft_Wirkungsgrad %>%
  line_spacing(space = 1.25, part = "body")

# Setzen der Tabelle auf volle Breite
ft_Wirkungsgrad <- set_table_properties(ft_Wirkungsgrad, width = 1, layout = "autofit")

ft_Wirkungsgrad 

# Speichern in ...Probanden_Energieberechnung/xlsm
saveRDS(ft_Wirkungsgrad , "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Wirkungsgrad.rds")
ft_Wirkungsgrad  <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/ft_Wirkungsgrad.rds")

# Speichern in ...Ergebnisse/rds
saveRDS(ft_Wirkungsgrad , "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Wirkungsgrad.rds")
ft_Wirkungsgrad  <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Ergebnisse/rds/ft_Wirkungsgrad.rds")

# Speichern in ...Theoretischer_Hintergrund/rds
saveRDS(ft_Wirkungsgrad , "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Theoretischer_Hintergrund/rds/ft_Wirkungsgrad.rds")
ft_Wirkungsgrad  <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Theoretischer_Hintergrund/rds/ft_Wirkungsgrad.rds")

# Pfad definieren
save_path <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/Theoretischer_Hintergrund/images/"
save_as_image(ft_Wirkungsgrad, path = paste0(save_path, "tabelle_wirkungsgrade.png"), webshot = "webshot2",
              width = 4000, # Breite in Pixeln
              height = 2000) # Höhe in Pixeln
