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

# ANOVA-Funktion für jeden Parameter
perform_anova <- function(var_name, data) {
  # Spezielle Behandlung für bestimmte Variablennamen
  var_clean <- switch(var_name,
                      "A [l·min⁻¹]" = "A [l·min⁻¹]",
                      "TauA [min]" = "TauA [min]",
                      "B [l·min⁻¹]" = "B [l·min⁻¹]",
                      "TauB [min]" = "TauB [min]",
                      "VO2_Referenz [l·min⁻¹]" = "VO2_Referenz [l·min⁻¹]",
                      "R2_off" = "R2_off",
                      "EPOC_fast [l]" = "EPOC_fast [l]",
                      "EPOC_fast [ml·kg⁻¹]" = "EPOC_fast [ml·kg⁻¹]",
                      "EPOC_PCr [l]" = "EPOC_PCr [l]",
                      "EPOC_PCr [ml·kg⁻¹]" = "EPOC_PCr [ml·kg⁻¹]",
                      "O2_Speicher [l]" = "O2_Speicher [l]",
                      "WPCR [kJ]" = "WPCR [kJ]",
                      "WPCR_corrected [kJ]" = "WPCR_corrected [kJ]",
                      "PCr_used [mmol·kg⁻¹]" = "PCr_used [mmol·kg⁻¹]"
  )
  
  # Durchführung der ANOVA
  result <- try({
    aov_result <- aov(data[[var_clean]] ~ Bedingung, data = data)
    p_value <- summary(aov_result)[[1]][["Pr(>F)"]][1]
    
    # Signifikanzniveau bestimmen
    if (p_value < 0.001) "***"
    else if (p_value < 0.01) "**"
    else if (p_value < 0.05) "*"
    else ""
  }, silent = TRUE)
  
  if (inherits(result, "try-error")) {
    return("")
  } else {
    return(result)
  }
}

# Signifikanzen berechnen
stats$Signifikanz <- sapply(stats$Variable, function(var) {
  perform_anova(var, EPOC_data_df_short)
})

# Berechnung für Stehen mit Signifikanzmarkierung
stats$`Stehen (MW ± SD)` <- sapply(1:nrow(stats), function(i) {
  var <- stats$Variable[i]
  var_name <- gsub(" \\[min\\]", " [s]", var)
  data_stand <- filter(EPOC_data_df_short, Bedingung == "stehen")
  
  mean_sd <- if(var %in% c("TauA [min]", "TauB [min]")) {
    format_mean_sd(data_stand, var_name, 2)
  } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
    format_mean_sd(data_stand, var_name, 3)
  } else {
    format_mean_sd(data_stand, var_name, 2)
  }
  
  paste0(mean_sd, stats$Signifikanz[i])
})

# Tabelle erstellen
ft_EPOC_stats_Bedingung_mean <- flextable(stats[, -which(names(stats) == "Signifikanz")]) %>%
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



#############################################

library(flextable)
library(dplyr)
library(officer)

# ANOVA-Funktion mit Tukey post-hoc Test
perform_anova <- function(var_name, data) {
  var_clean <- switch(var_name,
                      "A [l·min⁻¹]" = "A [l·min⁻¹]",
                      "TauA [min]" = "TauA [min]",
                      "B [l·min⁻¹]" = "B [l·min⁻¹]",
                      "TauB [min]" = "TauB [min]",
                      "VO2_Referenz [l·min⁻¹]" = "VO2_Referenz [l·min⁻¹]",
                      "R2_off" = "R2_off",
                      "EPOC_fast [l]" = "EPOC_fast [l]",
                      "EPOC_fast [ml·kg⁻¹]" = "EPOC_fast [ml·kg⁻¹]",
                      "EPOC_PCr [l]" = "EPOC_PCr [l]",
                      "EPOC_PCr [ml·kg⁻¹]" = "EPOC_PCr [ml·kg⁻¹]",
                      "O2_Speicher [l]" = "O2_Speicher [l]",
                      "WPCR [kJ]" = "WPCR [kJ]",
                      "WPCR_corrected [kJ]" = "WPCR_corrected [kJ]",
                      "PCr_used [mmol·kg⁻¹]" = "PCr_used [mmol·kg⁻¹]"
  )
  
  result <- try({
    # ANOVA durchführen
    aov_result <- aov(data[[var_clean]] ~ Intensität, data = data)
    
    # Tukey post-hoc Test
    tukey_result <- TukeyHSD(aov_result)
    
    # P-Werte aus Tukey-Test extrahieren
    p_moderat_leicht <- tukey_result$Intensität["moderat-leicht", "p adj"]
    p_schwer_leicht <- tukey_result$Intensität["schwer-leicht", "p adj"]
    p_schwer_moderat <- tukey_result$Intensität["schwer-moderat", "p adj"]
    
    # Signifikanzniveaus für jede Gruppe
    sig_leicht <- if(p_moderat_leicht < 0.05 || p_schwer_leicht < 0.05) {
      if(min(p_moderat_leicht, p_schwer_leicht) < 0.001) "***"
      else if(min(p_moderat_leicht, p_schwer_leicht) < 0.01) "**"
      else "*"
    } else ""
    
    sig_moderat <- if(p_moderat_leicht < 0.05 || p_schwer_moderat < 0.05) {
      if(min(p_moderat_leicht, p_schwer_moderat) < 0.001) "***"
      else if(min(p_moderat_leicht, p_schwer_moderat) < 0.01) "**"
      else "*"
    } else ""
    
    sig_schwer <- if(p_schwer_leicht < 0.05 || p_schwer_moderat < 0.05) {
      if(min(p_schwer_leicht, p_schwer_moderat) < 0.001) "***"
      else if(min(p_schwer_leicht, p_schwer_moderat) < 0.01) "**"
      else "*"
    } else ""
    
    list(leicht = sig_leicht, moderat = sig_moderat, schwer = sig_schwer)
  }, silent = TRUE)
  
  if(inherits(result, "try-error")) {
    return(list(leicht = "", moderat = "", schwer = ""))
  } else {
    return(result)
  }
}

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

# Signifikanzen berechnen
sig_results <- lapply(stats$Variable, function(var) {
  perform_anova(var, EPOC_data_df_short)
})

# Berechnung für leicht mit Signifikanzmarkierung
stats$`Leicht (MW ± SD)` <- sapply(1:nrow(stats), function(i) {
  var <- stats$Variable[i]
  var_name <- gsub(" \\[min\\]", " [s]", var)
  data_light <- filter(EPOC_data_df_short, Intensität == "leicht")
  
  mean_sd <- if(var %in% c("TauA [min]", "TauB [min]")) {
    format_mean_sd(data_light, var_name, 2)
  } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
    format_mean_sd(data_light, var_name, 3)
  } else {
    format_mean_sd(data_light, var_name, 2)
  }
  
  paste0(mean_sd, sig_results[[i]]$leicht)
})

# Berechnung für moderat mit Signifikanzmarkierung
stats$`Moderat (MW ± SD)` <- sapply(1:nrow(stats), function(i) {
  var <- stats$Variable[i]
  var_name <- gsub(" \\[min\\]", " [s]", var)
  data_moderate <- filter(EPOC_data_df_short, Intensität == "moderat")
  
  mean_sd <- if(var %in% c("TauA [min]", "TauB [min]")) {
    format_mean_sd(data_moderate, var_name, 2)
  } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
    format_mean_sd(data_moderate, var_name, 3)
  } else {
    format_mean_sd(data_moderate, var_name, 2)
  }
  
  paste0(mean_sd, sig_results[[i]]$moderat)
})

# Berechnung für schwer mit Signifikanzmarkierung
stats$`Schwer (MW ± SD)` <- sapply(1:nrow(stats), function(i) {
  var <- stats$Variable[i]
  var_name <- gsub(" \\[min\\]", " [s]", var)
  data_heavy <- filter(EPOC_data_df_short, Intensität == "schwer")
  
  mean_sd <- if(var %in% c("TauA [min]", "TauB [min]")) {
    format_mean_sd(data_heavy, var_name, 2)
  } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
    format_mean_sd(data_heavy, var_name, 3)
  } else {
    format_mean_sd(data_heavy, var_name, 2)
  }
  
  paste0(mean_sd, sig_results[[i]]$schwer)
})

# Erstellen der finalen Tabelle ohne Signifikanz-Spalte
final_stats <- stats[, c("Variable", "Leicht (MW ± SD)", "Moderat (MW ± SD)", "Schwer (MW ± SD)")]

# Tabelle erstellen (ohne Signifikanz-Spalte)
ft_EPOC_stats_Intensitaet_mean <- flextable(final_stats) %>%
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


#########################################################################

library(flextable)
library(dplyr)
library(officer)

# ANOVA-Funktion für separate Analysen pro Bedingung
perform_separate_anovas <- function(var_name, data) {
  var_clean <- switch(var_name,
                      "A [l·min⁻¹]" = "A [l·min⁻¹]",
                      "TauA [min]" = "TauA [min]",
                      "B [l·min⁻¹]" = "B [l·min⁻¹]",
                      "TauB [min]" = "TauB [min]",
                      "VO2_Referenz [l·min⁻¹]" = "VO2_Referenz [l·min⁻¹]",
                      "R2_off" = "R2_off",
                      "EPOC_fast [l]" = "EPOC_fast [l]",
                      "EPOC_fast [ml·kg⁻¹]" = "EPOC_fast [ml·kg⁻¹]",
                      "EPOC_PCr [l]" = "EPOC_PCr [l]",
                      "EPOC_PCr [ml·kg⁻¹]" = "EPOC_PCr [ml·kg⁻¹]",
                      "O2_Speicher [l]" = "O2_Speicher [l]",
                      "WPCR [kJ]" = "WPCR [kJ]",
                      "WPCR_corrected [kJ]" = "WPCR_corrected [kJ]",
                      "PCr_used [mmol·kg⁻¹]" = "PCr_used [mmol·kg⁻¹]"
  )
  
  get_sig_symbols <- function(p_value) {
    if(is.na(p_value)) return("")
    if(p_value < 0.001) return("***")
    if(p_value < 0.01) return("**")
    if(p_value < 0.05) return("*")
    return("")
  }
  
  # Separate Analysen für Sitzen und Stehen
  result_sit <- try({
    data_sit <- filter(data, Bedingung == "sitzen")
    aov_sit <- aov(data_sit[[var_clean]] ~ Intensität, data = data_sit)
    tukey_sit <- TukeyHSD(aov_sit)
    
    # P-Werte aus Tukey-Test für Sitzen
    p_mod_leicht_sit <- tukey_sit$Intensität["moderat-leicht", "p adj"]
    p_schwer_leicht_sit <- tukey_sit$Intensität["schwer-leicht", "p adj"]
    p_schwer_mod_sit <- tukey_sit$Intensität["schwer-moderat", "p adj"]
    
    sit_sig <- list(
      leicht = if(p_mod_leicht_sit < 0.05 || p_schwer_leicht_sit < 0.05) get_sig_symbols(min(p_mod_leicht_sit, p_schwer_leicht_sit)) else "",
      moderat = if(p_mod_leicht_sit < 0.05 || p_schwer_mod_sit < 0.05) get_sig_symbols(min(p_mod_leicht_sit, p_schwer_mod_sit)) else "",
      schwer = if(p_schwer_leicht_sit < 0.05 || p_schwer_mod_sit < 0.05) get_sig_symbols(min(p_schwer_leicht_sit, p_schwer_mod_sit)) else ""
    )
  }, silent = TRUE)
  
  result_stand <- try({
    data_stand <- filter(data, Bedingung == "stehen")
    aov_stand <- aov(data_stand[[var_clean]] ~ Intensität, data = data_stand)
    tukey_stand <- TukeyHSD(aov_stand)
    
    # P-Werte aus Tukey-Test für Stehen
    p_mod_leicht_stand <- tukey_stand$Intensität["moderat-leicht", "p adj"]
    p_schwer_leicht_stand <- tukey_stand$Intensität["schwer-leicht", "p adj"]
    p_schwer_mod_stand <- tukey_stand$Intensität["schwer-moderat", "p adj"]
    
    stand_sig <- list(
      leicht = if(p_mod_leicht_stand < 0.05 || p_schwer_leicht_stand < 0.05) get_sig_symbols(min(p_mod_leicht_stand, p_schwer_leicht_stand)) else "",
      moderat = if(p_mod_leicht_stand < 0.05 || p_schwer_mod_stand < 0.05) get_sig_symbols(min(p_mod_leicht_stand, p_schwer_mod_stand)) else "",
      schwer = if(p_schwer_leicht_stand < 0.05 || p_schwer_mod_stand < 0.05) get_sig_symbols(min(p_schwer_leicht_stand, p_schwer_mod_stand)) else ""
    )
  }, silent = TRUE)
  
  if(inherits(result_sit, "try-error") || inherits(result_stand, "try-error")) {
    return(list(
      sitzen_leicht = "", sitzen_moderat = "", sitzen_schwer = "",
      stehen_leicht = "", stehen_moderat = "", stehen_schwer = ""
    ))
  } else {
    return(list(
      sitzen_leicht = result_sit$leicht,
      sitzen_moderat = result_sit$moderat,
      sitzen_schwer = result_sit$schwer,
      stehen_leicht = result_stand$leicht,
      stehen_moderat = result_stand$moderat,
      stehen_schwer = result_stand$schwer
    ))
  }
}

# Berechnung der Statistiken gruppiert nach Bedingung und Intensität
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

# Signifikanzen berechnen
sig_results <- lapply(stats$Variable, function(var) {
  perform_two_way_anova(var, EPOC_data_df_short)
})

# Funktion für die Formatierung der Werte mit Signifikanz
format_with_significance <- function(data_subset, var, sig, precision = 2) {
  var_name <- gsub(" \\[min\\]", " [s]", var)
  
  mean_sd <- if(var %in% c("TauA [min]", "TauB [min]")) {
    format_mean_sd(data_subset, var_name, 2)
  } else if(var %in% c("EPOC_PCr [l]", "O2_Speicher [l]", "EPOC_fast [l]", "VO2_Referenz [l·min⁻¹]")) {
    format_mean_sd(data_subset, var_name, 3)
  } else {
    format_mean_sd(data_subset, var_name, 2)
  }
  
  paste0(mean_sd, sig)
}

# Berechnung für jede Kombination
combinations <- list(
  "Sitzen Leicht (MW ± SD)" = c("sitzen", "leicht", "sitzen_leicht"),
  "Sitzen Moderat (MW ± SD)" = c("sitzen", "moderat", "sitzen_moderat"),
  "Sitzen Schwer (MW ± SD)" = c("sitzen", "schwer", "sitzen_schwer"),
  "Stehen Leicht (MW ± SD)" = c("stehen", "leicht", "stehen_leicht"),
  "Stehen Moderat (MW ± SD)" = c("stehen", "moderat", "stehen_moderat"),
  "Stehen Schwer (MW ± SD)" = c("stehen", "schwer", "stehen_schwer")
)

# Initialisiere stats_bed_int mit der Variable-Spalte
stats_bed_int <- data.frame(Variable = stats$Variable)

# Fülle die restlichen Spalten
for(col_name in names(combinations)) {
  bedingung <- combinations[[col_name]][1]
  intensitaet <- combinations[[col_name]][2]
  sig_key <- combinations[[col_name]][3]
  
  stats_bed_int[[col_name]] <- sapply(1:nrow(stats), function(i) {
    var <- stats$Variable[i]
    data_subset <- filter(EPOC_data_df_short, 
                          Bedingung == bedingung, 
                          Intensität == intensitaet)
    format_with_significance(data_subset, var, sig_results[[i]][[sig_key]])
  })
}

# Tabelle erstellen
ft_EPOC_stats_Bedingung_Intensitaet_mean <- flextable(stats_bed_int) %>%
  set_header_labels(
    Variable = "Parameter",  
    `Sitzen Leicht (MW ± SD)` = "Leicht",
    `Sitzen Moderat (MW ± SD)` = "Moderat", 
    `Sitzen Schwer (MW ± SD)` = "Schwer",
    `Stehen Leicht (MW ± SD)` = "Leicht",
    `Stehen Moderat (MW ± SD)` = "Moderat",
    `Stehen Schwer (MW ± SD)` = "Schwer"
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

# Anzeigen der Tabelle
ft_EPOC_stats_Bedingung_Intensitaet_mean
