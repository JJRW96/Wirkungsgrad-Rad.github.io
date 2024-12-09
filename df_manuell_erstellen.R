#EPOC_data_df_short <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/EPOC_data_df_short.rds")
#Bedingungen_data <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Bedingungen_data.rds")

create_dataframe_string <- function(data) {
  # DataFrame Namen aus dem Aufruf extrahieren
  df_name <- deparse(substitute(data))
  
  # Erst alle Spaltennamen speichern 
  col_names <- names(data)
  
  # Spaltennamen ausgeben zur Kontrolle
  print("Spaltennamen:")
  print(col_names)
  
  cat(sprintf("\n%s <- data.frame(\n", df_name))
  
  # Durch alle Spalten iterieren
  for(i in seq_along(data)) {
    # Gespeicherte Spaltennamen verwenden
    cat(sprintf("  `%s` = ", col_names[i]))
    
    # Werte formatieren
    if(is.numeric(data[[i]])) {
      cat("c(", paste(data[[i]], collapse = ", "), ")")
    } else {
      # Direkt beim Erstellen der Zeichenkette trimmen
      formatted_values <- paste(sprintf("\"%s\"", trimws(data[[i]])), collapse = ", ")
      cat("c(", formatted_values, ")")
    }
    
    # Komma hinzufügen wenn nicht die letzte Spalte
    if(i < ncol(data)) {
      cat(",\n")
    } else {
      cat("\n")
    }
  }
  cat("  , check.names = FALSE\n)")
}

# Anwendung auf den Dataframe
#create_dataframe_string(EPOC_data_df_short)
#create_dataframe_string(EPOC_data_df_VO2)
#create_dataframe_string(Bedingungen_data_Wirkungsgrad)
#create_dataframe_string(Bedingungen_data_Shiny_Regression)
#create_dataframe_string(Erg_data_short)

