library(fs)

source_path <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm"
target_base_path <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io"

# Globaler Zähler für kopierte/ersetzte Dateien
copied_or_replaced_count <- 0

# Vektor zur Speicherung der Namen der kopierten/ersetzten Dateien
copied_or_replaced_files <- character(0)

copy_or_replace <- function(source_file, target_file) {
  if (!file_exists(target_file) || file_info(source_file)$modification_time > file_info(target_file)$modification_time) {
    file_copy(source_file, target_file, overwrite = TRUE)
    cat("Kopiert oder ersetzt:", basename(source_file), "nach", dirname(target_file), "\n")
    copied_or_replaced_count <<- copied_or_replaced_count + 1
    copied_or_replaced_files <<- unique(c(copied_or_replaced_files, basename(source_file)))
  } else {
    cat("Übersprungen (identisch oder älter):", basename(source_file), "\n")
  }
}

source_rds_files <- dir_ls(source_path, recurse = TRUE, regexp = "\\.rds$")
rds_folders <- dir_ls(target_base_path, recurse = TRUE, type = "directory", regexp = "rds$")

for (folder in rds_folders) {
  for (source_file in source_rds_files) {
    target_file <- file.path(folder, basename(source_file))
    copy_or_replace(source_file, target_file)
  }
}

cat("\nProzess abgeschlossen.\n")
cat("Anzahl der kopierten oder ersetzten .rds-Dateien:", copied_or_replaced_count, "\n\n")

if (length(copied_or_replaced_files) > 0) {
  cat("Namen der kopierten oder ersetzten Dateien:\n")
  cat(paste("- ", copied_or_replaced_files), sep = "\n")
} else {
  cat("Es wurden keine Dateien kopiert oder ersetzt.\n")
}