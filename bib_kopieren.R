# Benötigte Bibliothek laden
library(fs)

# Pfad zur Hauptdatei references.bib
main_bib_path <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io/references.bib"

# Hauptverzeichnis
root_dir <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io"

# Suche nach allen references.bib Dateien in Unterordnern
bib_files <- dir_ls(root_dir, recurse = TRUE, glob = "*references.bib")

# Entferne die Hauptdatei aus der Liste
bib_files <- bib_files[bib_files != main_bib_path]

# Zähler für ersetzte Dateien
replaced_count <- 0

# Ersetze alle gefundenen references.bib Dateien
for (file in bib_files) {
  file.copy(main_bib_path, file, overwrite = TRUE)
  replaced_count <- replaced_count + 1
}

# Ausgabe der Ergebnisse
cat(sprintf("Es wurden %d references.bib Dateien ersetzt:\n", replaced_count))
for (file in bib_files) {
  cat(sprintf("- %s\n", file))
}
