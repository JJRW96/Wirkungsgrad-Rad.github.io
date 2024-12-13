# Benötigte Pakete
if (!require(fs)) install.packages("fs")
library(fs)

# Pfade definieren
base_dir <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io"
pdf_dir <- file.path(base_dir, "pdf")

# Alle .qmd Dateien im Verzeichnis und Unterverzeichnissen finden
qmd_files <- dir_ls(base_dir, recurse = TRUE, glob = "*.qmd")

# Für jede .qmd Datei
for (qmd_file in qmd_files) {
  # Quarto render Befehl erstellen
  cmd <- sprintf('quarto render "%s" --to pdf --output-dir "%s" -M echo=false --pdf-engine-opt=-buf-size=1000000', 
                 qmd_file,
                 pdf_dir)
  
  # Befehl ausgeben
  cat(cmd, "\n\n")
}