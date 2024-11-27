# Lade erforderliche Bibliotheken
library(stringr)  
library(dplyr)  

# Definiere den Pfad zum Hauptordner
path <- "C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Wirkungsgrad-Rad.github.io"

# Finde alle .qmd Dateien im Hauptordner und allen Unterordnern
files <- list.files(path, pattern = "\\.qmd$", full.names = TRUE, recursive = TRUE)
# list.files(): Listet Dateien auf
# pattern = "\\.qmd$": Sucht nur nach Dateien, die auf .qmd enden
# full.names = TRUE: Gibt vollständige Pfade zurück
# recursive = TRUE: Durchsucht auch alle Unterordner

# Funktion zum Extrahieren von Quellen aus einer Datei
extract_sources <- function(file) {
  content <- readLines(file, warn = FALSE)  # Liest den Inhalt der Datei
  # str_extract_all(): Extrahiert alle Übereinstimmungen des Musters
  # "@[A-Za-z]+[12][0189]\\d{2}": Muster für Quellen (z.B. @Autor1234)
  sources <- unlist(str_extract_all(content, "@[A-Za-z]+[12][0189]\\d{2}"))
  return(sources)
}

# Extrahiere Quellen aus allen Dateien
all_sources <- unique(unlist(lapply(files, extract_sources)))
# lapply(): Wendet extract_sources auf jede Datei an
# unlist(): Flacht die Liste der Ergebnisse zu einem Vektor ab
# unique(): Entfernt Duplikate

# Filtere die Quellen nach den spezifizierten Kriterien
valid_sources <- all_sources[str_detect(all_sources, "^@[A-Za-z]+(18|19|20)\\d{2}$")]
# str_detect(): Prüft, ob der String dem Muster entspricht
# "^@[A-Za-z]+(18|19|20)\\d{2}$": Muster für gültige Quellen (18xx, 19xx, 20xx)

# Sortiere die Quellen
sorted_sources <- valid_sources %>%
  tibble(source = .) %>%  # Erstellt einen Dataframe mit einer Spalte 'source'
  mutate(
    # Extrahiert den Autorennamen (nur @Buchstaben)
    author = str_extract(source, "@[A-Za-z]+"),
    # Extrahiert das Jahr und konvertiert zu einer Zahl
    year = as.numeric(str_extract(source, "\\d{4}")),
    # Erstellt eine neue Spalte für die Sortierung (Autor in Kleinbuchstaben)
    sort_author = tolower(author)
  ) %>%
  arrange(sort_author, year) %>%  # Sortiert nach Autor (Kleinbuchstaben) und Jahr
  pull(source)  # Extrahiert die sortierte 'source' Spalte als Vektor

# Formatiere die sortierten Quellen als eine einzige Zeile
formatted_sources <- paste(sorted_sources, collapse = ", ")
# paste(): Verbindet alle Quellen zu einem String
# collapse = ", ": Verwendet Komma und Leerzeichen als Trennzeichen

# Lese die Zieldatei
target_file <- file.path(path, "Quellen.qmd")
content <- readLines(target_file)  # Liest den Inhalt der Zieldatei

# Finde die Position, wo die Quellen eingefügt werden sollen
start_pos <- which(str_detect(content, "nocite: \\|"))
end_pos <- which(str_detect(content, "^---"))[2]  # Findet die zweite Zeile mit "---"

# Prüfe, ob es Inhalt zwischen nocite: | und dem zweiten --- gibt, der dem Muster entspricht
if (any(str_detect(content[(start_pos + 1):(end_pos - 1)], "^@[A-Za-z]+(18|19|20)\\d{2}$"))) {
  # Wenn ja, lösche diesen Inhalt
  content <- c(content[1:start_pos], content[end_pos:length(content)])
  end_pos <- start_pos + 1  # Aktualisiere end_pos, da wir Inhalt gelöscht haben
}

# Erstelle den neuen Inhalt
new_content <- c(
  content[1:start_pos],  # Behält den Inhalt bis zur "nocite: |" Zeile
  paste0("  ", formatted_sources),  # Fügt die formatierte Quellenliste ein, eingerückt mit 2 Leerzeichen
  content[end_pos:length(content)]  # Fügt den Rest des Inhalts hinzu
)

# Schreibe die aktualisierte Datei
writeLines(new_content, target_file) # Schreibt den neuen Inhalt in die Zieldatei