```{r message=FALSE, warning=FALSE, echo=FALSE, output=FALSE}
library(dplyr)
library(tidyr)
library(shiny)
library(DT)

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

dt_EPOC <- datatable(EPOC_data_df_short,
                     options = list(
                       pageLength = 10,
                       scrollX = TRUE,
                       autoWidth = FALSE,
                       columnDefs = list(
                         list(
                           targets = '_all',
                           className = 'dt-head-nowrap dt-body-nowrap'
                         )
                       )
                     )) %>% 
  formatRound(
    columns = c("VO2_gross_SS [l·min⁻¹]", "EPOC_fast [l]", "VO2_net_SS [l·min⁻¹]", 
                "O2_Speicher [l]", "VO2_Referenz [l·min⁻¹]", "VO2_Ruhe [l·min⁻¹]",
                "EPOC_fast [ml·kg⁻¹]", "EPOC_PCr [l]", "EPOC_PCr [ml·kg⁻¹]"),
    digits = 3
  ) %>%
  formatRound(
    columns = c("P_Tot_kg [W·kg⁻¹]", "A [l·min⁻¹]", "B [l·min⁻¹]", 
                "MM_akt [kg]", "R2_off", 
                "WPCR_corrected [kJ]", "PCr_used [mmol·kg⁻¹]"),
    digits = 2
  ) %>%
  formatRound(
    columns = c("P_Tot [W]", "TauA [s]", "TauB [s]"),
    digits = 1
  ) %>%
  formatRound(
    columns = c("Nr", "Masse [kg]"),
    digits = 0
  )
```

```{r}
#| label: tbl-EPOCALL
#| tbl-cap-location: top
#| tbl-cap: "Parameter der EPOC-Modellierung und Komponenten der anaerob-alaktaziden Energieumsätze aller Belastungsdurchgänge"

# Anzeigen der Tabelle
dt_EPOC
```