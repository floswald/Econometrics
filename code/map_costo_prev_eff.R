## Map (costo_previsto / costo_effettivo) / n_progetti by Italian province
## Data: data/costo_previsto_vs_effettivo_provincia.csv (BDAP-MOP, cumulative)
## Boundaries: ISTAT "Limiti amministrativi" (non generalizzati), province layer (field SIGLA)
## Run with the working directory set to the Econometrics book root (e.g. open Econometrics.Rproj).

pkgs <- c("sf", "dplyr", "ggplot2", "viridis", "scales")
missing_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs, repos = "https://cloud.r-project.org")

library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

csv_path <- "data/costo_previsto_vs_effettivo_provincia.csv"
cache_dir <- "~/.cache/istat_confini"
shp_zip_url <- "https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/2026/Limiti01012026.zip"

dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
zip_path <- file.path(cache_dir, "Limiti01012026.zip")

if (!file.exists(zip_path)) {
  download.file(shp_zip_url, zip_path, mode = "wb")
}

prov_shp_path <- list.files(cache_dir, pattern = "^ProvCM.*\\.shp$", recursive = TRUE, full.names = TRUE)[1]
if (is.na(prov_shp_path)) {
  unzip(zip_path, exdir = cache_dir)
  prov_shp_path <- list.files(cache_dir, pattern = "^ProvCM.*\\.shp$", recursive = TRUE, full.names = TRUE)[1]
}
stopifnot(!is.na(prov_shp_path))

province_sf_raw <- st_read(prov_shp_path, quiet = TRUE)

## ISTAT boundaries still carry the 4 historical Sardinian provinces
## (OT, OG, VS, CI); cruscotto data reports the current "Sud Sardegna" (SU).
## Dissolve those 4 polygons into one so the SIGLA join lines up.
province_sf <- province_sf_raw %>%
  mutate(SIGLA = ifelse(SIGLA %in% c("OT", "OG", "VS", "CI"), "SU", SIGLA)) %>%
  group_by(SIGLA) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()

costi <- read.csv(csv_path, stringsAsFactors = FALSE, na.strings = character(0)) %>%
  mutate(
    metrica = (costo_previsto_eur / costo_effettivo_eur) / n_progetti
  ) %>%
  filter(is.finite(metrica))

map_df <- province_sf %>%
  left_join(costi, by = c("SIGLA" = "provincia"))

n_unmatched <- sum(is.na(map_df$metrica))
if (n_unmatched > 0) {
  message(n_unmatched, " province senza dato (controllare SIGLA / codici mancanti)")
}

p <- ggplot(map_df) +
  geom_sf(aes(fill = metrica), color = "white", linewidth = 0.1) +
  scale_fill_viridis(
    name = "(prev/eff) / n_progetti\n(scala log)",
    option = "C",
    trans = "log10",
    labels = scales::label_number(accuracy = 0.0001),
    na.value = "grey85"
  ) +
  labs(
    title = "Rapporto costo previsto / costo effettivo, normalizzato per numero progetti",
    subtitle = "BDAP-MOP opere pubbliche, dati cumulativi per provincia",
    caption = "Fonte: cruscotto-italia.dati.gov.it (BDAP-MOP) + confini ISTAT"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

out_path <- "images/costo_previsto_vs_effettivo_provincia_map.png"
ggsave(out_path, p, width = 9, height = 10, dpi = 200, bg = "white")
message("Saved map to ", out_path)
