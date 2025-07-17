library(tidyverse)
library(eurostat)
library(rsdmx)

## Load INSEE datasets ------

idbanks_INSEE <- c("010533929",
                   "010533930",
                   "010533976",
                   "010533977",
                   "010534011",
                   "010534012")

# Build URL
url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbanks_INSEE, collapse = "+")
)

# Read and clean data

`IPPI-2015` <- url |>
  readSDMX() |>
  as_tibble() |>
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01")),
         OBS_VALUE = as.numeric(OBS_VALUE)) |>
  select(-TIME_PERIOD)

## Données du graphique 5 --------

# Set French locale for date formatting
Sys.setlocale("LC_TIME", "fr_CA.UTF-8")

# ---- Préparation des données ----
graphique6 <- `IPPI-2015` |>
  mutate(
    produit = str_extract(
      TITLE_FR,
      "(?<=CPF \\d{2}\\.\\d{2} − ).*?(?= (MDD|MN|:))"
    ),
    qualite = case_when(
      str_detect(TITLE_FR, "\\bMDD\\b") ~ "Marques de Distributeur (MDD)",
      str_detect(TITLE_FR, "\\bMN\\b") ~ "Marques Nationales (MN)",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    date >= as.Date("2021-10-01"),
    date <= as.Date("2023-10-01")
  ) |>
  group_by(TITLE_FR) |>
  arrange(date) |>
  mutate(OBS_VALUE = 100 * OBS_VALUE / OBS_VALUE[1]) |>
  ungroup() |>
  mutate(
    produit = factor(
      produit,
      levels = c(
        "Pâtes molles",
        "Biscuits et gâteaux de conservation",
        "Jambons cuits supérieurs"
      )
    )
  ) |>
  select(qualite, produit, date, OBS_VALUE) |>
  arrange(produit, qualite, date)

# Sauvegarde CSV
write_csv2(graphique6, file = "graphique6.csv")

# ---- Graphique 6 ----

ggplot(data = graphique6) +
  geom_line(aes(x = date, y = OBS_VALUE, color = qualite), size = 1) +
  facet_wrap(~ produit) +
  scale_y_continuous(breaks = seq(-100, 500, 5)) +
  scale_x_date(
    breaks = seq.Date(from = as.Date("2021-10-01"), to = Sys.Date(), by = "3 months"),
    labels = date_format("%b %Y")
  ) +
  scale_color_manual(values = c("#005DA4", "#F59C00")) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Indice 100 = Octobre 2021"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.margin = margin(t = -3),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)
  )

ggsave("graphique6.pdf", width = 7, height = 4, device = cairo_pdf)
ggsave("graphique6.png", bg = "white", width = 7, height = 4)
ggsave("graphique6.svg", bg = "white", width = 7, height = 4)



