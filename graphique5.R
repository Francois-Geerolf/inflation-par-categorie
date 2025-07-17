library(tidyverse)
library(eurostat)
library(scales)

# ---- Téléchargement des données Eurostat ----

datasets <- c("hbs_str_t225", "hbs_str_t223", "hbs_str_t226")

data_list <- datasets |>
  set_names() |>
  map(~ get_eurostat(.x, stringsAsFactors = FALSE, cache = FALSE, time_format = "raw") |>
        rename(time = TIME_PERIOD))

# Affectation des jeux de données
list2env(data_list, envir = .GlobalEnv)

# ---- Préparation des données pour le graphique ----

graphique5 <- bind_rows(
  hbs_str_t225 |> rename(category = age) |> mutate(type = "age"),
  hbs_str_t223 |> rename(category = quantile) |> mutate(type = "quantile"),
  hbs_str_t226 |> rename(category = deg_urb) |> mutate(type = "deg_urb")
) |>
  filter(
    coicop %in% c("CP041", "CP042"),
    time == "2020",
    geo == "FR"
  ) |>
  select_if(~ n_distinct(.) > 1) |>
  select(type, everything()) |>
  mutate(
    Coicop = factor(coicop,
                    levels = c("CP042", "CP041"),
                    labels = c("Loyers imputés (propriétaires occupants)",
                               "Loyers réels (locataires)")),
    Category = factor(category,
                      levels = c(
                        "Y_LT30", "Y30-44", "Y45-59", "Y_GE60",
                        "DEG1", "DEG2", "DEG3",
                        "QUINTILE1", "QUINTILE2", "QUINTILE3", "QUINTILE4", "QUINTILE5"
                      ),
                      labels = c(
                        "- de 30 ans", "De 30 à 44 ans", "De 45 à 59 ans", "+ de 60 ans",
                        "Villes", "Villes - peuplées\net banlieues", "Zones rurales",
                        "1er\n(+ pauvres)", "2è", "3è", "4è", "5è\n(+ riches)"
                      )),
    Type = factor(type,
                  levels = c("age", "deg_urb", "quantile"),
                  labels = c("Âge", "Commune de résidence", "Cinquième de niveau de vie")),
    values = values / 1000
  ) |>
  arrange(Coicop) |>
  select(type, Type, category, Category, coicop, Coicop, values)

# ---- Sauvegarde des données ----

write_csv2(graphique5, "graphique5.csv")
save(graphique5, file = "graphique5.RData")

# ---- Graphique 5 ----

graphique5 |>
  ggplot(aes(x = Category, y = values, fill = Coicop)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Type, scales = "free") +
  scale_fill_manual(values = c("#005DA4", "#F59C00")) +
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.02),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    x = NULL,
    y = "Poids budgétaire des loyers (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = -35),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ---- Exports graphiques ----

ggsave("graphique5.pdf", width = 7, height = 4, device = cairo_pdf)
ggsave("graphique5.png", bg = "white", width = 7, height = 4)
ggsave("graphique5.svg", bg = "white", width = 7, height = 4)
