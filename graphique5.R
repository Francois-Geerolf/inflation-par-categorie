library(tidyverse)
library(eurostat)

## Load Eurostat datasets ------

datasets_eurostat <- c("hbs_str_t225", "hbs_str_t223", "hbs_str_t226")

for (dataset in datasets_eurostat){
  assign(dataset, 
         get_eurostat(dataset, stringsAsFactors = F, cache = F, time_format = "raw") |>
           rename(time = TIME_PERIOD)
  )
}

## Graphique 5 --------

graphique5 <- hbs_str_t225 %>%
  rename(category = age) %>%
  mutate(type = "age") %>%
  bind_rows(hbs_str_t223 %>%
              rename(category = quantile) %>%
              mutate(type = "quantile")) %>%
  bind_rows(hbs_str_t226 %>%
              rename(category = deg_urb) %>%
              mutate(type = "deg_urb")) %>%
  filter(coicop %in% c("CP041", "CP042"),
         time == "2020",
         geo == "FR") %>%
  select_if(~ n_distinct(.) > 1) %>%
  select(type, everything(.)) %>%
  arrange(coicop) %>%
  mutate(Coicop = factor(coicop,
                         levels = c("CP042",
                                    "CP041"),
                         labels = c("Loyers imputés (propriétaires occupants)",
                                    "Loyers réels (locataires)")),
         Category = factor(category, levels = c("Y_LT30", "Y30-44", "Y45-59", "Y_GE60",
                                                "DEG1", "DEG2", "DEG3",
                                                "QUINTILE1", "QUINTILE2", "QUINTILE3", "QUINTILE4", "QUINTILE5"),
                           labels = c("- de 30 ans", "De 30 à 44 ans", "De 45 à 59 ans", "+ de 60 ans",
                                      "Villes", "Villes - peuplées\net banlieues", "Zones rurales",
                                      "1er\n(+ pauvres)", "2è", "3è", "4è", "5è\n(+ riches)")),
         Type = factor(type, levels = c("age", "deg_urb", "quantile"),
                       labels = c("Âge", "Commune de résidence", "Cinquième de niveau de vie"))) %>%
  arrange(Coicop) %>%
  mutate(values = values/1000) %>%
  select(type, Type, category, Category, coicop, Coicop, values)

write_csv2(graphique5, "graphique5.csv")
save(graphique5, file = "graphique5.RData")

graphique5 %>%
  ggplot + geom_col(aes(x = Category, y = values, fill = Coicop)) +
  theme_minimal() +
  xlab("") + ylab("Poids budgétaire des loyers (%)") +
  scale_y_continuous(breaks = 0.01*seq(-30, 50, 2),
                     labels = percent_format(accuracy = 1)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin=margin(t=-35),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_manual(values = c("#005DA4", "#F59C00")) +
  facet_wrap(~ Type, scales = "free")




ggsave("graphique5.pdf", width = 7, height = 4, device = cairo_pdf)
ggsave("graphique5.png", bg = "white", width = 7, height = 4)
ggsave("graphique5.svg", bg = "white", width = 7, height = 4)



