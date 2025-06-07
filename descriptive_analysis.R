# PACKAGES ----------------------------------------------------------------
library(countrycode)
library(janitor)
library(ggplot2)
library(maps)
library(tidyverse)
library(vdemdata)

# 1. SOCIAL SPENDING ------------------------------------------------------
vdem <- vdemdata::vdem %>% 
  clean_names() %>% 
  select(country_name, country_text_id, country_id, year, co_wcode, 
         v2x_polyarchy) %>% 
  mutate(iso3c = countrycode(co_wcode, origin = 'cown', 
                             destination = 'iso3c'))

ws_dataset_latam <- readRDS("data/ws_dataset.RDS") %>% 
  filter(!iso3c  %in% c("ATG", "BHS", "BRB", "DMA", "GRD", "JAM", "KNA",
                        "LCA", "VCT", "TTO", "BLZ", "SUR"),
         year %in% c(1990:2019)) %>% 
  mutate(region2 = ifelse(region %in% c('North America'),
                   'Mexico', region)) %>% 
  left_join(vdem, join_by(iso3c, year)) %>% 
  mutate(region2 = case_when(
    region2 == "Caribbean" ~ "Caribe",
    region2 == "Central America" ~ "América Central",
    region2 == "Mexico" ~ "México",
    region2 == "South America" ~ "América do Sul"
  ))

write_rds(ws_dataset_latam, "data/ws_dataset_latam.rds")

# 1. LATAM Level ----------------------------------------------------------
ws_dataset_latam <- read_rds("data/ws_dataset_latam.rds")

ws_region_mean_y <- ws_dataset_latam %>% group_by(region2, year) %>%
  summarise(pcp_mean = mean(cg_pcp_sexp, na.rm = T),
            prop_mean = mean(cg_prop_sexp, na.rm = T),
            gdp_prop_mean = mean(cg_gdp_sexp, na.rm = T)) %>% 
  pivot_longer(cols = pcp_mean:gdp_prop_mean, 
               names_to = "type", values_to = "value")

latam_mean <- ws_dataset_latam %>% 
  group_by(year) %>% 
  summarise(pcp_mean = mean(cg_pcp_sexp, na.rm = T),
            prop_mean = mean(cg_prop_sexp, na.rm = T),
            gdp_prop_mean = mean(cg_gdp_sexp, na.rm = T)) %>% 
  pivot_longer(cols = pcp_mean:gdp_prop_mean, 
               names_to = "type", values_to = "value") %>% 
  mutate(type = factor(type, levels=c("gdp_prop_mean",
                                       "prop_mean", "pcp_mean")),
         region2 = "América Latina") %>% 
  rbind(ws_region_mean_y)

gg_latam_mean <- latam_mean %>% 
  filter(region2 %in% c("América Latina", "América Central")) %>% 
  mutate(type = fct_recode(type,
                     "Gasto Social (% PIB)" = "gdp_prop_mean",
                     "Gasto Social (% Total)" = "prop_mean",
                     "Gasto Social per capita" = "pcp_mean")) %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = region2)) +
  geom_point() +
  facet_wrap(~type, nrow = 2, scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, 
                                linewidth = 0.5),
    legend.position = "bottom",
    panel.spacing = unit(1.6, "lines"),
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(breaks = seq(1990, 2019, by = 2)) +
  labs(x = "Ano", y = "", linetype = "Região")

gg_latam_mean

ggsave('plot/gg_latam_mean.jpeg', 
       plot = gg_latam_mean, dpi = 500, 
       height = 10, width = 14, units = 'in')

## 1.1 Averages -----------------------------------------------------------
ws_dataset_latam %>% 
  summarise(pcp_mean = mean(cg_pcp_sexp, na.rm = T),
            prop_mean = mean(cg_prop_sexp, na.rm = T),
            gdp_prop_mean = mean(cg_gdp_sexp, na.rm = T))

ws_region_mean <- ws_dataset_latam %>% group_by(region2) %>%
  summarise(pcp_mean = mean(cg_pcp_sexp, na.rm = T),
            prop_mean = mean(cg_prop_sexp, na.rm = T),
            gdp_prop_mean = mean(cg_gdp_sexp, na.rm = T))

ws_region_mean

ws_country_mean <- ws_dataset_latam %>% 
  filter(region2 == "América Central") %>% group_by(iso3c) %>%
  summarise(pcp_mean = mean(cg_pcp_sexp, na.rm = T),
            prop_mean = mean(cg_prop_sexp, na.rm = T),
            gdp_prop_mean = mean(cg_gdp_sexp, na.rm = T))

ws_country_mean

quadrant_plot <- function(data, x, y, labx, laby){
  x_mid <- mean(c(max(data[[x]], na.rm = TRUE), min(data[[x]], na.rm = TRUE)))
  y_mid <- mean(c(max(data[[y]], na.rm = TRUE), min(data[[y]], na.rm = TRUE)))
  
  df <- data %>% mutate(quadrant = case_when(
    .data[[x]] > x_mid & .data[[y]] > y_mid ~ "Q1",
    .data[[x]] <= x_mid & .data[[y]] > y_mid ~ "Q2",
    .data[[x]] <= x_mid & .data[[y]] <= y_mid ~ "Q3",
    TRUE ~ "Q4"
  ))
  
  qdplot <- df %>% 
    ggplot( 
      aes(x = .data[[x]], y = .data[[y]])) +
    geom_vline(xintercept = x_mid) +
    geom_hline(yintercept = y_mid) +
    geom_point() +
    geom_text(hjust = 0, nudge_x = 0.1, size = 4, aes(label = iso3c)) +
    labs(x = labx,
         y = laby) +
    theme_minimal()
  
  return(qdplot)
}

quadrant_plot(ws_country_mean, "gdp_prop_mean", "pcp_mean",
              "Gasto Social (% PIB)", "Gasto Social per capita")
quadrant_plot(ws_country_mean, "prop_mean", "pcp_mean",
              "Gasto Social (% Total)", "Gasto Social per capita")
quadrant_plot(ws_country_mean, "prop_mean", "gdp_prop_mean",
              "Gasto Social (% Total)", "Gasto Social (% PIB)")

## 1.2 Time series --------------------------------------------------------
cores_dalton_friendly <- c(
  "#000000",   # Preto (mantido)
  "#FFA500",   # Laranja vivo (substitui #E69F00)
  "#00BFFF",   # Azul céu vivo (substitui #0072B2)
  "#87CEFA",   # Azul claro mais vibrante (substitui #56B4E9)
  "#FF69B4",   # Rosa-choque (substitui #CC79A7)
  "#D55E00",   # Vermelho-terra (mantido)
  "#9370DB",   # Roxo médio vivo (substitui #6A5ACD)
  "#00FF7F"    # Verde-água vibrante (substitui #009E73)
)

levels(factor(ws_dataset_latam$iso3c))

alpha_trends <- function(data, var, varlabel){
  var <- rlang::ensym(var)
  data %>% 
    filter(region2 == "América Central") %>% 
    ggplot(aes(x = year, y = !!var, alpha = iso3c)) +
    geom_line(aes(colour = iso3c), linewidth = 1.5, linetype = "solid") +
    scale_alpha_manual(values = c(CRI = 1.0, GTM = 0.4, HND = 0.4,
                                  NIC = 0.4, PAN = 1.0, SLV = 0.4),
                       guide = "none") +
    scale_colour_manual(values = cores_dalton_friendly) +
    theme_minimal() +
    theme(
      text = element_text(size = 20),
      axis.text.x = element_text(angle = 45)) +
    scale_x_continuous(breaks = seq(1990, 2019, by = 1)) +
    xlab("Ano") + ylab(varlabel) +
    labs(colour = "País")}

country_pcp_ts <- alpha_trends(ws_dataset_latam, 
                               "cg_pcp_sexp", "Gasto Social per capita")
country_prop_ts <- alpha_trends(ws_dataset_latam, 
                                "cg_prop_sexp", "Gasto Social (% Total)")
country_gdp_ts <- alpha_trends(ws_dataset_latam, 
                               "cg_gdp_sexp", "Gasto Social (% PIB)")

ggsave(plot = country_pcp_ts, "plot/country_pcp_ts.jpeg", 
       width = 15, height = 9)

ggsave(plot = country_prop_ts, "plot/country_prop_ts.jpeg",
       width = 15, height = 9)

ggsave(plot = country_gdp_ts, "plot/country_gdp_ts.jpeg",
       width = 15, height = 9)

# COMPARISONS -------------------------------------------------------------
var.list <- c("cg_prop_sexp", "cg_gdp_sexp", "cg_pcp_sexp", "gdp_pcp_ppp",
              "pop", "urban_pop", "statecap_baseline", "unemp", "gg_rev", 
              "kof_trade_df", "dp_ratio_old", "v2pariglef_ord",
              "v2x_polyarchy")

all.stats <- ws_dataset_latam %>% 
  filter(region2 == "América Central") %>% 
  group_by(iso3c) %>% 
  reframe(across(var.list, ~ mean(.x, na.rm = TRUE)))

all.stats %>% arrange(desc(cg_gdp_sexp)) %>% 
  select(iso3c, cg_gdp_sexp, everything())

all.stats %>% filter(iso3c %in% c("CRI", "PAN")) %>% t()

write.csv2(all.stats, "data/all.stats.csv")