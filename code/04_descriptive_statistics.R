################################################################################
# 04 - Descriptive statistics
#
# Replication package:
# Public Lighting, Fear of Crime, and Nighttime Mobility:
# Evidence from Brazil
################################################################################

################################################################################
# Load analytical dataset
################################################################################

Base_final_iluminacao <- readRDS(
  "data/processed/Base_final_iluminacao.rds"
)

################################################################################
# Construct descriptive indices
################################################################################

Base_final_iluminacao <- Base_final_iluminacao %>%
  mutate(

    # Household security index
    seg_dom =
      grades +
      alarme +
      camera +
      cerca,

    # Neighborhood disorder index
    desordem =
      locais_abandonados +
      carros_abandonados +
      terrenos_abandonados +
      tiros +
      cheiro_ruim +
      ruidos
  )

################################################################################
# Public lighting factor
################################################################################

Base_final_iluminacao <- Base_final_iluminacao %>%
  mutate(
    iluminacao_f = factor(
      iluminacao_ord,
      levels = 1:5,
      labels = c(
        "Optimal",
        "Good",
        "Regular",
        "Poor",
        "Very Poor"
      )
    )
  )

################################################################################
# Variables used in descriptive analyses
################################################################################

vars_modelo <- c(

  # Victimization
  "vitimizacao_12meses_casa_bairro",
  "violencia_viz",

  # Fear of crime
  "medo_assalto_roubo_viz",
  "evita_noite_vio",

  # Public lighting
  "iluminacao_otima",
  "iluminacao_boa",
  "iluminacao_regular",
  "iluminacao_ruim",
  "iluminacao_pess",
  "iluminacao_f",
  "iluminacao_ord",

  # Geographic controls
  "sudeste",
  "sul",
  "nordeste",
  "centro_oeste",
  "norte",
  "capital",
  "viz_capital",
  "interior",
  "porte_muito_grande",
  "porte_grande",
  "porte_medio",
  "porte_pequeno",

  # Individual controls
  "mulher",
  "casado_uniao",
  "branco",
  "idade_16_24",
  "idade_25_34",
  "idade_35_44",
  "idade_45_59",
  "idade_60_mais",
  "n_moradores",
  "Anos_mesma_vizinhanca",
  "ln_renda_pc",
  "indice_riqueza_z",
  "pea_dummy",
  "desordem"
)

################################################################################
# Complete-case sample
################################################################################

Base_figura_full_cc <- Base_final_iluminacao %>%
  dplyr::select(all_of(vars_modelo)) %>%
  tidyr::drop_na()

################################################################################
# Variables reported in Table 1
################################################################################

vars_descritiva <- c(
  "vitimizacao_12meses_casa_bairro",
  "violencia_viz",
  "medo_assalto_roubo_viz",
  "evita_noite_vio",
  "iluminacao_otima",
  "iluminacao_boa",
  "iluminacao_regular",
  "iluminacao_ruim",
  "iluminacao_pess"
)

################################################################################
# Descriptive statistics by sex
################################################################################

tabela_descritiva_sexo <- Base_figura_full_cc %>%
  mutate(
    sexo = case_when(
      mulher == 1 ~ "Women",
      mulher == 0 ~ "Men"
    )
  ) %>%
  select(all_of(vars_descritiva), sexo) %>%
  pivot_longer(
    cols = all_of(vars_descritiva),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  group_by(variavel) %>%
  summarise(

    Media_Total = mean(valor, na.rm = TRUE),
    DP_Total    = sd(valor, na.rm = TRUE),
    N_Total     = sum(!is.na(valor)),

    Media_Homens = mean(valor[sexo == "Men"], na.rm = TRUE),
    DP_Homens    = sd(valor[sexo == "Men"], na.rm = TRUE),
    N_Homens     = sum(!is.na(valor[sexo == "Men"])),

    Media_Mulheres = mean(valor[sexo == "Women"], na.rm = TRUE),
    DP_Mulheres    = sd(valor[sexo == "Women"], na.rm = TRUE),
    N_Mulheres     = sum(!is.na(valor[sexo == "Women"]))

  ) %>%
  ungroup()

################################################################################
# Table formatting
################################################################################

tabela_descritiva_sexo <- tabela_descritiva_sexo %>%
  mutate(

    Variável = dplyr::recode(

      variavel,

      vitimizacao_12meses_casa_bairro = "Victimization (12 months, home/neighborhood)",
      violencia_viz = "Neighborhood violence",
      medo_assalto_roubo_viz = "Fear of robbery",
      evita_noite_vio = "Avoids going out at night due to violence",
      iluminacao_otima = "Optimal lighting",
      iluminacao_boa = "Good lighting",
      iluminacao_regular = "Regular lighting",
      iluminacao_ruim = "Poor lighting",
      iluminacao_pess = "Very poor lighting"

    ),

    across(
      c(
        Media_Total,
        DP_Total,
        Media_Homens,
        DP_Homens,
        Media_Mulheres,
        DP_Mulheres
      ),
      ~ round(.x * 100, 1)
    )

  ) %>%
  select(

    Variável,

    Media_Total,
    DP_Total,
    N_Total,

    Media_Homens,
    DP_Homens,
    N_Homens,

    Media_Mulheres,
    DP_Mulheres,
    N_Mulheres

  )

################################################################################
# Export descriptive table
################################################################################

wb <- createWorkbook()

addWorksheet(
  wb,
  "Descriptive statistics"
)

writeData(
  wb,
  "Descriptive statistics",
  tabela_descritiva_sexo
)

saveWorkbook(
  wb,
  file = "output/tables/table_descriptive_statistics_by_sex.xlsx",
  overwrite = TRUE
)

message("✓ Descriptive statistics completed.")



################################################################################
# Descriptive statistics: control variables
################################################################################

vars_controle <- c(

  "idade_16_24",
  "idade_25_34",
  "idade_35_44",
  "idade_45_59",
  "idade_60_mais",

  "casado_uniao",
  "branco",

  "n_moradores",
  "Anos_mesma_vizinhanca",

  "ln_renda_pc",
  "indice_riqueza_z",
  "pea_dummy",

  "capital",
  "viz_capital",
  "interior",

  "porte_muito_grande",
  "porte_grande",
  "porte_medio",
  "porte_pequeno",

  "sudeste",
  "sul",
  "nordeste",
  "centro_oeste",
  "norte",

  "desordem"

)

################################################################################
# Auxiliary functions
################################################################################

descritiva_simples <- function(data, var){

  x <- data[[var]]

  tibble(
    media = mean(x, na.rm = TRUE) * 100,
    dp    = sd(x, na.rm = TRUE) * 100,
    n     = sum(!is.na(x))
  )

}

descritiva_continua <- function(data, var){

  x <- data[[var]]

  tibble(
    media = mean(x, na.rm = TRUE),
    dp    = sd(x, na.rm = TRUE),
    n     = sum(!is.na(x))
  )

}

################################################################################
# Table of control variables
################################################################################

tabela_controle <- map_dfr(vars_controle, function(v){

  is_continua <- v %in% c(
    "ln_renda_pc",
    "indice_riqueza_z",
    "n_moradores",
    "Anos_mesma_vizinhanca",
    "desordem"
  )

  f <- if (is_continua)
    descritiva_continua
  else
    descritiva_simples

  geral <- f(Base_figura_full_cc, v)

  homens <- f(
    filter(Base_figura_full_cc, mulher == 0),
    v
  )

  mulheres <- f(
    filter(Base_figura_full_cc, mulher == 1),
    v
  )

  tibble(

    Variavel = v,

    Geral_media = geral$media,
    Geral_dp    = geral$dp,
    Geral_n     = geral$n,

    Homens_media = homens$media,
    Homens_dp    = homens$dp,
    Homens_n     = homens$n,

    Mulheres_media = mulheres$media,
    Mulheres_dp    = mulheres$dp,
    Mulheres_n     = mulheres$n

  )

})

################################################################################
# Labels
################################################################################

tabela_controle <- tabela_controle %>%
  mutate(

    Variavel = recode(

      Variavel,

      idade_16_24 = "Age 16–24",
      idade_25_34 = "Age 25–34",
      idade_35_44 = "Age 35–44",
      idade_45_59 = "Age 45–59",
      idade_60_mais = "Age 60+",

      casado_uniao = "Married or cohabiting",
      branco = "White or Asian",

      n_moradores = "Household size",
      Anos_mesma_vizinhanca = "Years in the same neighborhood",

      ln_renda_pc = "Log household income per capita",
      indice_riqueza_z = "Household wealth index",

      pea_dummy = "Economically active",

      capital = "State capital",
      viz_capital = "Metropolitan municipality",
      interior = "Interior municipality",

      porte_muito_grande = "Very large municipality",
      porte_grande = "Large municipality",
      porte_medio = "Medium municipality",
      porte_pequeno = "Small municipality",

      sudeste = "Southeast",
      sul = "South",
      nordeste = "Northeast",
      norte = "North",
      centro_oeste = "Midwest",

      desordem = "Neighborhood disorder index"

    )

  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

################################################################################
# Export table
################################################################################

wb <- createWorkbook()

addWorksheet(
  wb,
  "Control variables"
)

writeData(
  wb,
  sheet = "Control variables",
  x = tabela_controle,
  headerStyle = createStyle(
    textDecoration = "bold"
  )
)

saveWorkbook(
  wb,
  file = "output/tables/table_control_variables_by_sex.xlsx",
  overwrite = TRUE
)

message("✓ Control variables table exported.")



################################################################################
# Figure 2 - Public lighting and crime-related outcomes
################################################################################

################################################################################
# Prepare plotting dataset
################################################################################

base_plot <- Base_figura_full_cc %>%
  mutate(
    iluminacao_f = factor(
      iluminacao_ord,
      levels = 1:5,
      labels = c(
        "Optimal",
        "Good",
        "Regular",
        "Poor",
        "Very Poor"
      )
    )
  ) %>%
  group_by(iluminacao_f) %>%
  summarise(

    victimization = mean(
      vitimizacao_12meses_casa_bairro,
      na.rm = TRUE
    ),

    neighborhood_violence = mean(
      violencia_viz,
      na.rm = TRUE
    ),

    fear = mean(
      medo_assalto_roubo_viz,
      na.rm = TRUE
    ),

    avoiding = mean(
      evita_noite_vio,
      na.rm = TRUE
    ),

    .groups = "drop"

  )

################################################################################
# Convert to long format
################################################################################

base_long <- base_plot %>%
  pivot_longer(
    cols = -iluminacao_f,
    names_to = "variable",
    values_to = "proportion"
  ) %>%
  mutate(
    variable = recode(
      variable,
      victimization = "Victimization",
      neighborhood_violence = "Neighborhood Violence",
      fear = "Fear of Crime",
      avoiding = "Avoiding Going Out at Night"
    )
  )

################################################################################
# Labels shown only at the first and last categories
################################################################################

labels_extremos <- base_long %>%
  group_by(variable) %>%
  filter(
    iluminacao_f %in% c(
      "Optimal",
      "Very Poor"
    )
  ) %>%
  ungroup()


################################################################################
# Figure 2
# Public lighting and crime-related outcomes
################################################################################

fig_iluminacao <- ggplot(
  base_long,
  aes(
    x = iluminacao_f,
    y = proportion,
    group = variable,
    linetype = variable,
    shape = variable
  )
) +

  geom_line(
    linewidth = 0.8,
    color = "black"
  ) +

  geom_point(
    size = 2.8,
    color = "black"
  ) +

  # Avoiding going out at night
  geom_text(
    data = subset(
      base_long,
      variable == "Avoiding Going Out at Night"
    ),
    aes(
      label = percent(
        proportion,
        accuracy = 0.1
      )
    ),
    vjust = -0.8,
    size = 3,
    family = "Times New Roman",
    show.legend = FALSE
  ) +

  # Fear of crime
  geom_text(
    data = subset(
      base_long,
      variable == "Fear of Crime"
    ),
    aes(
      label = percent(
        proportion,
        accuracy = 0.1
      )
    ),
    vjust = -0.8,
    nudge_x = -0.06,
    size = 3,
    family = "Times New Roman",
    show.legend = FALSE
  ) +

  # Neighborhood violence
  geom_text(
    data = subset(
      base_long,
      variable == "Neighborhood Violence"
    ),
    aes(
      label = percent(
        proportion,
        accuracy = 0.1
      )
    ),
    vjust = -0.8,
    nudge_x = -0.06,
    size = 3,
    family = "Times New Roman",
    show.legend = FALSE
  ) +

  # Victimization
  geom_text(
    data = subset(
      base_long,
      variable == "Victimization"
    ),
    aes(
      label = percent(
        proportion,
        accuracy = 0.1
      )
    ),
    vjust = -0.8,
    size = 3,
    family = "Times New Roman",
    show.legend = FALSE
  ) +

  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.85),
    expand = expansion(mult = c(0.02, 0.08))
  ) +

  labs(
    x = "Perceived Public Lighting Quality",
    y = "Proportion",
    linetype = NULL,
    shape = NULL
  ) +

  theme_classic(base_size = 12) +

  theme(
    text = element_text(
      family = "Times New Roman"
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    panel.grid.major.y = element_line(
      color = "grey85",
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(
      10,
      10,
      10,
      10
    )
  ) +

  guides(
    linetype = guide_legend(
      nrow = 2,
      byrow = TRUE
    ),
    shape = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

################################################################################
# Export figure
################################################################################

ggsave(
  filename = "output/figures/Figure_2_Public_Lighting_Crime_Outcomes.png",
  plot = fig_iluminacao,
  width = 22,
  height = 12,
  units = "cm",
  dpi = 300,
  bg = "white"
)

message("✓ Figure 2 exported.")


################################################################################
# Save processed dataset
################################################################################

saveRDS(
  Base_final_iluminacao,
  "data/processed/Base_final_iluminacao.rds"
)

message("✓ Processed dataset saved.")
message("Observations: ", format(nrow(Base_final_iluminacao), big.mark = ","))
message("Variables: ", ncol(Base_final_iluminacao))

