################################################################################
# 03 - Data preparation
#
# Replication package:
# Public Lighting, Fear of Crime, and Nighttime Mobility:
# Evidence from Brazil
#
# Purpose:
# - Clean the original PNV 2012 dataset
# - Recode variables
# - Construct analytical variables
# - Create the final dataset used in the analyses
################################################################################


################################################################################
# 03 - Data preparation
################################################################################

################################################################################
# 3.1 Sample weights
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    peso_usado = peso
  )

################################################################################
# 3.2 Geographic characteristics
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    # Region
    sudeste      = as.integer(regiao == 1),
    sul          = as.integer(regiao == 2),
    nordeste     = as.integer(regiao == 3),
    centro_oeste = as.integer(regiao == 4),
    norte        = as.integer(regiao == 5),

    # Metropolitan area
    capital      = as.integer(metrop == 1),
    viz_capital  = as.integer(metrop == 2),
    interior     = as.integer(metrop == 3),

    # Municipality size
    porte_muito_grande = as.integer(porte_mun == 5),
    porte_grande       = as.integer(porte_mun == 4),
    porte_medio        = as.integer(porte_mun == 3),
    porte_pequeno      = as.integer(porte_mun %in% c(0, 1, 2))
  )

 
################################################################################
# 3.3 Household composition
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    # Household composition
    p1  = ifelse(p1 == 96, 0, p1),       # Adults (16+)
    p1a = ifelse(p1a == 96, 0, p1a),     # Women (16+)
    p2a = ifelse(p2a %in% c(96, 97), 0, p2a),  # Children (<16)

    n_adultos   = p1,
    n_criancas  = p2a,
    n_moradores = n_adultos + n_criancas
  )

################################################################################
# 3.4 Age groups
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    idade_16_24 = case_when(
      idatab == 1 ~ 1,
      idatab %in% 2:5 ~ 0,
      TRUE ~ NA_real_
    ),

    idade_25_34 = case_when(
      idatab == 2 ~ 1,
      idatab %in% c(1, 3, 4, 5) ~ 0,
      TRUE ~ NA_real_
    ),

    idade_35_44 = case_when(
      idatab == 3 ~ 1,
      idatab %in% c(1, 2, 4, 5) ~ 0,
      TRUE ~ NA_real_
    ),

    idade_45_59 = case_when(
      idatab == 4 ~ 1,
      idatab %in% c(1, 2, 3, 5) ~ 0,
      TRUE ~ NA_real_
    ),

    idade_60_mais = case_when(
      idatab == 5 ~ 1,
      idatab %in% 1:4 ~ 0,
      TRUE ~ NA_real_
    )
  )


################################################################################
# 3.5 Residential characteristics
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    # Has always lived in the same city
    Mesma_cidade = case_when(
      p3 == 0.5   ~ 1,
      p3 == 99999 ~ 0,
      TRUE        ~ NA_real_
    ),

    # Years living in the same city
    p4ano = ifelse(p4ano > 95, NA, p4ano),

    # Years living in the same neighborhood
    p139ano = ifelse(p139ano > 95, NA, p139ano),

    Anos_mesma_cidade      = p4ano,
    Anos_mesma_vizinhanca  = p139ano
  ) %>%
  mutate(
    # Neighborhood residence cannot exceed city residence
    Anos_mesma_vizinhanca = ifelse(
      !is.na(Anos_mesma_cidade) &
      !is.na(Anos_mesma_vizinhanca) &
      Anos_mesma_vizinhanca > Anos_mesma_cidade,
      Anos_mesma_cidade,
      Anos_mesma_vizinhanca
    )
  )

################################################################################
# 3.6 Individual characteristics
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    # Marital status
    casado_uniao = case_when(
      p5 %in% c(2, 3) ~ 1,
      p5 %in% c(1, 4, 5, 6) ~ 0,
      TRUE ~ NA_real_
    ),

    # Sex
    mulher = case_when(
      sexotab == 2 ~ 1,
      sexotab == 1 ~ 0,
      TRUE ~ NA_real_
    ),

    # Race (White or Asian)
    branco = case_when(
      cor %in% c(1, 4) ~ 1,
      cor %in% c(2, 3, 5, 6) ~ 0,
      TRUE ~ NA_real_
    )
  )


################################################################################
# 3.7 Education
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    # Respondent's education
    escol_sem_instr = case_when(
      escola == 1 ~ 1,
      escola %in% 2:8 ~ 0,
      TRUE ~ NA_real_
    ),

    escol_ate_fund_completo = case_when(
      escola %in% c(2, 3) ~ 1,
      escola %in% c(1, 4:8) ~ 0,
      TRUE ~ NA_real_
    ),

    escol_ate_med_completo = case_when(
      escola %in% c(4, 5) ~ 1,
      escola %in% c(1:3, 6:8) ~ 0,
      TRUE ~ NA_real_
    ),

    escol_ate_pos_grad = case_when(
      escola %in% c(6:8) ~ 1,
      escola %in% c(1:5) ~ 0,
      TRUE ~ NA_real_
    ),

    # Household head's education
    escolc_sem_instr = case_when(
      escolac == 1 ~ 1,
      escolac %in% 2:8 ~ 0,
      TRUE ~ NA_real_
    ),

    escolc_ate_fund_completo = case_when(
      escolac %in% c(2, 3) ~ 1,
      escolac %in% c(1, 4:8) ~ 0,
      TRUE ~ NA_real_
    ),

    escolc_ate_med_completo = case_when(
      escolac %in% c(4, 5) ~ 1,
      escolac %in% c(1:3, 6:8) ~ 0,
      TRUE ~ NA_real_
    ),

    escolc_ate_pos_grad = case_when(
      escolac %in% c(6:8) ~ 1,
      escolac %in% c(1:5) ~ 0,
      TRUE ~ NA_real_
    )
  )

################################################################################
# 3.8 Labor market status
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    pea_dummy = case_when(
      pea %in% 1:10 ~ 1,
      pea %in% 11:16 ~ 0,
      TRUE ~ NA_real_
    )
  )

################################################################################
# 3.9 Household income
################################################################################

Base_geral <- Base_geral %>%
  mutate(

    # Household income categories (minimum wages)
    sal_min_ate_1 = case_when(
      rendaf == 1 ~ 1,
      rendaf %in% 2:8 ~ 0,
      TRUE ~ NA_real_
    ),

    sal_min_de_1_ate_2_sm = case_when(
      rendaf == 2 ~ 1,
      rendaf %in% c(1, 3:8) ~ 0,
      TRUE ~ NA_real_
    ),

    sal_min_de_2_ate_3_sm = case_when(
      rendaf == 3 ~ 1,
      rendaf %in% c(1, 2, 4:8) ~ 0,
      TRUE ~ NA_real_
    ),

    sal_min_de_3_ate_5_sm = case_when(
      rendaf == 4 ~ 1,
      rendaf %in% c(1:3, 5:8) ~ 0,
      TRUE ~ NA_real_
    ),

    sal_min_acima_5 = case_when(
      rendaf %in% 5:8 ~ 1,
      rendaf %in% 1:4 ~ 0,
      TRUE ~ NA_real_
    ),

    # Approximate household income (minimum wages)
    renda_sm_aprox = case_when(
      rendaf == 1 ~ 0.5,
      rendaf == 2 ~ 1.5,
      rendaf == 3 ~ 2.5,
      rendaf == 4 ~ 4.0,
      rendaf == 5 ~ 7.5,
      rendaf == 6 ~ 12.5,
      rendaf == 7 ~ 17.5,
      rendaf == 8 ~ 22.5,
      TRUE ~ NA_real_
    ),

    # Approximate per capita household income
    renda_pc_sm = renda_sm_aprox / n_moradores,

    # Log of per capita household income
    ln_renda_pc = log(renda_pc_sm)
  )


################################################################################
# 3.10 Daily routine and mobility
################################################################################

# Usual location during the day (Monday–Friday)
Base_geral <- Base_geral %>%
  mutate(
    casa_manha = case_when(
      p13a == 1 ~ 1,
      p13a == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    casa_tarde = case_when(
      p13b == 1 ~ 1,
      p13b == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    casa_noite = case_when(
      p13c == 1 ~ 1,
      p13c == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    casa_madrugada = case_when(
      p13d == 1 ~ 1,
      p13d == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

################################################################################
# 3.11 Leisure activities and nighttime mobility
################################################################################

itens_p14 <- c(
  "p14a", "p14b", "p14c", "p14d", "p14e",
  "p14h", "p14i", "p14j", "p14k"
)

Base_geral <- Base_geral %>%
  mutate(
    across(
      all_of(itens_p14),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x %in% c(97, 99) ~ NA_real_,
        TRUE ~ NA_real_
      )
    )
  ) %>%
  mutate(
    saidas_total = rowSums(across(all_of(itens_p14)), na.rm = TRUE),

    # Proxy for nighttime leisure activities
    sem_saida_noturna = case_when(
      p14a == 1 | p14c == 1 | p14d == 1 |
        p14e == 1 | p14j == 1 | p14k == 1 ~ 0,

      p14a == 0 & p14c == 0 & p14d == 0 &
        p14e == 0 & p14j == 0 & p14k == 0 ~ 1,

      TRUE ~ NA_real_
    ),

    # Avoids going out at night due to violence
    evita_noite_vio = case_when(
      p162a == 1 ~ 1,
      p162a == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Does not go to bars/nightclubs
    sem_bar_noturno = case_when(
      p14k == 0 ~ 1,
      p14k == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )


################################################################################
# 3.12 Transportation
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    trans_carro = case_when(
      p15a == 1 ~ 1,
      p15a %in% c(2:10) ~ 0,
      TRUE ~ NA_real_
    ),

    trans_moto = case_when(
      p15a == 2 ~ 1,
      p15a %in% c(1, 3:10) ~ 0,
      TRUE ~ NA_real_
    ),

    trans_onibus = case_when(
      p15a == 3 ~ 1,
      p15a %in% c(1, 2, 4:10) ~ 0,
      TRUE ~ NA_real_
    ),

    trans_bike = case_when(
      p15a == 9 ~ 1,
      p15a %in% c(1:8, 10) ~ 0,
      TRUE ~ NA_real_
    ),

    trans_ape = case_when(
      p15a == 10 ~ 1,
      p15a %in% 1:9 ~ 0,
      TRUE ~ NA_real_
    ),

    trans_outros = case_when(
      p15a %in% 4:8 ~ 1,
      p15a %in% c(1, 2, 3, 9, 10) ~ 0,
      TRUE ~ NA_real_
    )
  )

################################################################################
# 3.13 Housing characteristics
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    apartamento = case_when(
      p183 == 1 ~ 1,
      p183 %in% 2:7 ~ 0,
      TRUE ~ NA_real_
    ),

    casa = case_when(
      p183 == 2 ~ 1,
      p183 %in% c(1, 3:7) ~ 0,
      TRUE ~ NA_real_
    ),

    barraco_outros = case_when(
      p183 %in% 3:7 ~ 1,
      p183 %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    )
  )

################################################################################
# 3.14 Household wealth index (PCA)
################################################################################

bens <- c(
  "tvcor",
  "carro",
  "radio",
  "maqlav",
  "videodvd",
  "banhe",
  "gelad",
  "freezer"
)

# Asset ownership indicators
Base_geral <- Base_geral %>%
  mutate(
    across(
      all_of(bens),
      ~ case_when(
        .x %in% 1:6 ~ 1,
        .x == 96 ~ 0,
        .x == 97 ~ NA_real_
      ),
      .names = "{.col}_d"
    )
  )

# Observation identifier
Base_geral <- Base_geral %>%
  mutate(
    id_obs = row_number()
  )

# Dataset used in PCA
dados_pca <- Base_geral %>%
  dplyr::select(id_obs, ends_with("_d")) %>%
  tidyr::drop_na()

# Principal Component Analysis
pca <- prcomp(
  dados_pca %>%
    dplyr::select(-id_obs),
  scale. = TRUE
)

# Wealth index
indice_df <- data.frame(
  id_obs = dados_pca$id_obs,
  indice_riqueza = -pca$x[, 1]
)

# Merge with analytical dataset
Base_geral <- Base_geral %>%
  left_join(indice_df, by = "id_obs") %>%
  mutate(
    indice_riqueza_z = as.numeric(scale(indice_riqueza))
  )


################################################################################
# 4.1 Property crimes
################################################################################

# Theft (ever)
Base_geral <- Base_geral %>%
  mutate(
    furto = case_when(
      p19 == 1 | p21 == 1 | p23 == 1 ~ 1,
      (p19 %in% c(2, NA)) &
        (p21 %in% c(2, NA)) &
        (p23 %in% c(2, NA)) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Theft in the last 12 months
Base_geral <- Base_geral %>%
  mutate(
    furto_12meses = case_when(
      p19a == 3 | p21a == 3 | p23a == 3 ~ 1,
      (p19a == 4 | is.na(p19a)) &
        (p21a == 4 | is.na(p21a)) &
        (p23a == 4 | is.na(p23a)) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Theft in the last 12 months at home, on the street near home, or in the neighborhood
Base_geral <- Base_geral %>%
  mutate(
    furto_12meses_casa_bairro = case_when(

      # Home / street near home / neighborhood
      p34 %in% c(1, 2, 3) |
      p49 %in% c(1, 2, 3) |
      p65 %in% c(1, 14, 17) ~ 1,

      # All thefts occurred elsewhere
      (p34 %in% c(4:7, 9:13, 98) | is.na(p34)) &
      (p49 %in% c(4:7, 9:13, 98) | is.na(p49)) &
      (p65 %in% c(2:13, 15, 16, 19, 20, 98) | is.na(p65)) ~ 0,

      TRUE ~ NA_real_
    )
  )

# Robbery (ever)
Base_geral <- Base_geral %>%
  mutate(
    roubo = case_when(
      p20 == 1 | p22 == 1 | p24 == 1 ~ 1,
      (p20 %in% c(2, NA)) &
        (p22 %in% c(2, NA)) &
        (p24 %in% c(2, NA)) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Robbery in the last 12 months
Base_geral <- Base_geral %>%
  mutate(
    roubo_12meses = case_when(
      p20a == 3 | p22a == 3 | p24a == 3 ~ 1,
      (p20a == 4 | is.na(p20a)) &
        (p22a == 4 | is.na(p22a)) &
        (p24a == 4 | is.na(p24a)) ~ 0,
      TRUE ~ NA_real_
    )
  )


################################################################################
# Property crime victimization (continued)
################################################################################

# Robbery in the last 12 months at home, on the street near home, or in the neighborhood
Base_geral <- Base_geral %>%
  mutate(
    roubo_12meses_casa_bairro = case_when(

      # Home / street near home / neighborhood
      p39 %in% c(1, 2, 3) |
      p54 %in% c(1, 2, 3) |
      p71 %in% c(1, 14, 17) ~ 1,

      # All robberies occurred elsewhere
      (p39 %in% c(4:7, 9:13, 98) | is.na(p39)) &
      (p54 %in% c(4:7, 9:13, 98) | is.na(p54)) &
      (p71 %in% c(2:13, 15, 16, 19, 20, 98) | is.na(p71)) ~ 0,

      TRUE ~ NA_real_
    )
  )

# Property victimization in the last 12 months (home or neighborhood)
Base_geral <- Base_geral %>%
  mutate(
    vitimizacao_12meses_casa_bairro = case_when(
      furto_12meses_casa_bairro == 1 |
      roubo_12meses_casa_bairro == 1 ~ 1,

      furto_12meses_casa_bairro == 0 &
      roubo_12meses_casa_bairro == 0 ~ 0,

      TRUE ~ NA_real_
    )
  )

################################################################################
# Accidents
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    acidente_12meses = case_when(
      p28a == 1 |
      p28b == 1 |
      p28c == 1 |
      p28d == 1 |
      p28e == 1 ~ 1,

      p28a == 2 &
      p28b == 2 &
      p28c == 2 &
      p28d == 2 &
      p28e == 2 ~ 0,

      TRUE ~ NA_real_
    )
  )




################################################################################
# 4.2 Neighborhood violence
################################################################################

Base_geral <- Base_geral %>%
  mutate(

    # Neighbors physically assaulted
    agressao_viz = case_when(
      p150e == 1 ~ 1,
      p150e == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Neighbors robbed
    assaltados_viz = case_when(
      p150f == 1 ~ 1,
      p150f == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Neighbors murdered
    assassinados_viz = case_when(
      p150g == 1 ~ 1,
      p150g == 2 ~ 0,
      TRUE ~ NA_real_
    )

  ) %>%
  mutate(

    # Any violent event involving neighbors
    violencia_viz = case_when(
      agressao_viz == 1 |
        assaltados_viz == 1 |
        assassinados_viz == 1 ~ 1,

      agressao_viz == 0 &
        assaltados_viz == 0 &
        assassinados_viz == 0 ~ 0,

      TRUE ~ NA_real_
    )

  )


################################################################################
# 4.3 Neighborhood disorder (Broken Windows)
################################################################################

Base_geral <- Base_geral %>%
  mutate(

    # Abandoned buildings
    locais_abandonados = case_when(
      p147a == 1 ~ 1,
      p147a == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Abandoned vehicles
    carros_abandonados = case_when(
      p147b == 1 ~ 1,
      p147b == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Vacant lots
    terrenos_abandonados = case_when(
      p147c == 1 ~ 1,
      p147c == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Gunshots
    tiros = case_when(
      p147d == 1 ~ 1,
      p147d == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Bad smells
    cheiro_ruim = case_when(
      p147e == 1 ~ 1,
      p147e == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Noise
    ruidos = case_when(
      p147f == 1 ~ 1,
      p147f == 2 ~ 0,
      TRUE ~ NA_real_
    )

  )


################################################################################
# 4.4 Household security measures
################################################################################

Base_geral <- Base_geral %>%
  mutate(

    # Window bars
    grades = case_when(
      p151a == 1 ~ 1,
      p151a == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Alarm system
    alarme = case_when(
      p151f == 1 ~ 1,
      p151f == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Security cameras
    camera = case_when(
      p151g == 1 ~ 1,
      p151g == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Electric fence or razor wire
    cerca = case_when(
      p151l == 1 | p151m == 1 ~ 1,
      p151l == 2 & p151m == 2 ~ 0,
      TRUE ~ NA_real_
    )

  )

################################################################################
# 4.5 Perceived safety at home
################################################################################

Base_geral <- Base_geral %>%
  mutate(

    p159_num = as.numeric(p159),

    # Very safe
    muito_seguro_casa = case_when(
      p159_num == 1 ~ 1,
      p159_num %in% c(2, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),

    # Somewhat safe
    um_pouco_seguro_casa = case_when(
      p159_num == 2 ~ 1,
      p159_num %in% c(1, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),

    # Somewhat unsafe
    um_pouco_inseguro_casa = case_when(
      p159_num == 4 ~ 1,
      p159_num %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    ),

    # Very unsafe
    muito_inseguro_casa = case_when(
      p159_num == 3 ~ 1,
      p159_num %in% c(1, 2, 4) ~ 0,
      TRUE ~ NA_real_
    )

  ) %>%
  mutate(

    # Four-level perceived safety scale
    sensacao_seg_casa = case_when(
      muito_seguro_casa == 1 ~ 1,
      um_pouco_seguro_casa == 1 ~ 2,
      um_pouco_inseguro_casa == 1 ~ 3,
      muito_inseguro_casa == 1 ~ 4,
      TRUE ~ NA_real_
    ),

    # Binary indicator of feeling unsafe at home
    inseguro_casa = case_when(
      um_pouco_inseguro_casa == 1 |
        muito_inseguro_casa == 1 ~ 1,

      um_pouco_seguro_casa == 1 |
        muito_seguro_casa == 1 ~ 0,

      TRUE ~ NA_real_
    )

  )


################################################################################
# 4.6 Perceived neighborhood safety
################################################################################

Base_geral <- Base_geral %>%
  mutate(
    p152_num = as.numeric(p152),
    p153_num = as.numeric(p153)
  )

################################################################################
# Neighborhood safety during the day
################################################################################

Base_geral <- Base_geral %>%
  mutate(

    muito_seguro_bairro_dia = case_when(
      p152_num == 1 ~ 1,
      p152_num %in% c(2, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),

    um_pouco_seguro_bairro_dia = case_when(
      p152_num == 2 ~ 1,
      p152_num %in% c(1, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),

    um_pouco_inseguro_bairro_dia = case_when(
      p152_num == 4 ~ 1,
      p152_num %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    ),

    muito_inseguro_bairro_dia = case_when(
      p152_num == 3 ~ 1,
      p152_num %in% c(1, 2, 4) ~ 0,
      TRUE ~ NA_real_
    ),

    sensacao_seg_bairro = case_when(
      muito_seguro_bairro_dia == 1 ~ 1,
      um_pouco_seguro_bairro_dia == 1 ~ 2,
      um_pouco_inseguro_bairro_dia == 1 ~ 3,
      muito_inseguro_bairro_dia == 1 ~ 4,
      TRUE ~ NA_real_
    ),

    inseguro_bairro_dia = case_when(
      um_pouco_inseguro_bairro_dia == 1 |
        muito_inseguro_bairro_dia == 1 ~ 1,

      um_pouco_seguro_bairro_dia == 1 |
        muito_seguro_bairro_dia == 1 ~ 0,

      TRUE ~ NA_real_
    )

  )

################################################################################
# Neighborhood safety at night
################################################################################

Base_geral <- Base_geral %>%
  mutate(

    muito_seguro_bairro_noite = case_when(
      p153_num == 1 ~ 1,
      p153_num %in% c(2, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),

    um_pouco_seguro_bairro_noite = case_when(
      p153_num == 2 ~ 1,
      p153_num %in% c(1, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),

    um_pouco_inseguro_bairro_noite = case_when(
      p153_num == 4 ~ 1,
      p153_num %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    ),

    muito_inseguro_bairro_noite = case_when(
      p153_num == 3 ~ 1,
      p153_num %in% c(1, 2, 4) ~ 0,
      TRUE ~ NA_real_
    ),

    sensacao_seg_bairro_noite = case_when(
      muito_seguro_bairro_noite == 1 ~ 1,
      um_pouco_seguro_bairro_noite == 1 ~ 2,
      um_pouco_inseguro_bairro_noite == 1 ~ 3,
      muito_inseguro_bairro_noite == 1 ~ 4,
      TRUE ~ NA_real_
    ),

    inseguro_bairro_noite = case_when(
      um_pouco_inseguro_bairro_noite == 1 |
        muito_inseguro_bairro_noite == 1 ~ 1,

      um_pouco_seguro_bairro_noite == 1 |
        muito_seguro_bairro_noite == 1 ~ 0,

      TRUE ~ NA_real_
    )

  )



################################################################################
# 4.7 Fear of crime
################################################################################

Base_geral <- Base_geral %>%
  mutate(

    # Fear of being robbed in the neighborhood
    medo_assalto_roubo_viz = case_when(
      p163a == 1 | p163b == 1 ~ 1,

      (p163a %in% c(2, 96)) &
        (p163b %in% c(2, 96) | is.na(p163b)) ~ 0,

      TRUE ~ NA_real_
    ),

    # Fear of home burglary/robbery
    medo_assalto_roubo_casa = case_when(
      p166a == 1 ~ 1,
      p166a == 2 ~ 0,
      TRUE ~ NA_real_
    )

  )

################################################################################
# 4.8 Public lighting
################################################################################

Base_geral <- Base_geral %>%
  mutate(

    p148b_num = as.numeric(p148b),

    # Excellent lighting
    iluminacao_otima = case_when(
      p148b_num == 1 ~ 1,
      p148b_num %in% 2:6 ~ 0,
      TRUE ~ NA_real_
    ),

    # Good lighting
    iluminacao_boa = case_when(
      p148b_num == 2 ~ 1,
      p148b_num %in% c(1, 3:6) ~ 0,
      TRUE ~ NA_real_
    ),

    # Fair lighting
    iluminacao_regular = case_when(
      p148b_num == 3 ~ 1,
      p148b_num %in% c(1, 2, 4:6) ~ 0,
      TRUE ~ NA_real_
    ),

    # Poor lighting
    iluminacao_ruim = case_when(
      p148b_num == 4 ~ 1,
      p148b_num %in% c(1:3, 5, 6) ~ 0,
      TRUE ~ NA_real_
    ),

    # Very poor lighting
    iluminacao_pess = case_when(
      p148b_num %in% c(5, 6) ~ 1,
      p148b_num %in% 1:4 ~ 0,
      TRUE ~ NA_real_
    ),

    # Ordered lighting scale
    iluminacao_ord = case_when(
      p148b_num == 1 ~ 1,
      p148b_num == 2 ~ 2,
      p148b_num == 3 ~ 3,
      p148b_num == 4 ~ 4,
      p148b_num %in% c(5, 6) ~ 5,
      TRUE ~ NA_real_
    )

  )


################################################################################
# 5. Final analytical dataset
################################################################################

Base_final_iluminacao <- Base_geral %>%
  dplyr::select(

    # Identification
    id_obs,
    peso_usado,

    # Geographic characteristics
    sudeste, sul, nordeste, centro_oeste, norte,
    capital, viz_capital, interior,
    porte_muito_grande, porte_grande,
    porte_medio, porte_pequeno,

    # Demographic characteristics
    n_adultos, n_criancas, n_moradores,
    idade_16_24, idade_25_34,
    idade_35_44, idade_45_59,
    idade_60_mais,
    Anos_mesma_cidade,
    Anos_mesma_vizinhanca,
    Mesma_cidade,
    mulher,
    casado_uniao,
    branco,

    # Education
    escol_sem_instr,
    escol_ate_fund_completo,
    escol_ate_med_completo,
    escol_ate_pos_grad,
    escolc_sem_instr,
    escolc_ate_fund_completo,
    escolc_ate_med_completo,
    escolc_ate_pos_grad,

    # Labor market
    pea_dummy,

    # Income
    sal_min_ate_1,
    sal_min_de_1_ate_2_sm,
    sal_min_de_2_ate_3_sm,
    sal_min_de_3_ate_5_sm,
    sal_min_acima_5,
    ln_renda_pc,

    # Daily routine and mobility
    casa_manha,
    casa_tarde,
    casa_noite,
    casa_madrugada,
    saidas_total,
    sem_saida_noturna,
    evita_noite_vio,
    sem_bar_noturno,
    trans_carro,
    trans_moto,
    trans_onibus,
    trans_bike,
    trans_ape,
    trans_outros,

    # Housing
    apartamento,
    casa,
    barraco_outros,

    # Wealth
    indice_riqueza_z,

    # Victimization
    furto,
    furto_12meses,
    furto_12meses_casa_bairro,
    roubo,
    roubo_12meses,
    roubo_12meses_casa_bairro,
    acidente_12meses,
    vitimizacao_12meses_casa_bairro,

    # Neighborhood violence
    agressao_viz,
    assaltados_viz,
    assassinados_viz,
    violencia_viz,

    # Neighborhood disorder (Broken Windows)
    locais_abandonados,
    carros_abandonados,
    terrenos_abandonados,
    tiros,
    cheiro_ruim,
    ruidos,

    # Household security measures
    grades,
    alarme,
    camera,
    cerca,

    # Perceived safety
    inseguro_casa,
    sensacao_seg_casa,
    inseguro_bairro_dia,
    sensacao_seg_bairro,
    inseguro_bairro_noite,
    sensacao_seg_bairro_noite,
    medo_assalto_roubo_viz,
    medo_assalto_roubo_casa,

    # Public lighting
    iluminacao_otima,
    iluminacao_boa,
    iluminacao_regular,
    iluminacao_ruim,
    iluminacao_pess,
    iluminacao_ord
  )

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
