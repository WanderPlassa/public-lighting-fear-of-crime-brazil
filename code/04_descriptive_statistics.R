#### 3) Descritiva  -----

Base_final_iluminacao <-
  readRDS("data/processed/Base_final_iluminacao.rds")


str(Base_final_iluminacao)
names(Base_final_iluminacao)


# Índice simples de segurança domiciliar
Base_final_iluminacao <- Base_final_iluminacao %>%
  mutate(
    seg_dom = grades + alarme + camera + cerca
  )

table(Base_final_iluminacao$seg_dom, useNA = "ifany")

# Índice simples de desordem (broken windows)
Base_final_iluminacao <- Base_final_iluminacao %>%
  mutate(
    desordem = locais_abandonados + carros_abandonados +
      terrenos_abandonados + tiros + cheiro_ruim + ruidos
  )

table(Base_final_iluminacao$desordem, useNA = "ifany")


# Decisão formal (como declarar no banco)

# Variavel iluminacao como factor
Base_final_iluminacao <- Base_final_iluminacao %>%
  mutate(
    iluminacao_f = factor(
      iluminacao_ord,
      levels = c(1, 2, 3, 4, 5),
      labels = c("otima", "boa", "regular", "ruim", "pessima")
    )
  )

 

vars_modelo <- c(
  # Vitimizacao e violencia
  "vitimizacao_12meses_casa_bairro", "violencia_viz", 
  
  # Medo e saída
  "medo_assalto_roubo_viz", "evita_noite_vio",
  
  # Iluminação
  "iluminacao_otima", "iluminacao_boa", "iluminacao_regular",
  "iluminacao_ruim", "iluminacao_pess","iluminacao_f", "iluminacao_ord",
  
  # Região / localização
  "sudeste", "sul", "nordeste", "centro_oeste", "norte",
  "capital", "viz_capital", "interior",
  "porte_muito_grande", "porte_grande", "porte_medio", "porte_pequeno",
  
  # Controles
  "mulher", "casado_uniao", "branco", "idade_16_24", 
  "idade_25_34", "idade_35_44", "idade_45_59", "idade_60_mais", "n_moradores",
  "Anos_mesma_vizinhanca", "ln_renda_pc",
  "indice_riqueza_z", "pea_dummy", "desordem"
)


for (v in vars_modelo) {
  cat("\n====================================\n")
  cat("Variável:", v, "\n")
  print(table(Base_final_iluminacao[[v]], useNA = "ifany"))
}

Base_figura_full_cc <- Base_final_iluminacao %>%
  dplyr::select(all_of(vars_modelo)) %>%
  tidyr::drop_na()


vars_descritiva <- c(
  # Resultados
  "vitimizacao_12meses_casa_bairro",
  "violencia_viz",
  "medo_assalto_roubo_viz",
  "evita_noite_vio",
  
  # Iluminação (referência = ótima)
  "iluminacao_otima",
  "iluminacao_boa",
  "iluminacao_regular",
  "iluminacao_ruim",
  "iluminacao_pess"
)


 

estat_desc <- function(x) {
  c(
    Media = mean(x, na.rm = TRUE),
    DP    = sd(x, na.rm = TRUE),
    N     = sum(!is.na(x))
  )
}



tabela_descritiva_sexo <- Base_figura_full_cc %>%
  mutate(
    sexo = case_when(
      mulher == 1 ~ "Mulheres",
      mulher == 0 ~ "Homens"
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
    
    Media_Homens = mean(valor[sexo == "Homens"], na.rm = TRUE),
    DP_Homens    = sd(valor[sexo == "Homens"], na.rm = TRUE),
    N_Homens     = sum(!is.na(valor[sexo == "Homens"])),
    
    Media_Mulheres = mean(valor[sexo == "Mulheres"], na.rm = TRUE),
    DP_Mulheres    = sd(valor[sexo == "Mulheres"], na.rm = TRUE),
    N_Mulheres     = sum(!is.na(valor[sexo == "Mulheres"]))
  ) %>%
  ungroup()



tabela_descritiva_sexo <- tabela_descritiva_sexo %>%
  mutate(
    Variável = dplyr::recode(
      variavel,
      vitimizacao_12meses_casa_bairro = "Vitimização (12 meses, casa/bairro)",
      violencia_viz = "Violência no entorno",
      medo_assalto_roubo_viz = "Medo de assalto ou roubo",
      evita_noite_vio = "Evita sair à noite por violência",
      iluminacao_otima = "Iluminação ótima",
      iluminacao_boa = "Iluminação boa",
      iluminacao_regular = "Iluminação regular",
      iluminacao_ruim = "Iluminação ruim",
      iluminacao_pess = "Iluminação péssima"
    ),
    
    across(
      c(Media_Total, DP_Total,
        Media_Homens, DP_Homens,
        Media_Mulheres, DP_Mulheres),
      ~ round(.x * 100, 1)
    )
  ) %>%
  select(
    Variável,
    Media_Total, DP_Total, N_Total,
    Media_Homens, DP_Homens, N_Homens,
    Media_Mulheres, DP_Mulheres, N_Mulheres
  )


Base_figura_full_cc %>%
  summarise(
    across(
      all_of(vars_descritiva),
      ~ sum(.x == 1, na.rm = TRUE)
    )
  )

 
Base_figura_full_cc %>%
  group_by(mulher) %>%
  summarise(
    across(
      all_of(vars_descritiva),
      ~ sum(.x == 1, na.rm = TRUE)
    )
  )





wb <- createWorkbook()
addWorksheet(wb, "Descritiva_por_sexo")
writeData(wb, "Descritiva_por_sexo", tabela_descritiva_sexo)

saveWorkbook(
  wb,
  file = "tabela_descritiva_variaveis_centrais_por_sexo.xlsx",
  overwrite = TRUE
)


names(Base_figura_full_cc)



vars_controle <- c(
  "idade_16_24", "idade_25_34", "idade_35_44", "idade_45_59", "idade_60_mais", 
  "casado_uniao", "branco",
  "n_moradores", "Anos_mesma_vizinhanca",
  "ln_renda_pc", "indice_riqueza_z", "pea_dummy",
  "capital", "viz_capital", "interior", 
  "porte_muito_grande", "porte_grande", "porte_medio", "porte_pequeno",
  "sudeste", "sul", "nordeste", "centro_oeste", "norte",
  "desordem"
)

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




tabela_controle <- map_dfr(vars_controle, function(v){
  
  is_continua <- v %in% c(
    "ln_renda_pc", "indice_riqueza_z",
    "n_moradores", "Anos_mesma_vizinhanca", "desordem"
  )
  
  f <- if (is_continua) descritiva_continua else descritiva_simples
  
  geral   <- f(Base_figura_full_cc, v)
  homens  <- f(filter(Base_figura_full_cc, mulher == 0), v)
  mulheres<- f(filter(Base_figura_full_cc, mulher == 1), v)
  
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



tabela_controle <- tabela_controle %>%
  mutate(
    Variavel = dplyr::recode(
      Variavel,
      "idade_16_24" = "Idade 16–24",
      "idade_25_34" = "Idade 25–34",
      "idade_35_44" = "Idade 35–44",
      "idade_45_59" = "Idade 45–59",
      "idade_60_mais" = "Idade 60+",
      "casado_uniao" = "Casado ou união estável",
      "branco" = "Branco ou amarelo",
      "n_moradores" = "Número de moradores",
      "Anos_mesma_vizinhanca" = "Anos na mesma vizinhança",
      "ln_renda_pc" = "Log da renda domiciliar per capita",
      "indice_riqueza_z" = "Índice de riqueza domiciliar",
      "pea_dummy" = "Economicamente ativo",
      "capital" = "Capital",
      "viz_capital" = "Município vizinho à capital",
      "interior" = "Município no interior",
      "porte_muito_grande" = "Município muito grande",
      "porte_grande" = "Município grande",
      "porte_medio" = "Município médio",
      "porte_pequeno" = "Município pequeno",
      "sudeste" = "Sudeste",
      "sul" = "Sul",
      "nordeste" = "Nordeste",
      "norte" = "Norte",
      "centro_oeste" = "Centro-Oeste",
      "desordem" = "Índice de desordem urbana"
    )
  )

 

tabela_controle <- tabela_controle %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

 

wb <- createWorkbook()

addWorksheet(wb, "Descritiva_controles")

writeData(
  wb,
  sheet = "Descritiva_controles",
  x = tabela_controle,
  startRow = 1,
  startCol = 1,
  headerStyle = createStyle(textDecoration = "bold")
)


saveWorkbook(
  wb,
  file = "tabela_descritiva_controles_genero.xlsx",
  overwrite = TRUE
)


 

 # FIGURE 2  

 # Preparing data

base_plot <- Base_figura_full_cc %>%
  
  mutate(
    
    iluminacao_f = factor(
      iluminacao_ord,
      levels = 1:5,
      labels = c(
        "Excellent",
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


# Long format

base_long <- base_plot %>%
  
  pivot_longer(
    
    cols = -iluminacao_f,
    
    names_to = "variable",
    
    values_to = "proportion"
    
  )



# Labels

base_long <- base_long %>%
  
  mutate(
    
    variable = recode(
      
      variable,
      
      "victimization" = "Victimization",
      
      "neighborhood_violence" = "Neighborhood Violence",
      
      "fear" = "Fear of Robbery or Assault",
      
      "avoiding" = "Avoiding Going Out at Night"
      
    )
    
  )



# Only first and last values

labels_extremos <- base_long %>%
  
  group_by(variable) %>%
  
  filter(
    iluminacao_f %in% c(
      "Optimal",
      "Very Poor"
    )
  ) %>%
  
  ungroup()



# Figure
 

# Preparing data

base_plot <- Base_figura_full_cc %>%
  mutate(
    iluminacao_f = factor(
      iluminacao_ord,
      levels = 1:5,
      labels = c(
        "Excellent",
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

# Long format

base_long <- base_plot %>%
  pivot_longer(
    cols = -iluminacao_f,
    names_to = "variable",
    values_to = "proportion"
  )

# Labels used in the figure

base_long <- base_long %>%
  mutate(
    variable = recode(
      variable,
      "victimization" = "Victimization",
      "neighborhood_violence" = "Neighborhood Violence",
      "fear" = "Fear of Crime",
      "avoiding" = "Avoiding Going Out at Night"
    )
  )

# Figure

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
  
  # Avoiding Going Out at Night
  
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
  
  # Fear of Robbery or Assault
  
  geom_text(
    data = subset(
      base_long,
      variable == "Fear of Robbery or Assault"
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
  
  # Neighborhood Violence
  
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
    expand = expansion(
      mult = c(0.02, 0.08)
    )
  ) +
  
  labs(
    x = "Perceived Public Lighting Quality",
    y = "Proportion (%)",
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
      10, 10, 10, 10
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

# Display

plot(fig_iluminacao)

# Save

ggsave(
  filename = "Figure_2_Public_Lighting_Crime_Outcomes.png",
  plot = fig_iluminacao,
  width = 22,
  height = 12,
  units = "cm",
  dpi = 300,
  bg = "white"
)

