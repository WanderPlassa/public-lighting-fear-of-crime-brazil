#### 4) Estimações  -----

Base_final_iluminacao <-
  readRDS("data/processed/Base_final_iluminacao.rds")


names(Base_final_iluminacao)


vars_modelo <- c(
  # Vitimizacao e violencia
  "vitimizacao_12meses_casa_bairro", "violencia_viz", 
  
  # Medo e saída
  "medo_assalto_roubo_viz", "evita_noite_vio",
  
  # Iluminação
  "iluminacao_boa", "iluminacao_regular",
  "iluminacao_ruim", "iluminacao_pess",
  
  # Região / localização
  "sudeste", "sul", "nordeste", "centro_oeste",
  "capital", "viz_capital",
  "porte_muito_grande", "porte_grande", "porte_medio",
  
  # Controles
  "mulher", "casado_uniao", "branco", "idade_16_24", 
  "idade_25_34", "idade_35_44", "idade_45_59", "n_moradores",
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


# Estimações Gerais 


modelo_sem_control <- '
  # Regressões estruturais
  vitimizacao_12meses_casa_bairro ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess
  violencia_viz   ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess

  medo_assalto_roubo_viz ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess + vitimizacao_12meses_casa_bairro + violencia_viz

  evita_noite_vio ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess + medo_assalto_roubo_viz
'

 
# model <- glm(violencia_viz ~ iluminacao_boa + iluminacao_regular + 
#     iluminacao_ruim + iluminacao_pess, data = Base_figura_full_cc,
#   family = binomial(link = "logit")
# )
# 
# summary(model)
#  
# library(margins)
# 
# mfx <- margins(model)
# summary(mfx)
# 
# R> summary(mfx)
# factor             AME     SE       z      p   lower  upper
# iluminacao_boa.  0.0127 0.0071  1.7814  0.0749 -0.0013 0.0266
# iluminacao_regular 0.1232 0.0076 16.2424 0.0000  0.1083 0.1380
# iluminacao_ruim  0.1702 0.0094 18.0988 0.0000  0.1518 0.1887
# iluminacao_pess  0.1962 0.0094 20.8837  0.0000  0.1777 0.2146



modelo_control_geo <- '
  # Regressões estruturais (com controles geográficos)
  vitimizacao_12meses_casa_bairro ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio

  violencia_viz ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio

  medo_assalto_roubo_viz ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
    vitimizacao_12meses_casa_bairro + violencia_viz +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio

  evita_noite_vio ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
    medo_assalto_roubo_viz +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio
'


 

modelo_control_completo <- '
  # Regressões estruturais (com todos controles)

  # Vitimização
  vitimizacao_12meses_casa_bairro ~  iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +
    mulher + casado_uniao + branco + idade_16_24 +  idade_25_34  +  idade_35_44  +  idade_45_59  +
    n_moradores +
    Anos_mesma_vizinhanca + ln_renda_pc + indice_riqueza_z + pea_dummy +
    desordem

  # Violência no entorno
  violencia_viz ~  iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +
    mulher + casado_uniao + branco + idade_16_24 +  idade_25_34  +  idade_35_44  +  idade_45_59  +
     n_moradores +
    Anos_mesma_vizinhanca + ln_renda_pc + indice_riqueza_z + pea_dummy +
    desordem

  # Medo do crime
  medo_assalto_roubo_viz ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
    vitimizacao_12meses_casa_bairro + violencia_viz +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +
    mulher + casado_uniao + branco + idade_16_24 +  idade_25_34  +  idade_35_44  +  idade_45_59  +
    n_moradores +
    Anos_mesma_vizinhanca + ln_renda_pc + indice_riqueza_z + pea_dummy +
    desordem

  # Saídas de casa
  evita_noite_vio ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
    medo_assalto_roubo_viz +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +
    mulher + casado_uniao + branco + idade_16_24 +  idade_25_34  +  idade_35_44  +  idade_45_59  +
    n_moradores +
    Anos_mesma_vizinhanca + ln_renda_pc + indice_riqueza_z + pea_dummy + desordem
'
 

modelo_control_completo_ef_indir <- '

  #  ------------------
  # Regressões estruturais
  # -------------------

  # Vitimização
  vitimizacao_12meses_casa_bairro ~     a1*iluminacao_boa + a2*iluminacao_regular + a3*iluminacao_ruim + a4*iluminacao_pess +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +
    mulher + casado_uniao + branco + idade_16_24 +  idade_25_34  +  idade_35_44  +  idade_45_59  +
    n_moradores +
    Anos_mesma_vizinhanca + ln_renda_pc + indice_riqueza_z + pea_dummy +
    desordem

  # Violência
  violencia_viz ~     b1*iluminacao_boa + b2*iluminacao_regular + b3*iluminacao_ruim + b4*iluminacao_pess +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +
    mulher + casado_uniao + branco + idade_16_24 +  idade_25_34  +  idade_35_44  +  idade_45_59  +
    n_moradores +
    Anos_mesma_vizinhanca + ln_renda_pc + indice_riqueza_z + pea_dummy +
    desordem

  # Medo
  medo_assalto_roubo_viz ~     c1*iluminacao_boa + c2*iluminacao_regular + c3*iluminacao_ruim + c4*iluminacao_pess +
    d1*vitimizacao_12meses_casa_bairro + d2*violencia_viz +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +
    mulher + casado_uniao + branco + idade_16_24 +  idade_25_34  +  idade_35_44  +  idade_45_59  +
    n_moradores +
    Anos_mesma_vizinhanca + ln_renda_pc + indice_riqueza_z + pea_dummy +
    desordem

  # Evitar sair à noite
  evita_noite_vio ~     e1*iluminacao_boa + e2*iluminacao_regular + e3*iluminacao_ruim + e4*iluminacao_pess +
    f*medo_assalto_roubo_viz +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +
    mulher + casado_uniao + branco + idade_16_24 +  idade_25_34  +  idade_35_44  +  idade_45_59  +
    n_moradores +
    Anos_mesma_vizinhanca + ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

  # -------------------
  # EFEITOS INDIRETOS
  # -------------------

  # Iluminação -> medo -> evita
  ind_boa_medo      := c1 * f
  ind_regular_medo  := c2 * f
  ind_ruim_medo     := c3 * f
  ind_pess_medo     := c4 * f

  # Iluminação -> violencia_viz -> medo -> evita
  ind_boa_viol      := b1 * d2 * f
  ind_regular_viol  := b2 * d2 * f
  ind_ruim_viol     := b3 * d2 * f
  ind_pess_viol     := b4 * d2 * f

  # Iluminação -> vitimizacao_12meses_casa_bairro -> medo -> evita
  ind_boa_vit       := a1 * d1 * f
  ind_regular_vit   := a2 * d1 * f
  ind_ruim_vit      := a3 * d1 * f
  ind_pess_vit      := a4 * d1 * f

  # -------------------
  # EFEITO INDIRETO TOTAL
  # -------------------

  ind_total_boa     := ind_boa_medo + ind_boa_viol + ind_boa_vit
  ind_total_regular := ind_regular_medo + ind_regular_viol + ind_regular_vit
  ind_total_ruim    := ind_ruim_medo + ind_ruim_viol + ind_ruim_vit
  ind_total_pess    := ind_pess_medo + ind_pess_viol + ind_pess_vit

  # -------------------
  # EFEITO TOTAL
  # -------------------

  total_boa     := e1 + ind_total_boa
  total_regular := e2 + ind_total_regular
  total_ruim    := e3 + ind_total_ruim
  total_pess    := e4 + ind_total_pess
  
  # -------------------
  # PROPORÇÃO MEDIADA
  # -------------------

  prop_med_boa     := ind_total_boa     / total_boa
  prop_med_regular := ind_total_regular / total_regular
  prop_med_ruim    := ind_total_ruim    / total_ruim
  prop_med_pess    := ind_total_pess    / total_pess  
'

 

# Rodar SEM
rodar_sem <- function(modelo, dados) {
  sem(
    modelo,
    data = dados,
    estimator = "MLR",
    missing = "fiml"
  )
}

# Extrair parâmetros estruturais
extrair_sem <- function(fit) {
  lavaan::parameterEstimates(
    fit,
    standardized = TRUE
  ) %>%
    filter(op %in% c("~", ":=")) %>%
    select(lhs, op, rhs, est, se, pvalue)
}

# Extrair decomposição
extrair_decomposicao <- function(fit) {
  
  tab_all <- lavaan::parameterEstimates(
    fit,
    standardized = TRUE
  )
  
  direto <- tab_all %>%
    filter(op=="~",
           lhs=="evita_noite_vio",
           rhs %in% c("iluminacao_boa",
                      "iluminacao_regular",
                      "iluminacao_ruim",
                      "iluminacao_pess")) %>%
    mutate(condicao = gsub("iluminacao_", "", rhs)) %>%
    select(condicao, est, se, pvalue)
  
  indireto <- tab_all %>%
    filter(op==":=", grepl("^ind_total_", lhs)) %>%
    mutate(condicao = gsub("ind_total_", "", lhs)) %>%
    select(condicao, est, se, pvalue)
  
  total <- tab_all %>%
    filter(op==":=", grepl("^total_", lhs)) %>%
    mutate(condicao = gsub("total_", "", lhs)) %>%
    select(condicao, est, se, pvalue)
  
  prop <- tab_all %>%
    filter(op==":=", grepl("^prop_med_", lhs)) %>%
    mutate(condicao = gsub("prop_med_", "", lhs)) %>%
    select(condicao, est, se, pvalue)
  
  tabela <- direto %>%
    rename(efeito_direto = est,
           se_direto = se,
           p_direto = pvalue) %>%
    left_join(indireto %>%
                rename(efeito_indireto = est,
                       se_indireto = se,
                       p_indireto = pvalue),
              by="condicao") %>%
    left_join(total %>%
                rename(efeito_total = est,
                       se_total = se,
                       p_total = pvalue),
              by="condicao") %>%
    left_join(prop %>%
                rename(proporcao_mediada = est,
                       se_prop = se,
                       p_prop = pvalue),
              by="condicao") %>%
    mutate(condicao = dplyr::recode(
      condicao,
      "boa"     = "Iluminação boa",
      "regular" = "Iluminação regular",
      "ruim"    = "Iluminação ruim",
      "pess"    = "Iluminação péssima"
    ))
  
  return(tabela)
}



fit_sem_simples   <- rodar_sem(modelo_sem_control, Base_figura_full_cc)
fit_sem_geo       <- rodar_sem(modelo_control_geo, Base_figura_full_cc)
fit_sem_completo  <- rodar_sem(modelo_control_completo, Base_figura_full_cc)

fit_sem_decomp <- rodar_sem(
  modelo_control_completo_ef_indir,
  Base_figura_full_cc
)



tab_sem_simples  <- extrair_sem(fit_sem_simples)
tab_sem_geo      <- extrair_sem(fit_sem_geo)
tab_sem_completo <- extrair_sem(fit_sem_completo)

tab_decomp <- extrair_decomposicao(fit_sem_decomp)



wb <- createWorkbook()

addWorksheet(wb, "SEM_sem_controle")
writeData(wb, "SEM_sem_controle", tab_sem_simples)

addWorksheet(wb, "SEM_control_geo")
writeData(wb, "SEM_control_geo", tab_sem_geo)

addWorksheet(wb, "SEM_control_completo")
writeData(wb, "SEM_control_completo", tab_sem_completo)

addWorksheet(wb, "Decomposicao_efeitos")
writeData(wb, "Decomposicao_efeitos", tab_decomp)

saveWorkbook(
  wb,
  file = "resultados_iluminacao_SEM.xlsx",
  overwrite = TRUE
)






# Heterogeneidade: mulher vs homem
 
# Mulheres


Base_mulheres <- Base_figura_full_cc %>%
  filter(mulher == 1)


modelo_control_completo_mulheres <- '

vitimizacao_12meses_casa_bairro ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

violencia_viz ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

medo_assalto_roubo_viz ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
  vitimizacao_12meses_casa_bairro + violencia_viz +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

evita_noite_vio ~ iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
  medo_assalto_roubo_viz +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem
'



modelo_control_completo_ef_indir_mulheres <- '

vitimizacao_12meses_casa_bairro ~ a1*iluminacao_boa + a2*iluminacao_regular +
  a3*iluminacao_ruim + a4*iluminacao_pess +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

violencia_viz ~ b1*iluminacao_boa + b2*iluminacao_regular +
  b3*iluminacao_ruim + b4*iluminacao_pess +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

medo_assalto_roubo_viz ~ c1*iluminacao_boa + c2*iluminacao_regular +
  c3*iluminacao_ruim + c4*iluminacao_pess +
  d1*vitimizacao_12meses_casa_bairro + d2*violencia_viz +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

evita_noite_vio ~ e1*iluminacao_boa + e2*iluminacao_regular +
  e3*iluminacao_ruim + e4*iluminacao_pess +
  f*medo_assalto_roubo_viz +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

ind_total_boa     := c1*f + b1*d2*f + a1*d1*f
ind_total_regular := c2*f + b2*d2*f + a2*d1*f
ind_total_ruim    := c3*f + b3*d2*f + a3*d1*f
ind_total_pess    := c4*f + b4*d2*f + a4*d1*f

total_boa     := e1 + ind_total_boa
total_regular := e2 + ind_total_regular
total_ruim    := e3 + ind_total_ruim
total_pess    := e4 + ind_total_pess

prop_med_boa     := ind_total_boa / total_boa
prop_med_regular := ind_total_regular / total_regular
prop_med_ruim    := ind_total_ruim / total_ruim
prop_med_pess    := ind_total_pess / total_pess
'


fit_mulheres <- sem(
  modelo_control_completo_mulheres,
  data = Base_mulheres,
  estimator = "MLR",
  missing = "fiml"
)

fit_mulheres_decomp <- sem(
  modelo_control_completo_ef_indir_mulheres,
  data = Base_mulheres,
  estimator = "MLR",
  missing = "fiml"
)


tab_sem_mulheres <- parameterEstimates(fit_mulheres, standardized = FALSE)

tab_decomp_mulheres <- extrair_decomposicao(fit_mulheres_decomp)


wb <- createWorkbook()

addWorksheet(wb, "Mulheres_SEM_Completo")
writeData(wb, "Mulheres_SEM_Completo", tab_sem_mulheres)

addWorksheet(wb, "Mulheres_Decomposicao")
writeData(wb, "Mulheres_Decomposicao", tab_decomp_mulheres)

saveWorkbook(
  wb,
  file = "resultados_SEM_mulheres.xlsx",
  overwrite = TRUE
)


# Homens

Base_homens <- Base_figura_full_cc %>%
  filter(mulher == 0)


modelo_control_completo_homens <- '

vitimizacao_12meses_casa_bairro ~ iluminacao_boa + iluminacao_regular +
  iluminacao_ruim + iluminacao_pess +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

violencia_viz ~ iluminacao_boa + iluminacao_regular +
  iluminacao_ruim + iluminacao_pess +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

medo_assalto_roubo_viz ~ iluminacao_boa + iluminacao_regular +
  iluminacao_ruim + iluminacao_pess +
  vitimizacao_12meses_casa_bairro + violencia_viz +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

evita_noite_vio ~ iluminacao_boa + iluminacao_regular +
  iluminacao_ruim + iluminacao_pess +
  medo_assalto_roubo_viz +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem
'


modelo_control_completo_ef_indir_homens <- '

vitimizacao_12meses_casa_bairro ~ a1*iluminacao_boa + a2*iluminacao_regular +
  a3*iluminacao_ruim + a4*iluminacao_pess +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

violencia_viz ~ b1*iluminacao_boa + b2*iluminacao_regular +
  b3*iluminacao_ruim + b4*iluminacao_pess +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

medo_assalto_roubo_viz ~ c1*iluminacao_boa + c2*iluminacao_regular +
  c3*iluminacao_ruim + c4*iluminacao_pess +
  d1*vitimizacao_12meses_casa_bairro + d2*violencia_viz +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

evita_noite_vio ~ e1*iluminacao_boa + e2*iluminacao_regular +
  e3*iluminacao_ruim + e4*iluminacao_pess +
  f*medo_assalto_roubo_viz +
  sudeste + sul + nordeste + centro_oeste +
  capital + viz_capital +
  porte_muito_grande + porte_grande + porte_medio +
  casado_uniao + branco + idade_16_24 + idade_25_34 +
  idade_35_44 + idade_45_59 +
  n_moradores + Anos_mesma_vizinhanca +
  ln_renda_pc + indice_riqueza_z + pea_dummy + desordem

ind_total_boa     := c1*f + b1*d2*f + a1*d1*f
ind_total_regular := c2*f + b2*d2*f + a2*d1*f
ind_total_ruim    := c3*f + b3*d2*f + a3*d1*f
ind_total_pess    := c4*f + b4*d2*f + a4*d1*f

total_boa     := e1 + ind_total_boa
total_regular := e2 + ind_total_regular
total_ruim    := e3 + ind_total_ruim
total_pess    := e4 + ind_total_pess

prop_med_boa     := ind_total_boa / total_boa
prop_med_regular := ind_total_regular / total_regular
prop_med_ruim    := ind_total_ruim / total_ruim
prop_med_pess    := ind_total_pess / total_pess
'


fit_homens <- sem(
  modelo_control_completo_homens,
  data = Base_homens,
  estimator = "MLR",
  missing = "fiml"
)

fit_homens_decomp <- sem(
  modelo_control_completo_ef_indir_homens,
  data = Base_homens,
  estimator = "MLR",
  missing = "fiml"
)


tab_sem_homens <- parameterEstimates(
  fit_homens,
  standardized = FALSE
)

tab_decomp_homens <- extrair_decomposicao(
  fit_homens_decomp
)


wb <- createWorkbook()

addWorksheet(wb, "Homens_SEM_Completo")
writeData(wb, "Homens_SEM_Completo", tab_sem_homens)

addWorksheet(wb, "Homens_Decomposicao")
writeData(wb, "Homens_Decomposicao", tab_decomp_homens)

saveWorkbook(
  wb,
  file = "resultados_SEM_homens.xlsx",
  overwrite = TRUE
)




preparar_plot_decomp <- function(tab_decomp, grupo_nome) {
  
  tab_decomp %>%
    mutate(grupo = grupo_nome) %>%
    select(
      grupo,
      condicao,
      efeito_direto, se_direto,
      efeito_indireto, se_indireto,
      efeito_total, se_total
    ) %>%
    pivot_longer(
      cols = starts_with("efeito_"),
      names_to = "tipo_efeito",
      values_to = "estimativa"
    ) %>%
    mutate(
      se = case_when(
        tipo_efeito == "efeito_direto"   ~ se_direto,
        tipo_efeito == "efeito_indireto" ~ se_indireto,
        tipo_efeito == "efeito_total"    ~ se_total
      ),
      tipo_efeito = dplyr::recode(
        tipo_efeito,
        "efeito_direto"   = "Direto",
        "efeito_indireto" = "Indireto",
        "efeito_total"    = "Total"
      ),
      ic_inf = estimativa - 1.96 * se,
      ic_sup = estimativa + 1.96 * se
    )
}


 
plot_mulheres <- preparar_plot_decomp(tab_decomp_mulheres, "Mulheres")
plot_homens   <- preparar_plot_decomp(tab_decomp_homens, "Homens")



plot_genero <- bind_rows(plot_mulheres, plot_homens)


 

ggplot(plot_genero,
       aes(x = tipo_efeito,
           y = estimativa,
           color = grupo)) +
  
  geom_point(
    position = position_dodge(width = 0.6),
    size = 3
  ) +
  
  geom_errorbar(
    aes(ymin = ic_inf, ymax = ic_sup),
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  
  facet_wrap(~ condicao, ncol = 2) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(
    x = "",
    y = "Efeito marginal",
    color = ""
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom"
  )




names(Base_final_iluminacao)

 

vars_modelo <- c(
  # Vitimizacao e violencia
  "vitimizacao_12meses_casa_bairro", "violencia_viz", 
  
  # Medo e saída
  "medo_assalto_roubo_viz", "evita_noite_vio",
  
  # Iluminação
  "iluminacao_boa", "iluminacao_regular",
  "iluminacao_ruim", "iluminacao_pess",
  
  # Região / localização
  "sudeste", "sul", "nordeste", "centro_oeste",
  "capital", "viz_capital",
  "porte_muito_grande", "porte_grande", "porte_medio",
  
  # Controles
  "mulher", "casado_uniao", "branco", "idade_16_24", 
  "idade_25_34", "idade_35_44", "idade_45_59", "n_moradores",
  "Anos_mesma_vizinhanca", "ln_renda_pc",
  "indice_riqueza_z", "pea_dummy", "desordem", 
  
  # Transporte
  "trans_carro", "trans_moto",  "trans_onibus", "trans_bike", "trans_ape",                      
   "trans_outros"  
  
)


for (v in vars_modelo) {
  cat("\n====================================\n")
  cat("Variável:", v, "\n")
  print(table(Base_final_iluminacao[[v]], useNA = "ifany"))
}

Base_figura_full_transporte <- Base_final_iluminacao %>%
  dplyr::select(all_of(vars_modelo)) %>%
  tidyr::drop_na() 




Base_figura_full_transporte <- Base_figura_full_transporte %>%
  mutate(trans_ativo = ifelse(trans_bike == 1 | trans_ape == 1, 1, 0))



table(Base_figura_full_transporte$trans_ativo, useNA = "ifany")


modelo_logit_transporte <- glm(
  trans_ativo ~
    iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +
    mulher + casado_uniao + branco + idade_16_24 +  idade_25_34  +  idade_35_44  +  idade_45_59  +
    n_moradores +
    Anos_mesma_vizinhanca + ln_renda_pc + indice_riqueza_z + pea_dummy +
    desordem + vitimizacao_12meses_casa_bairro + violencia_viz,
  family = binomial(link = "logit"),
  data = Base_figura_full_transporte ) 
 

summary(modelo_logit_transporte)




# Furto e Roubo separados

names(Base_final_iluminacao)



vars_modelo <- c(
  # Vitimizacao e violencia
  "vitimizacao_12meses_casa_bairro", "violencia_viz", 
  "furto_12meses_casa_bairro", "roubo_12meses_casa_bairro",
  
  # Medo e saída
  "medo_assalto_roubo_viz", "evita_noite_vio",
  
  # Iluminação
  "iluminacao_boa", "iluminacao_regular",
  "iluminacao_ruim", "iluminacao_pess",
  
  # Região / localização
  "sudeste", "sul", "nordeste", "centro_oeste",
  "capital", "viz_capital",
  "porte_muito_grande", "porte_grande", "porte_medio",
  
  # Controles
  "mulher", "casado_uniao", "branco", "idade_16_24", 
  "idade_25_34", "idade_35_44", "idade_45_59", "n_moradores",
  "Anos_mesma_vizinhanca", "ln_renda_pc",
  "indice_riqueza_z", "pea_dummy", "desordem", 
  
  # Transporte
  "trans_carro", "trans_moto",  "trans_onibus", "trans_bike", "trans_ape",                      
  "trans_outros"  
  
)


for (v in vars_modelo) {
  cat("\n====================================\n")
  cat("Variável:", v, "\n")
  print(table(Base_final_iluminacao[[v]], useNA = "ifany"))
}

Base_figura_full_furto_roubo <- Base_final_iluminacao %>%
  dplyr::select(all_of(vars_modelo)) %>%
  tidyr::drop_na() 


table(Base_figura_full_furto_roubo$furto_12meses_casa_bairro, useNA = "ifany")
table(Base_figura_full_furto_roubo$roubo_12meses_casa_bairro, useNA = "ifany")

modelo_logit_furto <- glm(
  furto_12meses_casa_bairro ~  iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +
    mulher + casado_uniao + branco + idade_16_24 +  idade_25_34  +  idade_35_44  +  idade_45_59  +
    n_moradores +
    Anos_mesma_vizinhanca + ln_renda_pc + indice_riqueza_z + pea_dummy +
    desordem,
  family = binomial(link = "logit"),
  data = Base_figura_full_furto_roubo ) 


summary(modelo_logit_furto)


modelo_logit_roubo <- glm(
  roubo_12meses_casa_bairro ~  iluminacao_boa + iluminacao_regular + iluminacao_ruim + iluminacao_pess +
    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +
    mulher + casado_uniao + branco + idade_16_24 +  idade_25_34  +  idade_35_44  +  idade_45_59  +
    n_moradores +
    Anos_mesma_vizinhanca + ln_renda_pc + indice_riqueza_z + pea_dummy +
    desordem,
  family = binomial(link = "logit"),
  data = Base_figura_full_furto_roubo ) 


summary(modelo_logit_roubo)
 

 

ame_furto <- avg_slopes(modelo_logit_furto)
summary(ame_furto)

ame_roubo <- avg_slopes(modelo_logit_roubo)
summary(ame_roubo)




# FIM


