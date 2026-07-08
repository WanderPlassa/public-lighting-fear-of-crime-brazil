################################################################################
# 05 - Structural Equation Models
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
# Variables used in SEM analyses
################################################################################

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

################################################################################
# Complete-case sample
################################################################################

Base_figura_full_cc <- Base_final_iluminacao %>%
  select(all_of(vars_modelo)) %>%
  drop_na()

################################################################################
# Model 
################################################################################

modelo_sem_control <- '

  vitimizacao_12meses_casa_bairro ~
      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess

  violencia_viz ~
      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess

  medo_assalto_roubo_viz ~
      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess +
      vitimizacao_12meses_casa_bairro +
      violencia_viz

  evita_noite_vio ~
      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess +
      medo_assalto_roubo_viz

'

for (v in vars_modelo) {
  cat("\n====================================\n")
  cat("Variável:", v, "\n")
  print(table(Base_final_iluminacao[[v]], useNA = "ifany"))
}


Base_figura_full_cc <- Base_final_iluminacao %>%
  dplyr::select(all_of(vars_modelo)) %>%
  tidyr::drop_na() 


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
  file = "output/tables/resultados_iluminacao_SEM.xlsx",
  overwrite = TRUE
)


################################################################################
# Men vs Woman 
################################################################################

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
  file = "output/tables/resultados_SEM_mulheres.xlsx",
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
  file = "output/tables/resultados_SEM_homens.xlsx",
  overwrite = TRUE
)





###############################################################
# FIM
###############################################################
