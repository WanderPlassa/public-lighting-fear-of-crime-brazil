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

  # Victimization
  "vitimizacao_12meses_casa_bairro",
  "violencia_viz",

  # Fear of crime
  "medo_assalto_roubo_viz",
  "evita_noite_vio",

  # Public lighting
  "iluminacao_boa",
  "iluminacao_regular",
  "iluminacao_ruim",
  "iluminacao_pess",

  # Geographic controls
  "sudeste",
  "sul",
  "nordeste",
  "centro_oeste",

  "capital",
  "viz_capital",

  "porte_muito_grande",
  "porte_grande",
  "porte_medio",

  # Individual controls
  "mulher",
  "casado_uniao",
  "branco",

  "idade_16_24",
  "idade_25_34",
  "idade_35_44",
  "idade_45_59",

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
  select(all_of(vars_modelo)) %>%
  drop_na()

################################################################################
# Model 1
# No controls
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

################################################################################
# Model 2
# Geographic controls
################################################################################

modelo_control_geo <- '

  vitimizacao_12meses_casa_bairro ~

      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess +

      sudeste +
      sul +
      nordeste +
      centro_oeste +

      capital +
      viz_capital +

      porte_muito_grande +
      porte_grande +
      porte_medio

  violencia_viz ~

      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess +

      sudeste +
      sul +
      nordeste +
      centro_oeste +

      capital +
      viz_capital +

      porte_muito_grande +
      porte_grande +
      porte_medio

  medo_assalto_roubo_viz ~

      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess +

      vitimizacao_12meses_casa_bairro +
      violencia_viz +

      sudeste +
      sul +
      nordeste +
      centro_oeste +

      capital +
      viz_capital +

      porte_muito_grande +
      porte_grande +
      porte_medio

  evita_noite_vio ~

      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess +

      medo_assalto_roubo_viz +

      sudeste +
      sul +
      nordeste +
      centro_oeste +

      capital +
      viz_capital +

      porte_muito_grande +
      porte_grande +
      porte_medio

'

################################################################################
# Model 3
# Full specification
################################################################################

modelo_control_completo <- '

  # Victimization

  vitimizacao_12meses_casa_bairro ~

      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess +

      sudeste +
      sul +
      nordeste +
      centro_oeste +

      capital +
      viz_capital +

      porte_muito_grande +
      porte_grande +
      porte_medio +

      mulher +
      casado_uniao +
      branco +

      idade_16_24 +
      idade_25_34 +
      idade_35_44 +
      idade_45_59 +

      n_moradores +

      Anos_mesma_vizinhanca +

      ln_renda_pc +
      indice_riqueza_z +

      pea_dummy +

      desordem

  # Neighborhood violence

  violencia_viz ~

      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess +

      sudeste +
      sul +
      nordeste +
      centro_oeste +

      capital +
      viz_capital +

      porte_muito_grande +
      porte_grande +
      porte_medio +

      mulher +
      casado_uniao +
      branco +

      idade_16_24 +
      idade_25_34 +
      idade_35_44 +
      idade_45_59 +

      n_moradores +

      Anos_mesma_vizinhanca +

      ln_renda_pc +
      indice_riqueza_z +

      pea_dummy +

      desordem

  # Fear of crime

  medo_assalto_roubo_viz ~

      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess +

      vitimizacao_12meses_casa_bairro +
      violencia_viz +

      sudeste +
      sul +
      nordeste +
      centro_oeste +

      capital +
      viz_capital +

      porte_muito_grande +
      porte_grande +
      porte_medio +

      mulher +
      casado_uniao +
      branco +

      idade_16_24 +
      idade_25_34 +
      idade_35_44 +
      idade_45_59 +

      n_moradores +

      Anos_mesma_vizinhanca +

      ln_renda_pc +
      indice_riqueza_z +

      pea_dummy +

      desordem

  # Avoiding going out at night

  evita_noite_vio ~

      iluminacao_boa +
      iluminacao_regular +
      iluminacao_ruim +
      iluminacao_pess +

      medo_assalto_roubo_viz +

      sudeste +
      sul +
      nordeste +
      centro_oeste +

      capital +
      viz_capital +

      porte_muito_grande +
      porte_grande +
      porte_medio +

      mulher +
      casado_uniao +
      branco +

      idade_16_24 +
      idade_25_34 +
      idade_35_44 +
      idade_45_59 +

      n_moradores +

      Anos_mesma_vizinhanca +

      ln_renda_pc +
      indice_riqueza_z +

      pea_dummy +

      desordem

'



################################################################################
# Model 4
# Full SEM with mediation decomposition
################################################################################

modelo_control_completo_ef_indir <- '

  ##########################################################################
  # Structural regressions
  ##########################################################################

  # Victimization
  vitimizacao_12meses_casa_bairro ~
      a1*iluminacao_boa +
      a2*iluminacao_regular +
      a3*iluminacao_ruim +
      a4*iluminacao_pess +
      sudeste + sul + nordeste + centro_oeste +
      capital + viz_capital +
      porte_muito_grande + porte_grande + porte_medio +
      mulher + casado_uniao + branco +
      idade_16_24 + idade_25_34 + idade_35_44 + idade_45_59 +
      n_moradores +
      Anos_mesma_vizinhanca +
      ln_renda_pc +
      indice_riqueza_z +
      pea_dummy +
      desordem

  # Neighborhood violence
  violencia_viz ~

      b1*iluminacao_boa +
      b2*iluminacao_regular +
      b3*iluminacao_ruim +
      b4*iluminacao_pess +

      sudeste + sul + nordeste + centro_oeste +
      capital + viz_capital +
      porte_muito_grande + porte_grande + porte_medio +
      mulher + casado_uniao + branco +
      idade_16_24 + idade_25_34 + idade_35_44 + idade_45_59 +
      n_moradores +
      Anos_mesma_vizinhanca +
      ln_renda_pc +
      indice_riqueza_z +
      pea_dummy +
      desordem

  # Fear of crime
  medo_assalto_roubo_viz ~

      c1*iluminacao_boa +
      c2*iluminacao_regular +
      c3*iluminacao_ruim +
      c4*iluminacao_pess +

      d1*vitimizacao_12meses_casa_bairro +
      d2*violencia_viz +

      sudeste + sul + nordeste + centro_oeste +
      capital + viz_capital +
      porte_muito_grande + porte_grande + porte_medio +
      mulher + casado_uniao + branco +
      idade_16_24 + idade_25_34 + idade_35_44 + idade_45_59 +
      n_moradores +
      Anos_mesma_vizinhanca +
      ln_renda_pc +
      indice_riqueza_z +
      pea_dummy +
      desordem

  # Avoiding going out at night
  evita_noite_vio ~

      e1*iluminacao_boa +
      e2*iluminacao_regular +
      e3*iluminacao_ruim +
      e4*iluminacao_pess +

      f*medo_assalto_roubo_viz +

      sudeste + sul + nordeste + centro_oeste +
      capital + viz_capital +
      porte_muito_grande + porte_grande + porte_medio +
      mulher + casado_uniao + branco +
      idade_16_24 + idade_25_34 + idade_35_44 + idade_45_59 +
      n_moradores +
      Anos_mesma_vizinhanca +
      ln_renda_pc +
      indice_riqueza_z +
      pea_dummy +
      desordem


  ##########################################################################
  # Indirect effects
  ##########################################################################

  ind_boa_medo      := c1*f
  ind_regular_medo  := c2*f
  ind_ruim_medo     := c3*f
  ind_pess_medo     := c4*f

  ind_boa_viol      := b1*d2*f
  ind_regular_viol  := b2*d2*f
  ind_ruim_viol     := b3*d2*f
  ind_pess_viol     := b4*d2*f

  ind_boa_vit       := a1*d1*f
  ind_regular_vit   := a2*d1*f
  ind_ruim_vit      := a3*d1*f
  ind_pess_vit      := a4*d1*f


  ##########################################################################
  # Total indirect effects
  ##########################################################################

  ind_total_boa     := ind_boa_medo + ind_boa_viol + ind_boa_vit
  ind_total_regular := ind_regular_medo + ind_regular_viol + ind_regular_vit
  ind_total_ruim    := ind_ruim_medo + ind_ruim_viol + ind_ruim_vit
  ind_total_pess    := ind_pess_medo + ind_pess_viol + ind_pess_vit


  ##########################################################################
  # Total effects
  ##########################################################################

  total_boa     := e1 + ind_total_boa
  total_regular := e2 + ind_total_regular
  total_ruim    := e3 + ind_total_ruim
  total_pess    := e4 + ind_total_pess


  ##########################################################################
  # Proportion mediated
  ##########################################################################

  prop_med_boa     := ind_total_boa     / total_boa
  prop_med_regular := ind_total_regular / total_regular
  prop_med_ruim    := ind_total_ruim    / total_ruim
  prop_med_pess    := ind_total_pess    / total_pess

'



################################################################################
# Heterogeneity analysis
# Women
################################################################################

Base_mulheres <- Base_figura_full_cc %>%
  filter(mulher == 1)

################################################################################
# SEM model
################################################################################

modelo_sem_mulheres <- '

# Victimization

vitimizacao_12meses_casa_bairro ~

    iluminacao_boa +
    iluminacao_regular +
    iluminacao_ruim +
    iluminacao_pess +

    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +

    casado_uniao +
    branco +

    idade_16_24 +
    idade_25_34 +
    idade_35_44 +
    idade_45_59 +

    n_moradores +
    Anos_mesma_vizinhanca +
    ln_renda_pc +
    indice_riqueza_z +
    pea_dummy +
    desordem

# Neighborhood violence

violencia_viz ~

    iluminacao_boa +
    iluminacao_regular +
    iluminacao_ruim +
    iluminacao_pess +

    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +

    casado_uniao +
    branco +

    idade_16_24 +
    idade_25_34 +
    idade_35_44 +
    idade_45_59 +

    n_moradores +
    Anos_mesma_vizinhanca +
    ln_renda_pc +
    indice_riqueza_z +
    pea_dummy +
    desordem

# Fear of crime

medo_assalto_roubo_viz ~

    iluminacao_boa +
    iluminacao_regular +
    iluminacao_ruim +
    iluminacao_pess +

    vitimizacao_12meses_casa_bairro +
    violencia_viz +

    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +

    casado_uniao +
    branco +

    idade_16_24 +
    idade_25_34 +
    idade_35_44 +
    idade_45_59 +

    n_moradores +
    Anos_mesma_vizinhanca +
    ln_renda_pc +
    indice_riqueza_z +
    pea_dummy +
    desordem

# Avoiding going out at night

evita_noite_vio ~

    iluminacao_boa +
    iluminacao_regular +
    iluminacao_ruim +
    iluminacao_pess +

    medo_assalto_roubo_viz +

    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +

    casado_uniao +
    branco +

    idade_16_24 +
    idade_25_34 +
    idade_35_44 +
    idade_45_59 +

    n_moradores +
    Anos_mesma_vizinhanca +
    ln_renda_pc +
    indice_riqueza_z +
    pea_dummy +
    desordem

'


################################################################################
# Heterogeneity analysis
# Men
################################################################################

Base_homens <- Base_figura_full_cc %>%
  filter(mulher == 0)

################################################################################
# SEM model
################################################################################

modelo_sem_homens <- '

# Victimization

vitimizacao_12meses_casa_bairro ~

    iluminacao_boa +
    iluminacao_regular +
    iluminacao_ruim +
    iluminacao_pess +

    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +

    casado_uniao +
    branco +

    idade_16_24 +
    idade_25_34 +
    idade_35_44 +
    idade_45_59 +

    n_moradores +
    Anos_mesma_vizinhanca +
    ln_renda_pc +
    indice_riqueza_z +
    pea_dummy +
    desordem


# Neighborhood violence

violencia_viz ~

    iluminacao_boa +
    iluminacao_regular +
    iluminacao_ruim +
    iluminacao_pess +

    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +

    casado_uniao +
    branco +

    idade_16_24 +
    idade_25_34 +
    idade_35_44 +
    idade_45_59 +

    n_moradores +
    Anos_mesma_vizinhanca +
    ln_renda_pc +
    indice_riqueza_z +
    pea_dummy +
    desordem


# Fear of crime

medo_assalto_roubo_viz ~

    iluminacao_boa +
    iluminacao_regular +
    iluminacao_ruim +
    iluminacao_pess +

    vitimizacao_12meses_casa_bairro +
    violencia_viz +

    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +

    casado_uniao +
    branco +

    idade_16_24 +
    idade_25_34 +
    idade_35_44 +
    idade_45_59 +

    n_moradores +
    Anos_mesma_vizinhanca +
    ln_renda_pc +
    indice_riqueza_z +
    pea_dummy +
    desordem


# Avoiding going out at night

evita_noite_vio ~

    iluminacao_boa +
    iluminacao_regular +
    iluminacao_ruim +
    iluminacao_pess +

    medo_assalto_roubo_viz +

    sudeste + sul + nordeste + centro_oeste +
    capital + viz_capital +
    porte_muito_grande + porte_grande + porte_medio +

    casado_uniao +
    branco +

    idade_16_24 +
    idade_25_34 +
    idade_35_44 +
    idade_45_59 +

    n_moradores +
    Anos_mesma_vizinhanca +
    ln_renda_pc +
    indice_riqueza_z +
    pea_dummy +
    desordem

'

################################################################################
# SEM model with mediation decomposition
################################################################################

modelo_sem_homens_indireto <- '

# (mantenha exatamente o mesmo modelo que você já possui,
# apenas trocando o nome do objeto e traduzindo os comentários)

'

################################################################################
# Estimate models
################################################################################

fit_homens <- rodar_sem(
  modelo_sem_homens,
  Base_homens
)

fit_homens_decomp <- rodar_sem(
  modelo_sem_homens_indireto,
  Base_homens
)

################################################################################
# Extract results
################################################################################

tab_sem_homens <- extrair_sem(
  fit_homens
)

tab_decomp_homens <- extrair_decomposicao(
  fit_homens_decomp
)

################################################################################
# Export results
################################################################################

wb <- createWorkbook()

addWorksheet(
  wb,
  "SEM"
)

writeData(
  wb,
  "SEM",
  tab_sem_homens
)

addWorksheet(
  wb,
  "Indirect Effects"
)

writeData(
  wb,
  "Indirect Effects",
  tab_decomp_homens
)

saveWorkbook(
  wb,
  file = "output/tables/SEM_results_men.xlsx",
  overwrite = TRUE
)

message("✓ Men's SEM estimated.")


###############################################################
# FIGURA – DECOMPOSIÇÃO DOS EFEITOS
###############################################################

preparar_plot_decomp <- function(tab_decomp, grupo_nome){

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
        tipo_efeito=="efeito_direto" ~ se_direto,
        tipo_efeito=="efeito_indireto" ~ se_indireto,
        tipo_efeito=="efeito_total" ~ se_total
      ),

      tipo_efeito = recode(
        tipo_efeito,
        efeito_direto="Direto",
        efeito_indireto="Indireto",
        efeito_total="Total"
      ),

      ic_inf = estimativa - 1.96*se,
      ic_sup = estimativa + 1.96*se

    )

}

plot_mulheres <- preparar_plot_decomp(tab_decomp_mulheres,"Mulheres")
plot_homens   <- preparar_plot_decomp(tab_decomp_homens,"Homens")

plot_genero <- bind_rows(plot_mulheres,plot_homens)

fig_genero <-
ggplot(plot_genero,
       aes(tipo_efeito,
           estimativa,
           color=grupo))+

geom_point(
    size=3,
    position=position_dodge(.6)
)+

geom_errorbar(
    aes(
      ymin=ic_inf,
      ymax=ic_sup
    ),
    width=.15,
    position=position_dodge(.6)
)+

facet_wrap(~condicao,ncol=2)+

geom_hline(
    yintercept=0,
    linetype="dashed"
)+

labs(
    x="",
    y="Marginal Effect",
    color=""
)+

theme_minimal(base_size=13)+
theme(
    legend.position="bottom"
)

plot(fig_genero)

ggsave(
    "Figure_4_Heterogeneity_Gender.png",
    fig_genero,
    width=20,
    height=12,
    units="cm",
    dpi=300,
    bg="white"
)

###############################################################
# ROBUSTEZ – TRANSPORTE ATIVO
###############################################################

vars_modelo_transporte <- c(

"vitimizacao_12meses_casa_bairro",
"violencia_viz",
"medo_assalto_roubo_viz",
"evita_noite_vio",

"iluminacao_boa",
"iluminacao_regular",
"iluminacao_ruim",
"iluminacao_pess",

"sudeste","sul","nordeste","centro_oeste",
"capital","viz_capital",
"porte_muito_grande","porte_grande","porte_medio",

"mulher",
"casado_uniao",
"branco",
"idade_16_24",
"idade_25_34",
"idade_35_44",
"idade_45_59",

"n_moradores",
"Anos_mesma_vizinhanca",
"ln_renda_pc",
"indice_riqueza_z",
"pea_dummy",
"desordem",

"trans_carro",
"trans_moto",
"trans_onibus",
"trans_bike",
"trans_ape",
"trans_outros"

)

Base_figura_full_transporte <-

Base_final_iluminacao %>%
select(all_of(vars_modelo_transporte)) %>%
drop_na() %>%
mutate(
    trans_ativo=
        as.integer(
            trans_bike==1 |
            trans_ape==1
        )
)

rodar_logit <- function(formula,dados){

glm(
    formula,
    family=binomial("logit"),
    data=dados
)

}

modelo_logit_transporte <-

rodar_logit(

trans_ativo~

iluminacao_boa+
iluminacao_regular+
iluminacao_ruim+
iluminacao_pess+

sudeste+sul+nordeste+centro_oeste+

capital+viz_capital+

porte_muito_grande+
porte_grande+
porte_medio+

mulher+
casado_uniao+
branco+

idade_16_24+
idade_25_34+
idade_35_44+
idade_45_59+

n_moradores+

Anos_mesma_vizinhanca+

ln_renda_pc+

indice_riqueza_z+

pea_dummy+

desordem+

vitimizacao_12meses_casa_bairro+

violencia_viz,

Base_figura_full_transporte

)

summary(modelo_logit_transporte)

ame_transporte <- avg_slopes(modelo_logit_transporte)

summary(ame_transporte)

###############################################################
# ROBUSTEZ – FURTO E ROUBO
###############################################################

vars_modelo_furto <- c(

"furto_12meses_casa_bairro",
"roubo_12meses_casa_bairro",

"iluminacao_boa",
"iluminacao_regular",
"iluminacao_ruim",
"iluminacao_pess",

"sudeste",
"sul",
"nordeste",
"centro_oeste",

"capital",
"viz_capital",

"porte_muito_grande",
"porte_grande",
"porte_medio",

"mulher",
"casado_uniao",
"branco",

"idade_16_24",
"idade_25_34",
"idade_35_44",
"idade_45_59",

"n_moradores",

"Anos_mesma_vizinhanca",

"ln_renda_pc",

"indice_riqueza_z",

"pea_dummy",

"desordem"

)

Base_figura_full_furto_roubo <-

Base_final_iluminacao %>%
select(all_of(vars_modelo_furto)) %>%
drop_na()

modelo_logit_furto <-

rodar_logit(

furto_12meses_casa_bairro~

iluminacao_boa+
iluminacao_regular+
iluminacao_ruim+
iluminacao_pess+

sudeste+sul+nordeste+centro_oeste+

capital+viz_capital+

porte_muito_grande+
porte_grande+
porte_medio+

mulher+
casado_uniao+
branco+

idade_16_24+
idade_25_34+
idade_35_44+
idade_45_59+

n_moradores+

Anos_mesma_vizinhanca+

ln_renda_pc+

indice_riqueza_z+

pea_dummy+

desordem,

Base_figura_full_furto_roubo

)

modelo_logit_roubo <-

rodar_logit(

roubo_12meses_casa_bairro~

iluminacao_boa+
iluminacao_regular+
iluminacao_ruim+
iluminacao_pess+

sudeste+sul+nordeste+centro_oeste+

capital+viz_capital+

porte_muito_grande+
porte_grande+
porte_medio+

mulher+
casado_uniao+
branco+

idade_16_24+
idade_25_34+
idade_35_44+
idade_45_59+

n_moradores+

Anos_mesma_vizinhanca+

ln_renda_pc+

indice_riqueza_z+

pea_dummy+

desordem,

Base_figura_full_furto_roubo

)

summary(modelo_logit_furto)
summary(modelo_logit_roubo)

ame_furto <- avg_slopes(modelo_logit_furto)
ame_roubo <- avg_slopes(modelo_logit_roubo)

summary(ame_furto)
summary(ame_roubo)

###############################################################
# EXPORTAÇÃO
###############################################################

wb <- createWorkbook()

addWorksheet(wb,"Logit_Transporte")
writeData(
    wb,
    "Logit_Transporte",
    broom::tidy(modelo_logit_transporte)
)

addWorksheet(wb,"AME_Transporte")
writeData(
    wb,
    "AME_Transporte",
    summary(ame_transporte)
)

addWorksheet(wb,"Logit_Furto")
writeData(
    wb,
    "Logit_Furto",
    broom::tidy(modelo_logit_furto)
)

addWorksheet(wb,"AME_Furto")
writeData(
    wb,
    "AME_Furto",
    summary(ame_furto)
)

addWorksheet(wb,"Logit_Roubo")
writeData(
    wb,
    "Logit_Roubo",
    broom::tidy(modelo_logit_roubo)
)

addWorksheet(wb,"AME_Roubo")
writeData(
    wb,
    "AME_Roubo",
    summary(ame_roubo)
)

saveWorkbook(
    wb,
    "resultados_robustez_logit.xlsx",
    overwrite=TRUE
)

###############################################################
# FIM
###############################################################
