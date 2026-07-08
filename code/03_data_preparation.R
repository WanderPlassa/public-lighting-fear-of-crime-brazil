#### 2) Data Wrangling (ajuste da base)  -----


#### 2.1) Caracteristicas Fisicas e Geo  -----
# O peso para cada individuo

str(Base_geral$peso)

Base_geral <- Base_geral %>%
  mutate(peso_usado = peso)


 
# Geograficas

# Regiao
str(Base_geral$regiao)
table(Base_geral$regiao, useNA = "ifany")

# cada dummy = 1 para a região específica, 0 para as demais, NA se regiao estiver faltando.
Base_geral <- Base_geral %>%
  mutate(
    sudeste      = as.integer(regiao == 1),
    sul          = as.integer(regiao == 2),
    nordeste     = as.integer(regiao == 3),
    centro_oeste = as.integer(regiao == 4),
    norte        = as.integer(regiao == 5)
  )


# Metropolitano
str(Base_geral$metrop)
table(Base_geral$metrop, useNA = "ifany")

# metrop == 1 → capital
# metrop == 2 → vizinha da capital
# metrop == 3 → interior

Base_geral <- Base_geral %>%
  mutate(
    capital     = as.integer(metrop == 1),
    viz_capital = as.integer(metrop == 2),
    interior    = as.integer(metrop == 3)
  )

# Porte
str(Base_geral$metrop)
table(Base_geral$porte_mun, useNA = "ifany")

# porte_mun == 5 → porte_muito_grande (acima de 500 mil)
# porte_mun == 4 → porte_grande (300 a 500 mil)
# porte_mun == 3 → porte_medio (50 a 300 mil)
# porte_mun %in% {0,1,2} → porte_pequeno (menos de 50 mil)

Base_geral <- Base_geral %>%
  mutate(
    porte_muito_grande = as.integer(porte_mun == 5),
    porte_grande       = as.integer(porte_mun == 4),
    porte_medio        = as.integer(porte_mun == 3),
    porte_pequeno      = as.integer(porte_mun %in% c(0, 1, 2))
  )


# Composicao domiciliar

# Quantas pessoas
table(Base_geral$p1, useNA = "ifany")

# Quantas mulheres
table(Base_geral$p1a, useNA = "ifany")

# Quantas criancas 
table(Base_geral$p2a, useNA = "ifany")

str(Base_geral$p1)
str(Base_geral$p1a)
str(Base_geral$p2a)

# Transformando os 96 e 97 em zero
Base_geral <- Base_geral %>% 
  mutate(
    p1  = ifelse(p1  == 96, 0, p1),   # adultos 16+
    p1a = ifelse(p1a == 96, 0, p1a),  # mulheres 16+
    p2a = ifelse(p2a == 96, 0, p2a),   # menores de 16
    p2a = ifelse(p2a == 97, 0, p2a)    # menores de 16
  )

table(Base_geral$p1, useNA = "ifany")
table(Base_geral$p1a, useNA = "ifany")
table(Base_geral$p2a, useNA = "ifany")


# Quantidade de moradores 
Base_geral <- Base_geral %>%
  mutate(
    n_adultos   = p1,           
    n_criancas  = p2a,
    n_moradores = n_adultos + n_criancas
  )

table(Base_geral$n_moradores, useNA = "ifany")


# Idade  
str(Base_geral$idatab)
table(Base_geral$idatab, useNA = "ifany")
table(Base_geral$idatab, Base_geral$pea, useNA = "ifany")
 
Base_geral <- Base_geral %>%
  mutate(
    # 16–24
    idade_16_24 = NA_real_,
    idade_16_24 = ifelse(idatab == 1, 1, idade_16_24),
    idade_16_24 = ifelse(idatab %in% c(2, 3, 4, 5), 0, idade_16_24),
    
    # 25–34
    idade_25_34 = NA_real_,
    idade_25_34 = ifelse(idatab == 2, 1, idade_25_34),
    idade_25_34 = ifelse(idatab %in% c(1, 3, 4, 5), 0, idade_25_34),
    
    # 35–44
    idade_35_44 = NA_real_,
    idade_35_44 = ifelse(idatab == 3, 1, idade_35_44),
    idade_35_44 = ifelse(idatab %in% c(1, 2, 4, 5), 0, idade_35_44),
    
    # 45–59
    idade_45_59 = NA_real_,
    idade_45_59 = ifelse(idatab == 4, 1, idade_45_59),
    idade_45_59 = ifelse(idatab %in% c(1, 2, 3, 5), 0, idade_45_59),
    
    # 60+
    idade_60_mais = NA_real_,
    idade_60_mais = ifelse(idatab == 5, 1, idade_60_mais),
    idade_60_mais = ifelse(idatab %in% c(1, 2, 3, 4), 0, idade_60_mais)
  )


 
# Criando dummy mesma cidade se o individuo sempre esteve na mesma cidade
#    1 se 0.5 (sim), 0 se 99999 (não), NA se 99997 (missing)

str(Base_geral$p3)
table(Base_geral$p3, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    Mesma_cidade = case_when(
      p3 == 0.5   ~ 1,
      p3 == 99999 ~ 0,
      p3 == 99997 ~ NA_real_,
      TRUE        ~ NA_real_          
    )
  )


#  Transformando p4ano (quantos anos mora na cidade) em missing se p4ano >95

table(Base_geral$p4ano, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    p4ano = ifelse(p4ano > 95, NA, p4ano)
  )


table(Base_geral$p4ano, useNA = "ifany")

 
# Transformando p139ano (mesma vizinhança) missing se p139ano > 95
 
str(Base_geral$p139ano)
attributes(Base_geral$p139ano)
table( Base_geral$p139ano ,  useNA="ifany") 

# Essa variavel nos faz perder bastante gente 6842
Base_geral <- Base_geral %>%
  mutate(
    p139ano = ifelse(p139ano > 95, NA, p139ano)
  )

table(Base_geral$p139ano, useNA = "ifany")

# Variaveis anos na mesma cidade e anos na mesma vizinhanca
Base_geral <- Base_geral %>%
  mutate(
    Anos_mesma_cidade   = p4ano,           
    Anos_mesma_vizinhanca  = p139ano 
  )


# Para quem teve Anos_mesma_vizinhanca > Anos_mesma_cidade
# fazer com que Anos_mesma_vizinhanca = Anos_mesma_cidade
Base_geral <- Base_geral %>%
  mutate(
    Anos_mesma_vizinhanca = ifelse(
      !is.na(Anos_mesma_cidade) &
        !is.na(Anos_mesma_vizinhanca) &
        Anos_mesma_vizinhanca > Anos_mesma_cidade,
      Anos_mesma_cidade,
      Anos_mesma_vizinhanca
    )
  )

# Estado Civil - Casado Uniao Estavel
str(Base_geral$p5)
attributes(Base_geral$p5)
table(Base_geral$p5, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    casado_uniao = case_when(
      p5 %in% c(2, 3) ~ 1,   # casado ou união consensual
      p5 %in% c(1, 4, 5, 6) ~ 0,   # demais situações
      TRUE ~ NA_real_        # missing
    )
  )

table(Base_geral$casado_uniao, useNA = "ifany")


# Sexo - Mulher
str(Base_geral$sexotab)
attributes(Base_geral$sexotab)
table(Base_geral$sexotab, useNA = "ifany")
 
Base_geral <- Base_geral %>%
  mutate(
    mulher = case_when(
      sexotab %in% c(2) ~ 1,                      # Mulher
      sexotab %in% c(1) ~ 0,                      # Homem
      TRUE ~ NA_real_                             # missing
    )
  )


table(Base_geral$mulher, useNA = "ifany")

# Cor - Branco/Amarelo
str(Base_geral$cor)
attributes(Base_geral$cor)
table(Base_geral$cor, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    branco = case_when(
      cor %in% c(1, 4) ~ 1,                      # branco ou amarelo
      cor %in% c(2, 3, 5, 6) ~ 0,                 # demais categorias
      TRUE ~ NA_real_                             # missing
    )
  )

# BRANCO = 1 para categorias 1 (Branca) e 4 (Amarela)
# NÃO BRANCO = 0 para 2 (Preta), 3 (Parda), 5 (Indígena), 6 (Morena)

table(Base_geral$branco, useNA = "ifany")


# Escolaridade da pessoa e do chefe da familia
str(Base_geral$escola)
attributes(Base_geral$escola)
table(Base_geral$escola, useNA = "ifany")

str(Base_geral$escolac)
attributes(Base_geral$escolac)
table(Base_geral$escolac, useNA = "ifany")

 
# Escolaridade
Base_geral <- Base_geral %>% 
  mutate(
    escol_sem_instr = case_when(
      escola == 1 ~ 1,
      escola %in% 2:8 ~ 0,
      TRUE ~ NA_real_
    ),
    
    escol_ate_fund_completo = case_when(
      escola %in% c(2,3) ~ 1,
      escola %in% c(1,4,5,6,7,8) ~ 0,
      TRUE ~ NA_real_
    ),
    
    escol_ate_med_completo = case_when(
      escola %in% c(4,5) ~ 1,
      escola %in% c(1,2,3,6,7,8) ~ 0,
      TRUE ~ NA_real_
    ),
    
    escol_ate_pos_grad = case_when(
      escola %in% c(6,7,8) ~ 1,
      escola %in% c(1,2,3,4,5) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Escolaridade do chefe
Base_geral <- Base_geral %>% 
  mutate(
    # Chefe sem instrução
    escolc_sem_instr = case_when(
      escolac == 1 ~ 1,
      escolac %in% 2:8 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Chefe até fundamental completo
    escolc_ate_fund_completo = case_when(
      escolac %in% c(2, 3) ~ 1,
      escolac %in% c(1, 4, 5, 6, 7, 8) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Chefe até médio completo
    escolc_ate_med_completo = case_when(
      escolac %in% c(4, 5) ~ 1,
      escolac %in% c(1, 2, 3, 6, 7, 8) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Chefe até pós-graduação
    escolc_ate_pos_grad = case_when(
      escolac %in% c(6, 7, 8) ~ 1,
      escolac %in% c(1, 2, 3, 4, 5) ~ 0,
      TRUE ~ NA_real_
    )
  )


table(Base_geral$escol_sem_instr, Base_geral$escolc_sem_instr, useNA = "ifany")
table(Base_geral$escol_ate_fund_completo, Base_geral$escolc_ate_fund_completo, useNA = "ifany")
table(Base_geral$escol_ate_med_completo, Base_geral$escolc_ate_med_completo, useNA = "ifany")
table(Base_geral$escol_ate_pos_grad, Base_geral$escolc_ate_pos_grad, useNA = "ifany")

# Pop economicamente ativa (PEA)
str(Base_geral$pea)
attributes(Base_geral$pea)
table(Base_geral$pea , useNA="ifany")

 
Base_geral <- Base_geral %>%
  mutate(
    pea_dummy = case_when(
      pea %in% c(1:10) ~ 1,   # PEA (ocupados + desempregado que procura)
      pea %in% c(11:16) ~ 0,  # Não PEA
      TRUE ~ NA_real_
    )
  )

table(Base_geral$pea_dummy , useNA="ifany")

# Salario 

# renda total mensal de todas as pessoas que moram neste domicílio
str(Base_geral$rendaf)
attributes(Base_geral$rendaf)
table(Base_geral$rendaf, useNA = "ifany")

# Nessa var perdemos bastante tambem
table(Base_geral$n_moradores, useNA = "ifany")


Base_geral <- Base_geral %>%
  mutate(
    sal_min_ate_1           = ifelse(rendaf == 1, 1, ifelse(rendaf %in% 2:8, 0, NA)),
    sal_min_de_1_ate_2_sm   = ifelse(rendaf == 2, 1, ifelse(rendaf %in% c(1,3:8), 0, NA)),
    sal_min_de_2_ate_3_sm   = ifelse(rendaf == 3, 1, ifelse(rendaf %in% c(1,2,4:8), 0, NA)),
    sal_min_de_3_ate_5_sm   = ifelse(rendaf == 4, 1, ifelse(rendaf %in% c(1:3,5:8), 0, NA)),
    sal_min_acima_5         = ifelse(rendaf %in% 5:8, 1, ifelse(rendaf %in% 1:4, 0, NA))
  )

table(Base_geral$sal_min_ate_1, Base_geral$rendaf, useNA="ifany")
table(Base_geral$sal_min_de_1_ate_2_sm, Base_geral$rendaf,  useNA="ifany")
table(Base_geral$sal_min_de_2_ate_3_sm, Base_geral$rendaf, useNA="ifany")
table(Base_geral$sal_min_de_3_ate_5_sm, Base_geral$rendaf,  useNA="ifany")
table(Base_geral$sal_min_acima_5, Base_geral$rendaf, useNA="ifany")

# Proxy de renda per capita (o problema da anterior e que domicilios
# com numero de moradores maior tende a ter renda maior)
Base_geral <- Base_geral %>%
  mutate(
    renda_sm_aprox = case_when(
      rendaf == 1 ~ 0.5,
      rendaf == 2 ~ 1.5,
      rendaf == 3 ~ 2.5,
      rendaf == 4 ~ 4,
      rendaf == 5 ~ 7.5,
      rendaf == 6 ~ 12.5,
      rendaf == 7 ~ 17.5,
      rendaf == 8 ~ 22.5,
      TRUE ~ NA_real_
    )
  )

# per capita
Base_geral <- Base_geral %>%
  mutate(
    renda_pc_sm = renda_sm_aprox / n_moradores
  )

# tirando o log 
Base_geral <- Base_geral %>%
  mutate(
    ln_renda_pc = log(renda_pc_sm)
  )

hist(Base_geral$ln_renda_pc)

# Onde fica a maior parte do Dia
# Segunda a Sexta
 

str(Base_geral$p13b)
attributes(Base_geral$p13b)
table(Base_geral$p13b ,  useNA="ifany")

str(Base_geral$p13c)
attributes(Base_geral$p13c)
table(Base_geral$p13c , useNA="ifany")

str(Base_geral$p13d)
attributes(Base_geral$p13d)
table(Base_geral$p13d ,  useNA="ifany") 


Base_geral <- Base_geral %>%
  mutate(
    casa_manha      = ifelse(p13a == 1, 1,
                             ifelse(p13a == 2, 0, NA)),
    casa_tarde      = ifelse(p13b == 1, 1,
                             ifelse(p13b == 2, 0, NA)),
    casa_noite      = ifelse(p13c == 1, 1,
                             ifelse(p13c == 2, 0, NA)),
    casa_madrugada  = ifelse(p13d == 1, 1,
                             ifelse(p13d == 2, 0, NA))
  )


table(Base_geral$p13a, Base_geral$casa_manha  , useNA="ifany")
table(Base_geral$p13b, Base_geral$casa_tarde ,  useNA="ifany")
table(Base_geral$p13c, Base_geral$casa_noite , useNA="ifany")
table(Base_geral$p13d, Base_geral$casa_madrugada ,  useNA="ifany") 


# Saiu para onde nos ultimos 30 dias

str(Base_geral$p14a)
attributes(Base_geral$p14a)
table(Base_geral$p14a,  useNA="ifany")
table(Base_geral$p14b,  useNA="ifany")
table(Base_geral$p14c,  useNA="ifany")
table(Base_geral$p14d,  useNA="ifany")
table(Base_geral$p14e,  useNA="ifany")
table(Base_geral$p14f,   useNA="ifany")
table(Base_geral$p14g,  useNA="ifany")
table(Base_geral$p14h,   useNA="ifany")
table(Base_geral$p14i,  useNA="ifany")
table(Base_geral$p14j,  useNA="ifany")
table(Base_geral$p14k,  useNA="ifany")
table(Base_geral$p162a , useNA="ifany")


itens_p14 <- c("p14a","p14b","p14c","p14d","p14e",
             "p14h","p14i","p14j","p14k")



Base_geral <- Base_geral %>%
  mutate(across(all_of(itens_p14),
                ~ case_when(
                  .x == 1 ~ 1,                 # Sim
                  .x == 2 ~ 0,                 # Não
                  .x %in% c(97, 99) ~ NA_real_,
                  TRUE ~ NA_real_
                )))

Base_geral$saidas_total <- rowSums(Base_geral[itens_p14], na.rm = TRUE)


table(Base_geral$saidas_total, useNA="ifany") 


# Alguma saida, que estou classificando como noturna (proxy, pois nao sei)

# cinema (p14a)
# comer fora (p14c)
# visitar amigos/parente (p14d)
# evento esportivo ao vivo (p14e)
# show/espetáculo (p14j)
# bar ou casa noturna (p14k)

table(Base_geral$p14a , useNA="ifany")
table(Base_geral$p14c, useNA="ifany")
table(Base_geral$p14d,  useNA="ifany")
table(Base_geral$p14e,  useNA="ifany")
table(Base_geral$p14j, useNA="ifany")
table(Base_geral$p14k, useNA="ifany")

Base_geral <- Base_geral %>%
  mutate(
    sem_saida_noturna = case_when(
      p14a == 1 | p14c == 1 | p14d == 1 |
        p14e == 1 | p14j == 1 | p14k == 1 ~ 0,
      
      p14a == 0 & p14c == 0 & p14d == 0 &
        p14e == 0 & p14j == 0 & p14k == 0 ~ 1,
      
      TRUE ~ NA_real_
    )
  )

table(Base_geral$sem_saida_noturna, useNA="ifany") 

# Evita sair a noite por conta da violencia
str(Base_geral$p162a)
attributes(Base_geral$p162a)
table(Base_geral$p162a,  useNA="ifany")

Base_geral <- Base_geral %>%
  mutate(
    evita_noite_vio = case_when(
      p162a == 1 ~ 1,
      p162a == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$sem_saida_noturna, Base_geral$evita_noite_vio , useNA="ifany")
table(Base_geral$evita_noite_vio, useNA="ifany")


# Sem bar noturno
table(Base_geral$p14k, useNA="ifany")

Base_geral <- Base_geral %>%
  mutate(
    sem_bar_noturno = case_when(
      p14k == 0 ~ 1,
      p14k == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$sem_bar_noturno, useNA="ifany")

# Meio de transporte
str(Base_geral$p15a)
attributes(Base_geral$p15a)
table(Base_geral$p15a , useNA="ifany")
 

Base_geral <- Base_geral %>%
  mutate(
    trans_carro  = case_when(p15a == 1 ~ 1,
                             p15a %in% c(2,3,4,5,6,7,8,9,10) ~ 0,
                             TRUE ~ NA_real_),
    
    trans_moto   = case_when(p15a == 2 ~ 1,
                             p15a %in% c(1,3,4,5,6,7,8,9,10) ~ 0,
                             TRUE ~ NA_real_),
    
    trans_onibus = case_when(p15a == 3 ~ 1,
                             p15a %in% c(1,2,4,5,6,7,8,9,10) ~ 0,
                             TRUE ~ NA_real_),
    
    trans_bike   = case_when(p15a == 9 ~ 1,
                             p15a %in% c(1,2,3,4,5,6,7,8,10) ~ 0,
                             TRUE ~ NA_real_),
    
    trans_ape    = case_when(p15a == 10 ~ 1,
                             p15a %in% c(1,2,3,4,5,6,7,8,9) ~ 0,
                             TRUE ~ NA_real_),
    
    # OUTROS: 4, 5, 6, 7, 8
    trans_outros = case_when(p15a %in% c(4,5,6,7,8) ~ 1,
                             p15a %in% c(1,2,3,9,10) ~ 0,
                             TRUE ~ NA_real_)  # 11,17,18,97,98 → NA
  )


table(Base_geral$trans_carro, Base_geral$p15a , useNA="ifany")
table( Base_geral$trans_moto, Base_geral$p15a ,  useNA="ifany")
table( Base_geral$trans_onibus, Base_geral$p15a , useNA="ifany")
table( Base_geral$trans_bike, Base_geral$p15a ,  useNA="ifany") 
table( Base_geral$trans_ape, Base_geral$p15a ,  useNA="ifany") 
table( Base_geral$trans_outros, Base_geral$p15a ,  useNA="ifany") 



# Residencia
table( Base_geral$Anos_mesma_cidade ,  useNA="ifany") 
table( Base_geral$Anos_mesma_vizinhanca ,  useNA="ifany") 


str(Base_geral$p183)
attributes(Base_geral$p183)
table( Base_geral$p183 ,  useNA="ifany") 
 
 
Base_geral <- Base_geral %>%
  mutate(
    apartamento = case_when(
      p183 == 1 ~ 1,
      p183 %in% c(2, 3, 4, 5, 6, 7) ~ 0,
      TRUE ~ NA_real_
    ),
    
    casa = case_when(
      p183 == 2 ~ 1,
      p183 %in% c(1, 3, 4, 5, 6, 7) ~ 0,
      TRUE ~ NA_real_
    ),
    
    barraco_outros = case_when(
      p183 %in% c(3, 4, 5, 6, 7) ~ 1,
      p183 %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    )
  )


table( Base_geral$apartamento ,  Base_geral$casa ,  useNA="ifany") 


# Bens 
str(Base_geral$tvcor)
attributes(Base_geral$tvcor)
table( Base_geral$tvcor   ,  useNA="ifany") 


table( Base_geral$carro ,  useNA="ifany") 
table( Base_geral$radio ,  useNA="ifany") 
table( Base_geral$empreg ,  useNA="ifany") 
table( Base_geral$maqlav ,  useNA="ifany") 
table( Base_geral$videodvd ,  useNA="ifany") 
table( Base_geral$banhe ,  useNA="ifany") 
table( Base_geral$gelad ,  useNA="ifany") 
table( Base_geral$freezer ,  useNA="ifany") 
table(Base_geral$n_moradores, useNA = "ifany")

bens <- c("tvcor", "carro", "radio", "maqlav",
          "videodvd", "banhe", "gelad", "freezer") 


# Recodificação correta (0/1 – posse)
Base_geral <- Base_geral %>%
  mutate(across(
    all_of(bens),
    ~ case_when(
      .x %in% 1:6 ~ 1,      # possui ao menos um
      .x == 96 ~ 0,         # não possui
      .x == 97 ~ NA_real_  # missing
    ),
    .names = "{.col}_d"
  ))
 

# Passo 1 — criar um ID explícito
Base_geral <- Base_geral %>%
  mutate(id_obs = row_number())

# Passo 2 — preparar base do PCA mantendo o ID
dados_pca <- Base_geral %>%
  dplyr::select(id_obs, ends_with("_d")) %>%
  tidyr::drop_na()


# Passo 3 — rodar o PCA
pca <- prcomp(
  dados_pca %>% dplyr::select(-id_obs),
  scale. = TRUE
)

# Passo 4 — criar data frame com o índice
indice_df <- data.frame(
  id_obs = dados_pca$id_obs,
  indice_riqueza = -pca$x[,1]
)


# Passo 5 — juntar  à base original

Base_geral <- Base_geral %>%
  left_join(indice_df, by = "id_obs") %>%
  mutate(indice_riqueza_z = scale(indice_riqueza))


Base_teste <- Base_geral %>%
  dplyr::select(indice_riqueza_z,
         tvcor_d, carro_d, radio_d, maqlav_d, videodvd_d ,banhe_d ,gelad_d ,freezer_d ,n_moradores
  )


table( Base_geral$indice_riqueza_z  ,  useNA="ifany") 

 

#### 2.2) Crimes  -----


# Furtos
str(Base_geral$p19)
attributes(Base_geral$p19)
table(Base_geral$p19, useNA = "ifany")

table(Base_geral$p19, useNA = "ifany")
table(Base_geral$p21, useNA = "ifany")
table(Base_geral$p23, useNA = "ifany")


# Furto em qualquer momento
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


# Furto nos ultimos 12 meses
str(Base_geral$p17)
attributes(Base_geral$p17)
table(Base_geral$p17, useNA = "ifany")


str(Base_geral$p18)
attributes(Base_geral$p18)
table(Base_geral$p18, useNA = "ifany")
 


str(Base_geral$p19a)
attributes(Base_geral$p19a)
table(Base_geral$p19a, useNA = "ifany")

str(Base_geral$p21a)
attributes(Base_geral$p21a)
table(Base_geral$p21a, useNA = "ifany")

str(Base_geral$p23a)
attributes(Base_geral$p23a)
table(Base_geral$p23a, useNA = "ifany")
 

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

table(Base_geral$furto, useNA = "ifany")
table(Base_geral$furto_12meses, useNA = "ifany")


# Furto nos ultimos 12 meses em casa, rua ou bairro
str(Base_geral$p34)
attributes(Base_geral$p34)
table(Base_geral$p34, useNA = "ifany")


str(Base_geral$p49)
attributes(Base_geral$p49)
table(Base_geral$p49, useNA = "ifany")


str(Base_geral$p65)
attributes(Base_geral$p65)
table(Base_geral$p65, useNA = "ifany")
 


Base_geral <- Base_geral %>%
  mutate(
    furto_12meses_casa_bairro = case_when(
      
      # REGRA 1: casa / rua da casa / bairro em qualquer pergunta
      (p34 %in% c(1, 2, 3)) |
        (p49 %in% c(1, 2, 3)) |
        (p65 %in% c(1, 14, 17)) ~ 1,
      
      # REGRA 2: todas indicam outros locais
      (p34 %in% c(4,5,6,7,9,10,11,12,13,98) | is.na(p34)) &
        (p49 %in% c(4,5,6,7,9,10,11,12,13,98) | is.na(p49)) &
        (p65 %in% c(2,3,4,5,6,7,8,9,10,11,12,13,15,16,19,20,98) | is.na(p65)) ~ 0,
      
      TRUE ~ NA_real_
    )
  )

table(Base_geral$furto_12meses_casa_bairro, useNA = "ifany")


# Roubo em qualquer momento
table(Base_geral$p20, useNA = "ifany")
table(Base_geral$p22, useNA = "ifany")
table(Base_geral$p24, useNA = "ifany")


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

table(Base_geral$roubo, useNA = "ifany")
table(Base_geral$roubo_12meses, useNA = "ifany")



# Roubo nos ultimos 12 meses em casa ou no bairro
str(Base_geral$p39)
attributes(Base_geral$p39)
table(Base_geral$p39, useNA = "ifany")


str(Base_geral$p54)
attributes(Base_geral$p54)
table(Base_geral$p54, useNA = "ifany")


str(Base_geral$p71)
attributes(Base_geral$p71)
table(Base_geral$p71, useNA = "ifany")



Base_geral <- Base_geral %>%
  mutate(
    roubo_12meses_casa_bairro = case_when(
      
      # REGRA 1: ocorreu em casa / rua da casa / bairro em QUALQUER pergunta
      (p39 %in% c(1, 2, 3)) |
        (p54 %in% c(1, 2, 3)) |
        (p71 %in% c(1, 14, 17)) ~ 1,
      
      # REGRA 2: todas indicam outros locais
      (p39 %in% c(4,5,6,7,9,10,11,12,13,98) | is.na(p39)) &
        (p54 %in% c(4,5,6,7,9,10,11,12,13,98) | is.na(p54)) &
        (p71 %in% c(2,3,4,5,6,7,8,9,10,11,12,13,15,16,19,20,98) | is.na(p71)) ~ 0,
      
      TRUE ~ NA_real_
    )
  )

table(Base_geral$roubo_12meses_casa_bairro, useNA = "ifany")



# Vitimizacao
table(Base_geral$furto_12meses_casa_bairro, useNA = "ifany")
table(Base_geral$roubo_12meses_casa_bairro, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    vitimizacao_12meses_casa_bairro = case_when(
      furto_12meses_casa_bairro == 1 | roubo_12meses_casa_bairro == 1 ~ 1,
      furto_12meses_casa_bairro == 0 & roubo_12meses_casa_bairro == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$vitimizacao_12meses_casa_bairro, useNA = "ifany")

# Acidente

table(Base_geral$p28a, useNA = "ifany")
table(Base_geral$p28b, useNA = "ifany")
table(Base_geral$p28c, useNA = "ifany")
table(Base_geral$p28d, useNA = "ifany")
table(Base_geral$p28e, useNA = "ifany")
 

# Acidente nos ultimos 12 meses
Base_geral <- Base_geral %>%
  mutate(
   acidente_12meses = case_when(
     p28a == 1 | p28b == 1 | p28c == 1 | p28d == 1 | p28e == 1 ~ 1,
     p28a == 2 & p28b == 2 & p28c == 2 & p28d == 2 | p28e == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$acidente_12meses, useNA = "ifany")



# Vizinhos Agressao, assalto e homicidio

# Agressao 
str(Base_geral$p150e)
attributes(Base_geral$p150e)
table(Base_geral$p150e, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    agressao_viz = case_when(
      p150e == 1 ~ 1,
      p150e == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$agressao_viz, useNA = "ifany")


# Assalto 
str(Base_geral$p150f)
attributes(Base_geral$p150f)
table(Base_geral$p150f, useNA = "ifany")


Base_geral <- Base_geral %>%
  mutate(
    assaltados_viz = case_when(
      p150f == 1 ~ 1,
      p150f == 2 ~ 0,  # equivalente explícito ao == 2
      TRUE ~ NA_real_
    )
  )

table(Base_geral$assaltados_viz, useNA = "ifany")

 
# Assassinato 
str(Base_geral$p150g)
attributes(Base_geral$p150g)
table(Base_geral$p150g, useNA = "ifany")


Base_geral <- Base_geral %>%
  mutate(
    assassinados_viz = case_when(
      p150g == 1 ~ 1,
      p150g == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

 table(Base_geral$assassinados_viz, useNA = "ifany")
 
 
 # Violencia contra vizinhos
 
 Base_geral <- Base_geral %>%
   mutate(
     violencia_viz = case_when(
       agressao_viz == 1 | assaltados_viz == 1 | assassinados_viz == 1 ~ 1,
       agressao_viz == 0 & assaltados_viz == 0 & assassinados_viz == 0 ~ 0,
       TRUE ~ NA_real_
     )
   )

 table(Base_geral$violencia_viz, useNA = "ifany")
 

# BROKEN WINDOWS

# Locais Abandonados
str(Base_geral$p147a)
attributes(Base_geral$p147a)
table(Base_geral$p147a, useNA = "ifany")


Base_geral <- Base_geral %>%
  mutate(
    locais_abandonados = case_when(
      p147a == 1 ~ 1,
      p147a == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$locais_abandonados, useNA = "ifany")


# Carros abandonados
str(Base_geral$p147b)
attributes(Base_geral$p147b)
table(Base_geral$p147b, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    carros_abandonados = case_when(
      p147b == 1 ~ 1,
      p147b == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$carros_abandonados, useNA = "ifany")


# Terrenos abandonados
str(Base_geral$p147c)
attributes(Base_geral$p147c)
table(Base_geral$p147c, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    terrenos_abandonados = case_when(
      p147c == 1 ~ 1,
      p147c == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$terrenos_abandonados, useNA = "ifany")

# Tiros
str(Base_geral$p147d)
attributes(Base_geral$p147d)
table(Base_geral$p147d, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    tiros = case_when(
      p147d == 1 ~ 1,
      p147d == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$tiros, useNA = "ifany")


# Cheiro Ruim
str(Base_geral$p147e)
attributes(Base_geral$p147e)
table(Base_geral$p147e, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    cheiro_ruim = case_when(
      p147e == 1 ~ 1,
      p147e == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$cheiro_ruim, useNA = "ifany")



# Ruido
str(Base_geral$p147f)
attributes(Base_geral$p147f)
table(Base_geral$p147f, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    ruidos = case_when(
      p147f == 1 ~ 1,
      p147f == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$ruidos, useNA = "ifany")


# ITENS DE SEGURANCA

# Grades
str(Base_geral$p151a)
attributes(Base_geral$p151a)
table(Base_geral$p151a, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    grades = case_when(
      p151a == 1 ~ 1,
      p151a == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$grades, useNA = "ifany")


# Alarmes
str(Base_geral$p151f)
attributes(Base_geral$p151f)
table(Base_geral$p151f, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    alarme = case_when(
      p151f == 1 ~ 1,
      p151f == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$alarme, useNA = "ifany")


# Camera
str(Base_geral$p151g)
attributes(Base_geral$p151g)
table(Base_geral$p151g, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    camera = case_when(
      p151g == 1 ~ 1,
      p151g == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$camera, useNA = "ifany")



# Cerca
str(Base_geral$p151l)
attributes(Base_geral$p151l)
table(Base_geral$p151l, useNA = "ifany")

str(Base_geral$p151m)
attributes(Base_geral$p151m)
table(Base_geral$p151m, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    cerca = case_when(
      p151l == 1 | p151m == 1 ~ 1,
      p151l == 2 & p151m == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(Base_geral$cerca, useNA = "ifany")



# SEGURANÇA

# Casa (sozinho)
str(Base_geral$p159)
attributes(Base_geral$p159)
table(Base_geral$p159, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    p159_num = as.numeric(p159)
  )

Base_geral <- Base_geral %>%
  mutate(
    muito_seguro_casa = case_when(
      p159_num == 1 ~ 1,
      p159_num %in% c(2, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),
    
    um_pouco_seguro_casa = case_when(
      p159_num == 2 ~ 1,
      p159_num %in% c(1, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),
    
    um_pouco_inseguro_casa = case_when(
      p159_num == 4 ~ 1,
      p159_num %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    
    muito_inseguro_casa = case_when(
      p159_num == 3 ~ 1,
      p159_num %in% c(1, 2, 4) ~ 0,
      TRUE ~ NA_real_
    )
  )


Base_geral <- Base_geral %>%
  mutate(
    sensacao_seg_casa = case_when(
      muito_seguro_casa == 1 ~ 1,
      um_pouco_seguro_casa == 1 ~ 2,
      um_pouco_inseguro_casa == 1 ~ 3,
      muito_inseguro_casa == 1 ~ 4,
      TRUE ~ NA_real_
    )
  )

Base_geral <- Base_geral %>%
  mutate(
    inseguro_casa = case_when(
      um_pouco_inseguro_casa == 1 | muito_inseguro_casa == 1 ~ 1,
      um_pouco_seguro_casa == 1 | muito_seguro_casa == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )


table(Base_geral$muito_seguro_casa, useNA = "ifany")
table(Base_geral$um_pouco_seguro_casa, useNA = "ifany")
table(Base_geral$um_pouco_inseguro_casa, useNA = "ifany")
table(Base_geral$muito_inseguro_casa, useNA = "ifany")



# Bairro
str(Base_geral$p152)
attributes(Base_geral$p152)
table(Base_geral$p152, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    p152_num = as.numeric(p152)
  )

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
    )
  )

Base_geral <- Base_geral %>%
  mutate(
    inseguro_bairro_dia = case_when(
      um_pouco_inseguro_bairro_dia == 1 | muito_inseguro_bairro_dia == 1 ~ 1,
      um_pouco_seguro_bairro_dia == 1 | muito_seguro_bairro_dia == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )


Base_geral <- Base_geral %>%
  mutate(
    sensacao_seg_bairro = case_when(
      muito_seguro_bairro_dia == 1 ~ 1,
      um_pouco_seguro_bairro_dia == 1 ~ 2,
      um_pouco_inseguro_bairro_dia == 1 ~ 3,
      muito_inseguro_bairro_dia == 1 ~ 4,
      TRUE ~ NA_real_
    )
  )


table(Base_geral$p153, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    p153_num = as.numeric(p153)
  )



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
    )
  )


Base_geral <- Base_geral %>%
  mutate(
    inseguro_bairro_noite = case_when(
      um_pouco_inseguro_bairro_noite == 1 |
        muito_inseguro_bairro_noite == 1 ~ 1,
      
      um_pouco_seguro_bairro_noite == 1 |
        muito_seguro_bairro_noite == 1 ~ 0,
      
      TRUE ~ NA_real_
    )
  )


Base_geral <- Base_geral %>%
  mutate(
    sensacao_seg_bairro_noite = case_when(
      muito_seguro_bairro_noite == 1 ~ 1,
      um_pouco_seguro_bairro_noite == 1 ~ 2,
      um_pouco_inseguro_bairro_noite == 1 ~ 3,
      muito_inseguro_bairro_noite == 1 ~ 4,
      TRUE ~ NA_real_
    )
  )



# Teme ser assaltado ou roubado na vizinhanca
str(Base_geral$p163a)
attributes(Base_geral$p163a)
table(Base_geral$p163a, useNA = "ifany")


str(Base_geral$p163b)
attributes(Base_geral$p163b)
table(Base_geral$p163b, useNA = "ifany")
 


Base_geral <- Base_geral %>%
  mutate(
    medo_assalto_roubo_viz = case_when(
      
      # REGRA 1: tem medo em pelo menos uma dimensão
      p163a == 1 | p163b == 1 ~ 1,
      
      # REGRA 2: não tem medo em nenhuma dimensão aplicável
      (p163a == 2 | p163a == 96) &
        (p163b == 2 | p163b == 96 | is.na(p163b)) ~ 0,
      
      TRUE ~ NA_real_
    )
  )

table(Base_geral$medo_assalto_roubo_viz, useNA = "ifany")


# Teme ser casa assaltada
str(Base_geral$p166a)
attributes(Base_geral$p166a)
table(Base_geral$p166a, useNA = "ifany")


Base_geral <- Base_geral %>%
  mutate(
    medo_assalto_roubo_casa = case_when(
      
      # REGRA 1: tem medo em pelo menos uma dimensão
      p166a == 1  ~ 1,
      
      # REGRA 2: não tem medo em nenhuma dimensão aplicável
      p166a == 2  ~ 0,
      
      TRUE ~ NA_real_
    )
  )

table(Base_geral$medo_assalto_roubo_casa, useNA = "ifany")


# ILUMINACAO 
str(Base_geral$p148b)
attributes(Base_geral$p148b)
table(Base_geral$p148b, useNA = "ifany")

Base_geral <- Base_geral %>%
  mutate(
    p148b_num = as.numeric(p148b)
  )


Base_geral <- Base_geral %>%
  mutate(
    iluminacao_otima = case_when(
      p148b_num == 1 ~ 1,
      p148b_num %in% c(2, 3, 4, 5, 6) ~ 0,
      TRUE ~ NA_real_
    )
  )

Base_geral <- Base_geral %>%
  mutate(
    iluminacao_boa = case_when(
      p148b_num == 2 ~ 1,
      p148b_num %in% c(1, 3, 4, 5, 6) ~ 0,
      TRUE ~ NA_real_
    )
  )

Base_geral <- Base_geral %>%
  mutate(
    iluminacao_regular = case_when(
      p148b_num == 3 ~ 1,
      p148b_num %in% c(1, 2, 4, 5, 6) ~ 0,
      TRUE ~ NA_real_
    )
  )


Base_geral <- Base_geral %>%
  mutate(
    iluminacao_ruim = case_when(
      p148b_num %in% c(4) ~ 1,
      p148b_num %in% c(1, 2, 3, 5, 6) ~ 0,
      TRUE ~ NA_real_
    )
  )

Base_geral <- Base_geral %>%
  mutate(
    iluminacao_pess = case_when(
      p148b_num %in% c(5, 6) ~ 1,
      p148b_num %in% c(1, 2, 3, 4) ~ 0,
      TRUE ~ NA_real_
    )
  )


table(Base_geral$iluminacao_otima, useNA = "ifany")
table(Base_geral$iluminacao_boa, useNA = "ifany")
table(Base_geral$iluminacao_regular, useNA = "ifany")
table(Base_geral$iluminacao_ruim, useNA = "ifany")
table(Base_geral$iluminacao_pess, useNA = "ifany")


Base_geral <- Base_geral %>%
  mutate(
    iluminacao_ord = case_when(
      p148b_num == 1 ~ 1,
      p148b_num == 2 ~ 2,
      p148b_num == 3 ~ 3,
      p148b_num == 4 ~ 4,
      p148b_num %in% c(5, 6) ~ 5,
      TRUE ~ NA_real_
    )
  )



# BASE FINAL

names(Base_geral)

Base_final_iluminacao <- Base_geral %>%
  dplyr::select(
    # Identificação e peso
    id_obs, peso_usado,
    
    # Geografia
    sudeste, sul, nordeste, centro_oeste, norte,
    capital, viz_capital, interior,
    porte_muito_grande, porte_grande, porte_medio, porte_pequeno,
    
    # Demografia e domicílio
    n_adultos, n_criancas, n_moradores,
    idade_16_24, idade_25_34, idade_35_44, idade_45_59, idade_60_mais,
    Anos_mesma_cidade, Anos_mesma_vizinhanca,
    Mesma_cidade,
    mulher,
    casado_uniao,
    branco,
    
    # Escolaridade
    escol_sem_instr, escol_ate_fund_completo,
    escol_ate_med_completo, escol_ate_pos_grad,
    escolc_sem_instr, escolc_ate_fund_completo,
    escolc_ate_med_completo, escolc_ate_pos_grad,
    
    # Populacao economicamente ativa
    pea_dummy,
    
    # Renda
    sal_min_ate_1,
    sal_min_de_1_ate_2_sm,
    sal_min_de_2_ate_3_sm,
    sal_min_de_3_ate_5_sm,
    sal_min_acima_5,
    ln_renda_pc,
    
    # Rotina e mobilidade
    casa_manha, casa_tarde, casa_noite, casa_madrugada,
    saidas_total,sem_saida_noturna, evita_noite_vio, sem_bar_noturno,
    trans_carro, trans_moto, trans_onibus,
    trans_bike, trans_ape, trans_outros,
    
    # Moradia
    apartamento, casa, barraco_outros,
    
    # Riqueza
    indice_riqueza_z,
    
    # Crimes
    furto, furto_12meses, furto_12meses_casa_bairro, 
    roubo, roubo_12meses, roubo_12meses_casa_bairro, 
    acidente_12meses, vitimizacao_12meses_casa_bairro,
    
    # Violência no entorno
    agressao_viz,
    assaltados_viz,
    assassinados_viz,
    violencia_viz,
    
    # Broken windows
    locais_abandonados,
    carros_abandonados,
    terrenos_abandonados,
    tiros,
    cheiro_ruim,
    ruidos,
    
    # Segurança domiciliar
    grades, alarme, camera, cerca,
    
    # Sensação de segurança
    inseguro_casa,
    sensacao_seg_casa,
    inseguro_bairro_dia,
    sensacao_seg_bairro,
    inseguro_bairro_noite,
    sensacao_seg_bairro_noite,
    medo_assalto_roubo_viz, 
    medo_assalto_roubo_casa,
    
    # Iluminação pública
    iluminacao_otima,
    iluminacao_boa,
    iluminacao_regular,
    iluminacao_ruim,
    iluminacao_pess,
    iluminacao_ord
  )


saveRDS(
  Base_final_iluminacao,
  "data/processed/Base_final_iluminacao.rds"
)
