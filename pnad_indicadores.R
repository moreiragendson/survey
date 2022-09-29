

# libraries ---------------------------------------------------------------


library(PNADcIBGE)
library(srvyr)


# read --------------------------------------------------------------------

vars <- c("V2007",
          "V2009",
          "UF",
          "Estrato",
          "V1028",
          "Capital",
          "Ano",
          "Trimestre",
          "UPA")

pnad <- get_pnadc(year = 2022, quarter = 2, vars = vars, design = F)


# data wrangling ----------------------------------------------------------


pnad <- pnad %>% 
  rename(sexo = V2007,
         idade = V2009)

names(pnad)

capitais <- pnad %>% 
  filter(!is.na(Capital)) %>% 
  mutate(Capital = str_remove_all(Capital, pattern="de"),
         Capital = str_sub(Capital, start = 10, end = -1),
         Capital = str_trim(Capital))



capitais <- capitais %>% 
  as_survey_design(
    ids = UPA,
    strata = Estrato,
    weights = V1028,
    nest = TRUE
  )


# razão de sexo -----------------------------------------------------------

razao_sexo <- capitais %>% 
  group_by(Capital, sexo) %>%
  summarise(n = survey_total()) %>% 
  select(-n_se) %>% 
  tidyr::pivot_wider(names_from = sexo,
                     values_from = n) %>% 
  dplyr::mutate(razao_sexo = Homem/Mulher * 100)


# razão de dependência jovem ----------------------------------------------



razao_dependencia_jovem <- capitais %>% 
  mutate(age = dplyr::case_when(idade >= 60 ~ "Idoso",
                                idade <= 15 ~ "Jovem",
                                is.na(idade) ~ as.character(idade),
                                TRUE ~ "PEA")) %>% # PEA
  group_by(Capital, age) %>% 
  summarise(n = survey_total()) %>% 
  select(-n_se) %>% 
  tidyr::pivot_wider(names_from = age,
                     values_from = n) %>% 
  mutate(razao_dependencia_jovem = Jovem/PEA * 100)



# razão de dependência de idosos ------------------------------------------


razao_dependencia_idosos <- capitais %>% 
  mutate(age = dplyr::case_when(idade >= 60 ~ "Idoso",
                                idade <= 15 ~ "Jovem",
                                is.na(idade) ~ as.character(idade),
                                TRUE ~ "PEA")) %>% # PEA
  group_by(Capital, age) %>% 
  summarise(n = survey_total()) %>% 
  select(-n_se) %>% 
  tidyr::pivot_wider(names_from = age,
                     values_from = n) %>% 
  mutate(razao_dependencia_idosos = Idoso/PEA * 100)


# índice de envelhecimento ------------------------------------------------


indice_envelhecimento <- capitais %>% 
  mutate(age = dplyr::case_when(idade >= 60 ~ "Idoso",
                                idade <= 15 ~ "Jovem",
                                is.na(idade) ~ as.character(idade),
                                TRUE ~ "Outro")) %>% # PEA
  group_by(Capital, age) %>% 
  summarise(n = survey_total()) %>% 
  select(-n_se) %>% 
  tidyr::pivot_wider(names_from = age,
                    values_from = n) %>% 
  mutate(indice_envelhecimento = Idoso/Jovem * 100)
