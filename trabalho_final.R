
library(tidyverse)
library(xlsx)

df <- read_csv(
  "Questionário sobre o RU  (respostas) - Respostas ao formulário 1.csv") %>%
  janitor::clean_names()

glimpse(df)

df %>% 
  pull(qual_seu_vinculo_com_a_ufmg) %>% 
  unique()

df <- df %>% 
  rename(vinculo = qual_seu_vinculo_com_a_ufmg,
         sexo = qual_o_seu_genero,
         fump = qual_a_sua_classificacao_na_fump_4,
         raca_cor = qual_sua_cor_raca,
         renda = qual_sua_renda_mensal_em_salarios_minimos,
         dias_qtd = quantos_dias_na_semana_voce_come_no_restaurante_universitario,
         refeicoes = quais_refeicoes_voce_faz_no_ru,
         refeicao_sem_ru = sem_o_servico_do_ru_eu_nao_conseguiria_realizar_uma_refeicao_completa_por_questoes_financeiras,
         importancia_permanencia = quao_importante_e_o_ru_para_sua_permanencia_na_universidade) %>% 
  filter(vinculo=="ESTUDANTE DE GRADUAÇÃO")


vars <- c("vinculo", "sexo", "fump", "raca_cor",
          "renda", "dias_qtd", "refeicoes",
          "refeicao_sem_ru", "importancia_permanencia")
df <- df %>% 
  select(carimbo_de_data_hora, all_of(vars))


df %>% pull(fump) %>% unique()

df <- df %>% mutate(
  renda_sm = case_when(
    renda=="Menos de 1 salário minimo (até R$1212,00)" ~ "Até 1 S.M.",
    renda=="de 1 a 3 salários mínimos (De R$1212,00 até R$3636,00)" ~ "1-3 S.M",
    renda=="de 3 a 5 salários mínimos (de R$3636,00 até R$6060,00)" ~ "3-5 S.M.",
    renda=="mais de 5 salários mínimos (mais de R$6060,00)" ~ "5+ S.M."),
  
  nivel_fump = ifelse(fump=="Não sou assistido pela Fump", "Desnivelado", fump)
)

df %>% 
  ggplot(aes(nivel_fump, fill=renda_sm))+
  geom_bar()+
  labs(x="Nível da FUMP",
       fill="Renda mensal (S.M.)")
  
  
barfill <- function(data, x, fill){
    
    data %>% 
      ggplot(aes(fct_infreq({{x}}), fill={{fill}}))+
      geom_bar()+
      # geom_text(stat='count',
      #           aes(label =  after_stat(count)))+
      theme_minimal()+
      theme(legend.position = "bottom",
            axis.text.x = element_text( angle = 45, vjust = 0.75))
}

df <- df %>% 
  mutate(nivel_fump = fct_explicit_na(nivel_fump,"Sem informação"))

df %>% 
  barfill(nivel_fump, renda_sm)+
  labs(x="Nível da FUMP",
       fill="Renda mensal (S.M.)",
       y=NULL)+
  scale_fill_brewer(palette = "Paired", type = "qual")
  

df %>% 
  group_by(nivel_fump) %>% 
  summarise(media_dias = mean(dias_qtd, na.rm=T)) %>%
  arrange(desc(media_dias)) %>% 
  googlesheets4::write_sheet(ss="1kq13hOwSlmUjcN02njHHQc_3-T6wIxJieCnqBQX9wJk",
                             sheet = "Média de dias por nível")

df %>% 
  barfill(nivel_fump, importancia_permanencia)

df %>% 
  pull(importancia_permanencia) %>% 
  unique()


ggplot(df, aes(nivel_fump, importancia_permanencia, fill=nivel_fump))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text( angle = 45, vjust = 0.75))+
  labs(x="Nível da FUMP",
       y = "Importância do RU p/ permanência")+
  scale_fill_brewer(palette = "Paired", type = "qual")


df %>% 
  group_by(nivel_fump) %>% 
  summarise(media_importancia = mean(importancia_permanencia, na.rm=T)) %>% 
  arrange(desc(media_importancia)) %>% 
  googlesheets4::sheet_write(ss="1kq13hOwSlmUjcN02njHHQc_3-T6wIxJieCnqBQX9wJk",
                             sheet = "Média da importância por nível")





ggplot(df, aes(nivel_fump, refeicao_sem_ru, fill=nivel_fump))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text( angle = 45, vjust = 0.75))+
  labs(x="Nível da FUMP",
       y = "Conseguiria realizar refeição sem RU?")+
  scale_fill_brewer(palette = "Paired", type = "qual")


df %>% 
  group_by(nivel_fump) %>% 
  summarise(media_refeicao_sem_ru = mean(refeicao_sem_ru, na.rm=T)) %>% 
  arrange(desc(media_refeicao_sem_ru)) %>% 
  googlesheets4::sheet_write(ss="1kq13hOwSlmUjcN02njHHQc_3-T6wIxJieCnqBQX9wJk",
                             sheet = "Média conseguir realizar refeição por nível")


df %>% 
  pull(refeicoes) %>% 
  unique()


df <- df %>% 
  mutate(qtd_refeicoes = str_count(refeicoes, regex("\\,")) + 1)

df %>% 
  select(qtd_refeicoes, refeicoes) %>% 
  distinct()

ggplot(df, aes(nivel_fump, qtd_refeicoes, fill=nivel_fump))+
  geom_boxplot()+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.x = element_text( angle = 45, vjust = 0.75))+
  labs(x="Nível da FUMP",
       y = "Quantidade de refeições")+
  scale_fill_brewer(palette = "Paired", type = "qual")+
  scale_y_continuous(labels = scales::number_format(decimal.mark = ","))






df %>% 
  group_by(nivel_fump) %>% 
  summarise(media_n_refeicoes = mean(qtd_refeicoes, na.rm=T)) %>% 
  arrange(desc(media_n_refeicoes)) %>% 
  googlesheets4::sheet_write(ss="1kq13hOwSlmUjcN02njHHQc_3-T6wIxJieCnqBQX9wJk",
                             sheet = "Média de quantidade de refeições por nível")

df %>% 
  mutate(raca_cor2 = case_when(raca_cor %in% c("Amarelo", "Indígena") ~ "Outro",
                               raca_cor=="Branco" ~ "Branco",
                               TRUE ~ "Negro")) %>% 
  barfill(nivel_fump, raca_cor2)+
  labs(x="Nível da FUMP",
       fill="Cor ou raça",
       y=NULL)+
  scale_fill_brewer(palette = "Paired", type = "qual")











df %>%
  mutate(sexo2 = case_when(
    str_detect(sexo, regex("(Homem)")) ~ "Homem",
    str_detect(sexo, regex("(Mulher)")) ~ "Mulher",
    TRUE ~ "Outro/Sem informação")) %>% 
  barfill(nivel_fump, sexo2)+
  labs(x="Nível da FUMP",
       fill="Gênero",
       y=NULL)+
  scale_fill_brewer(palette = "Paired", type = "qual")


barfill(df, nivel_fump, sexo)+
  labs(x="Nível da FUMP",
       fill="Gênero",
       y=NULL)+
  scale_fill_brewer(palette = "Paired", type = "qual")



