library(tidyverse)
library(here)
library(modelr)
library(broom)

theme_set(theme_bw())

reclamacoes = read_csv(here("data/3-avaliacao-humana/reclamacoes-avaliadas-20180703.csv"))
sentimentos = read_csv(here("data/5-sentimentos/sentimento.csv"))

reclamacoes = reclamacoes %>% mutate(comprimento_reclamacao = str_length(reclamacao))

reclamacoes = reclamacoes %>% 
  left_join(sentimentos, by = "id")

reclamacoes_l = reclamacoes %>%  
  select(-palavras_op30, -palavras_sent, -grupo_avaliando) %>% 
  gather(key = "lexico", 
         value = "polaridade", 
         sentimento_op30, sentimento_sent)

 reclamacoes_l = reclamacoes_l %>% 
     group_by(lexico) %>% 
     mutate(polaridade_invertida = -polaridade) %>%
     mutate(polaridade_normalizada = round((polaridade_invertida - min(polaridade_invertida)) / (abs(max(polaridade_invertida) - min(polaridade_invertida)) / 4) + 1))
 
 reclamacoes_l %>% View()
