library(tidyverse)
library(tidytext)
library(here)
library(lexiconPT)
theme_set(theme_bw())

reclamacoes_raw = read_csv(here("data/3-avaliacao-humana/reclamacoes-avaliadas-20180703.csv"))

reclamacoes = reclamacoes_raw %>%
    mutate(
        nome_orgao_site = orgao,
        orgao = str_split(link, "/") %>% map_chr(~ .[[5]])
    ) %>%
    filter(orgao %in% c("inss-ministerio-da-previdencia-social", "anac-agencia-nacional-de-aviacao-civil")) %>%
    mutate(id = 1:n(), grupo_avaliando = id %% 6 + 1)

data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

op30 <- oplexicon_v3.0
sent <- sentiLex_lem_PT02

glimpse(op30)

palavra_a_palavra = reclamacoes %>% 
  select(id, reclamacao) %>% 
  unnest_tokens(termo, reclamacao)

palavra_a_palavra %>%
  select(id, termo) %>%
  head(20)

palavras_com_sentimento = palavra_a_palavra %>% 
  left_join(op30 %>% select(term, op30 = polarity), by = c("termo" = "term")) %>% 
  left_join(sent %>% select(term, sent = polarity), by = c("termo" = "term")) 

sentimentos = palavras_com_sentimento %>% 
  group_by(id) %>%
  summarise(sentimento_op30 = sum(op30, na.rm = TRUE),
            palavras_op30 = sum(!is.na(op30)),
            sentimento_sent = sum(sent, na.rm = TRUE), 
            palavras_sent = sum(!is.na(sent)), 
            palavras = n())

sentimentos %>% 
  write_csv(here("data/5-sentimentos/sentimento.csv"))