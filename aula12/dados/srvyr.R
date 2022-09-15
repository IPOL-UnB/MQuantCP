## Expandindo amostra com srvyr



# 1. Caregue os pacotes "tidyverse", "survey" e "srvyr"
lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis","survey","srvyr") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

# 2. Leia o conjunto de dados 'api' do pacote survey usando o comando data(api)
data(api)

# 3. Elimine os objetos 'apiclus1', 'apiclus2', 'apipop' e 'apistrat'
# mantendo apenas o objeto apisrs
rm(apiclus1,apiclus2,apipop,apistrat)

# 4. crie o objeto 'pesos' selecionando as colunas 'stypr' e 'pw' do objeto 'apisrs'
pesos <- apisrs %>%
  dplyr::select(stype,pw)

# 5. crie o objeto 'escolas_notas' selecionando as colunas 
# 'cds', 'stype', 'api99' e 'api00' do objeto apisrs
escolas_notas <- apisrs %>%
  dplyr::select(cds,stype,api99,api00,awards,both)

# 6. Remova as duplicatas (linhas em duplicidade) do arquivo `pesos` que você criou no passo 4
pesos <- pesos %>%
  distinct()

# 7. Quantas linhas tem o novo objeto `pesos`, sem as duplicidades
nrow(pesos)

# 8. Traga a variável `pw` para `escola_notas`, criando um novo objeto `amostra` 
# resultado da união (join) do objeto `pesos` ao objeto `escolas_notas` 
# dica use left_join, com `escola_notas` na esquerda.
amostra <- escolas_notas %>%
  left_join(pesos)

# 9. Crie o objeto tbl_svy `amostra_expandida` expandindo a amostra aleatória simples (`amostra`)
# usando apenas a variável (coluna) "pw", contendo o peso amostral. 
amostra_expandida <- amostra %>%
  as_survey(weight = pw)

# 10. Faça um gráfico de barras comparando a variação média 
# das notas de 1999 (`api99`) e 2000 (`api00`) por tipo de escola (`stype`) 
# utilize as estimativas intervalares para construir barras com o intervalo de confiança
amostra_expandida %>%
  group_by(stype) %>%
  summarise(api_diff = survey_mean(api00 - api99, vartype = "ci")) %>%
  ggplot(aes(x = stype, y = api_diff, fill = stype,color=stype,
             ymax = api_diff_upp, ymin = api_diff_low)) +
  geom_bar(stat = "identity",alpha=0.6,color=NA) +
  geom_errorbar(width = 0,size=3) 


amostra_expandida %>%
  group_by(awards) %>%
  summarise(api_diff = survey_mean(api00 - api99, vartype = "ci")) %>%
  ggplot(aes(x = awards, y = api_diff, fill = awards,color=awards,
             ymax = api_diff_upp, ymin = api_diff_low)) +
  geom_bar(stat = "identity",alpha=0.6,color=NA) +
  geom_errorbar(width = 0,size=3) 

amostra_expandida %>%
  group_by(stype,awards) %>%
  summarise(pct = survey_mean(vartype = "ci")) %>%
  ggplot(aes(x = awards, y = pct, fill = stype,color=stype,
             ymax = pct_upp, ymin = pct_low)) +
  geom_bar(stat = "identity",alpha=0.6,color=NA) +
  geom_errorbar(width = 0,size=3) +
  facet_wrap(vars(stype))


amostra_expandida %>%
  group_by(stype,awards) %>%
  summarise(api_diff = survey_mean(api00 - api99, vartype = "ci")) %>%
  ggplot(aes(x = awards, y = api_diff, fill = stype,color=stype,
             ymax = api_diff_upp, ymin = api_diff_low)) +
  geom_bar(stat = "identity",alpha=0.6,color=NA) +
  geom_errorbar(width = 0,size=3) +
  facet_wrap(vars(stype))


