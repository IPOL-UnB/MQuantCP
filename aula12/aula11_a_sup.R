library(tidyverse)
library(dabestr)
library(infer)
library(rstatix)
library(car)

# Mudando dfe ----

dfe <- read_rds("dados/dfe.rds") %>% 
  mutate(aleat=floor(runif(nrow(.),min=0, max=6)),
         nota1=media-aleat,
         nota2=media+aleat,
         perfil_faltas = ifelse(faltas>mean(faltas,na.rm=T),
                                "Faltoso","Assíduo"),
         perfil_estudos = ifelse(media>median(media,na.rm=T),
                                "Conjunto","Individual")) %>% 
    dplyr::select(-aleat)

glimpse(dfe)


# Teste de médias usando infer ----

dfe %>% 
  drop_na(interess) %>%
  group_by(interess) %>% 
  shapiro_test(media)

# 
dfe %>%
  drop_na(interess) %>%
  ggplot() +
  geom_density(aes(x = media,fill=interess),color=NA,alpha=.5)

## Teste de médias usando infer

dfe %>% leveneTest(media ~ interess,.)

dfe %>%
  infer::t_test(media ~ interess)

dfe %>%
  infer::t_test(media ~ interess,var.equal = T)

dfe %>%
  rstatix::t_test(media ~ interess,var.equal = F)


## graficando
ggplot(data = dfe %>%
         drop_na(interess),
       mapping = aes(x = interess, y = media)) +
  geom_dotplot(binaxis = "y", stackdir = "center",
               fill="#D8D8D8", color="#D8D8D8", dotsize = 0.8) +
  geom_point(stat = "summary", fun = "mean", size = 2) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.1) +
  labs(y="Média",x="Interesse na disciplina") +
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
  theme_pubr()

library(ggpubr)
## Teste de médias usando infer

dfe %>% leveneTest(media ~ perfil_faltas,.)

dfe %>%
  infer::t_test(media ~ perfil_faltas)

dfe %>%
  infer::t_test(media ~ perfil_faltas,var.equal = T)

dfe %>%
  rstatix::t_test(media ~ perfil_faltas,var.equal = F)

## graficando
ggplot(data = dfe,
       mapping = aes(x = perfil_faltas, y = Nota_Biol)) +
  geom_dotplot(binaxis = "y", stackdir = "center",
               fill="#D8D8D8", color="#D8D8D8", dotsize = 0.8) +
  geom_point(stat = "summary", fun = "mean", size = 2) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.1) +
  ylab("Notas de biologia") +
  xlab("Posição na sala") +
  scale_y_continuous(limits=c(0,10), expand = c(0,0)) +
  theme_classic(base_size=12)


## Teste de médias usando infer

dfe %>% 
  group_by(perfil_estudos) %>% 
  shapiro_test(media)

dfe %>% leveneTest(media ~ perfil_estudos,.)

dfe %>%
  t_test(media ~ perfil_estudos)

dfe %>%
  t_test(media ~ perfil_estudos,var.equal = T)

dfe %>%
  t_test(media ~ perfil_estudos,var.equal = F)




# testes não paramétricos

######################### Teste de Mann-Whitney #########################

# Passo 3: Realização do teste de Mann-Whitney

wilcox.test(media ~ estcivil, data = dfe %>% drop_na(estcivil))

# Observação:
# O teste bicaudal é o default; caso deseje unicaudal, necessário incluir:
# alternative = "greater" ou alternative = "less"
# Exemplo: wilcox.test(Nota_Hist ~ Posicao_Sala, data = dfe, alternative="greater")
# Nesse caso, o teste verificará se é a mediana do primeiro grupo é maior que a mediana do segundo
# O R está considerando "Frente" como primeiro grupo


# Passo 4: Análise descritiva dos dfe

dfe %>% group_by(estcivil) %>% 
  get_summary_stats(media, Nota_Hist, Nota_Fis, type = "median_iqr")

# dfe paramétricos?
# dfe %>% group_by(Posicao_Sala) %>% 
#  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "mean_sd")


# Passo 5: Visualização da distribuição
par(mfrow=c(1,2))
hist(dfe$media[dfe$estcivil == "Casado"],
     ylab="Frequência", xlab="Nota", main="Casados")
hist(dfe$media[dfe$estcivil == "Solteiro"],
     ylab="Frequência", xlab="Nota", main="Solteiros")


######################### Teste de Wilcoxon #########################


# Passo 3: Realização do teste de Wilcoxon

wilcox.test(dfe$nota1, dfe$nota2, paired = TRUE)



# Passo 4: Análise descritiva dos dfe

dfe %>% get_summary_stats(Convulsoes_PT, Convulsoes_S1, dif, type = "median_iqr")

# dfe paramétricos?
# dfe %>% group_by(Posicao_Sala) %>% 
#  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "mean_sd")


######################### Teste de Kruskal-Wallis #########################


# Passo 3: Realização do teste de Kruskal-Wallis

kruskal.test(media ~ turma, data = dfe)


# Passo 4: Testes de post-hoc

# Teste de Dunn com ajuste do valor de p
dunn_test(media ~ turma, data = dfe, p.adjust.method = "bonferroni")


# Passo 5: Análise descritiva dos dfe
dfe %>% group_by(turma) %>% 
  get_summary_stats(media, type = "median_iqr")


# Passo 6: Visualização dos dfe
par(mfrow=c(1,2))
boxplot(media ~ turma, data = dfe)
boxplot(Pressao ~ turma, data = dfe)



# Histograma com todos os grupos, separados por cor
ggplot(dfe, aes(x = media)) +
  geom_histogram(aes(color = turma, fill = turma),
                 alpha = 0.3, position = "identity", binwidth = 10)




# Dabestr ----

## Dois grupos não pareados ----

two.group.unpaired <- 
  dfe %>%
  dabest(perfil_faltas, media, 
         # The idx below passes "Control" as the control group, 
         # and "Group1" as the test group. The mean difference
         # will be computed as mean(Group1) - mean(Control1).
         idx = c("Faltoso", "Assíduo"), 
         paired = FALSE)

# Calling the object automatically prints out a summary.
two.group.unpaired 


two.group.unpaired.meandiff <- mean_diff(two.group.unpaired)

# Calling the above object produces a textual summary of the computed effect size.
two.group.unpaired.meandiff

two.group.unpaired %>% cohens_d() %>% plot()
two.group.unpaired %>% hedges_g() %>% plot()
two.group.unpaired %>% median_diff() %>% plot()

## dois grupos não pareados


two.group.unpaired2 <- 
  dfe %>%
  dabest(interess, media, 
         # The idx below passes "Control" as the control group, 
         # and "Group1" as the test group. The mean difference
         # will be computed as mean(Group1) - mean(Control1).
         idx = c("Principal", "Secundário"), 
         paired = FALSE)

# Calling the object automatically prints out a summary.
two.group.unpaired2 


two.group.unpaired.meandiff2 <- mean_diff(two.group.unpaired2)

# Calling the above object produces a textual summary of the computed effect size.
two.group.unpaired.meandiff2

two.group.unpaired2 %>% cohens_d() %>% plot()

## Dois grupos não pareados

two.group.unpaired3 <- 
  dfe %>%
  dabest(perfil_estudos, media, 
         # The idx below passes "Control" as the control group, 
         # and "Group1" as the test group. The mean difference
         # will be computed as mean(Group1) - mean(Control1).
         idx = c("Conjunto", "Individual"), 
         paired = FALSE)

# Calling the object automatically prints out a summary.
two.group.unpaired3 


two.group.unpaired.meandiff3 <- mean_diff(two.group.unpaired3)


# Calling the above object produces a textual summary of the computed effect size.
two.group.unpaired.meandiff3 %>% plot()

two.group.unpaired3 %>% cohens_d() # %>% plot()

## Dois grupos pareados ----
two.group.paired <- 
  dfe %>%
  dplyr::select(id,nota1,nota2) %>%
  pivot_longer(!id,names_to = "grupo",values_to = "medida") %>%
  dabest(grupo, medida, 
         idx = c("nota1", "nota2"), 
         paired = TRUE, id.col = id)


# The summary indicates this is a paired comparison. 
two.group.paired


# Create a paired plot.
two.group.paired %>% 
  mean_diff()

# Create a paired plot.
two.group.paired %>% 
  mean_diff() %>% 
  plot()


two.group.paired %>% 
  cohens_d() %>%
  plot()



# S, H, G, C =
# Specify, Hypothesize, Generate, Calculate
# (how to remember: reverse alphabetical)
set.seed(5)
perm <- dfe %>% specify(media ~ faltas) %>%
  hypothesize(null="independence") %>%
  generate(reps=1000, type="permute") %>%
  calculate(stat="slope")

visualize(perm)

obs_beta <- dfe %>% specify(media ~ faltas) %>%
  calculate(stat="slope")

visualize(perm, bins=30) +
  shade_p_value(obs