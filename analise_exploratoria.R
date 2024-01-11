library(tidyverse)

ploa2024<- readr::read_csv2("ploa2024_rp.csv", locale = readr::locale(encoding = "LATIN1"))


dado_mdic<-
  ploa2024 %>%
  filter(ministerio=="mdic") %>%
  mutate(variacao_perc = ifelse(is.infinite(variacao_perc),NA,variacao_perc))

dado_mdic%>%
  write_csv("dado_mdic.csv")
  

#ranking por valor total
dados_grafico<-
  ploa2024 %>%
  filter(indicador_rp == "total RP") %>%
  arrange(total)


cor_ministerio <- ifelse(dados_grafico$ministerio == "mdic", "orange", "gray")




dados_grafico %>%
  mutate(total = round(total/10^9, 1)) %>%
  mutate(ministerio_nome = reorder(ministerio_nome, total)) %>%
  mutate(sinal= as.character(sign(total))) %>%
  ggplot(aes(x = total, y = ministerio_nome)) +
  geom_col() +
  geom_text(aes(label = total), 
            hjust = -0.1, 
            size = 2,
            show.legend = FALSE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size= 8, color = c(cor_ministerio))
  ) +
  labs(
    title = "Ranking de valor total do PLOA",
    subtitle = "Valores em RS bi",
    x = "",
    y = ""
  )


#ranking por variação do valor percentual total

dados_grafico<-
  ploa2024 %>%
  filter(indicador_rp == "total RP") %>%
  arrange(variacao_perc)
  

cor_ministerio <- ifelse(dados_grafico$ministerio == "mdic", "orange", "gray")



dados_grafico %>%
  mutate(variacao_perc = round(variacao_perc, 1)) %>%
  mutate(ministerio_nome = reorder(ministerio_nome, variacao_perc)) %>%
  mutate(sinal= as.character(sign(variacao_perc))) %>%
  ggplot(aes(x = variacao_perc, y = ministerio_nome)) +
  geom_col(aes(fill = sinal),show.legend = FALSE) +
  geom_text(aes(label = paste0(variacao_perc,"%")), 
            hjust = -0.1, 
            size = 2,
            show.legend = FALSE) +
  scale_fill_manual(values = c("red","white", "blue")) +  # Define as cores para valores negativos e positivos
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size= 8, color = c(cor_ministerio))
  ) +
  labs(
    title = "Ranking de variação percentual do PLOA",
    x = "",
    y = ""
  )




#Ranking de variação total do PLOA

dados_grafico<-
  ploa2024 %>%
  filter(indicador_rp == "total RP") %>%
  arrange(variacao)


cor_ministerio <- ifelse(dados_grafico$ministerio == "mdic", "orange", "gray")



dados_grafico %>%
  mutate(variacao = round(variacao/10^6, 1)) %>%
  mutate(ministerio_nome = reorder(ministerio_nome, variacao)) %>%
  mutate(sinal= as.character(sign(variacao))) %>%
  ggplot(aes(x = variacao, y = ministerio_nome)) +
  geom_col(aes(fill = sinal),show.legend = FALSE) +
  geom_text(aes(label = variacao), 
            hjust = -0.1, 
            size = 2,
            show.legend = FALSE) +
  scale_fill_manual(values = c("red", "blue")) +  # Define as cores para valores negativos e positivos
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size= 8, color = c(cor_ministerio))
  ) +
  labs(
    title = "Ranking de variação total do PLOA",
    subtitle = "Valores em RS mi",
    x = "",
    y = ""
  )


  


#ranking por valor total de emendas
dados_grafico_sumarizado<-
  ploa2024 %>%
  filter(substr(indicador_rp,1,1) %in% c("6","7","8")) %>%
  summarise(total = sum(total),
            .by = ministerio) %>%
  arrange(total)


cor_ministerio <- ifelse(dados_grafico_sumarizado$ministerio == "mdic", "orange", "gray")


fab<-
  ploa2024 %>%
  filter(substr(indicador_rp,1,1) %in% c("6","7","8")) %>%
  mutate(indicador_rp = case_when(
    substr(indicador_rp,1,1) == "6" ~ "Individuais-obrigatória",
    substr(indicador_rp,1,1) == "7" ~ "Bancada-impositiva",
    substr(indicador_rp,1,1) == "8" ~ "Comissão permanente"
  )) %>%
  mutate(total = round(total/10^6, 1)) %>%
  mutate(ministerio_nome = fct_reorder(ministerio_nome, .x= total,.fun= sum))

ploa2024 %>%
  filter(substr(indicador_rp,1,1) %in% c("6","7","8")) %>%
  mutate(indicador_rp = case_when(
    substr(indicador_rp,1,1) == "6" ~ "Individuais-obrigatória",
    substr(indicador_rp,1,1) == "7" ~ "Bancada-impositiva",
    substr(indicador_rp,1,1) == "8" ~ "Comissão permanente"
  )) %>%
  mutate(total = round(total/10^6, 1)) %>%
  mutate(ministerio_nome = fct_reorder(ministerio_nome, .x= total,.fun= sum)) %>%
  ggplot(aes(x = total, y = ministerio_nome)) +
  geom_col() +
  geom_text(aes(label = total), 
            hjust = c(rep(-0.1,63),rep(1,3),rep(-0.1,9)),
            color = c(rep("black",63),rep("white",3),rep("black",9)),
            size = 2,
            show.legend = FALSE) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(size= 8, color = c(cor_ministerio))
  ) +
  labs(
    title = "Ranking por valor total de emendas",
    subtitle = "Valores em RS mi",
    x = "",
    y = ""
  ) +
  facet_wrap(indicador_rp~.)
