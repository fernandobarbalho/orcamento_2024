library(tidyverse)
library(readxl)

arquivos <- list.files("data/")

arquivos<- paste0("data/",arquivos)

tabela_final<-
  purrr::map_dfr(1:length(arquivos), function(i){
    
    print(arquivos[i])
    
    tamanho_nome<- str_length(arquivos[i])
    ministerio<-str_sub(arquivos[i],31,tamanho_nome-4)  
    
    dados_rp <- read_excel(arquivos[i], 
                           col_types = c("skip", "numeric", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric"))
    
    names(dados_rp) <- c("ministerio","indicador_rp", "pl","acrescimo","cancelamento","total")
    
    dados_rp$ministerio<- ministerio
    dados_rp$indicador_rp[NROW(dados_rp)]<- "total RP"
    
    dados_rp
    
  })

tabela_final$variacao<- tabela_final$total- tabela_final$pl
tabela_final$variacao_perc<- (tabela_final$variacao/tabela_final$pl)*100 

unique(tabela_final$ministerio)


ministerio <- c("Igualdade_racial", "M_Mulheres", "mapa", "MC", "MCidades", "mcti",
                 "MD", "MDA", "MDH", "mdic", "MDS", "mec", "MEsporte", "MF", "MGI",
                 "MI", "minc", "MJ", "MMA", "MME", "MP", "MPesca", "MPI", "MPortos",
                 "MPS", "MRE", "MS", "MT", "MTE", "MTUR")

ministerio_nome <- c("Ministério da Igualdade racial", 
                                    "Ministério das Mulheres", 
                                    "Ministério da Agricultura Pecuária e Abastecimento", 
                                    "Ministério das Comunicações", 
                                    "Ministério das Cidades", 
                                    "Ministério da Ciência Tecnologia e Inovação",
                                    "Ministério da Defesa", 
                                    "Ministério do Desenvolvimento Agrário", 
                                    "Ministério dos Direitos Humanos", 
                                    "Ministério do Desenvolvimento Indústria e Comércio", 
                                    "Ministério do Desenvolvimento Social", 
                                    "Ministério da Educação", 
                                    "Ministério dos Esportes", 
                                    "Ministério da Fazenda", 
                                    "Ministério da Gestão e Inovação",
                                    "Ministério da Integração Nacional", 
                                    "Ministério da Cultura", 
                                    "Ministério da Justiça e Segurança Pública", 
                                    "Ministério do Meio Ambiente", 
                                    "Ministério das Minas e Energia", 
                                    "Ministério do Planejamento e Orçamento", 
                                    "Ministério da Pesca", 
                                    "Ministério dos Povos Indígenas", 
                                    "Ministério dos Portos e Aeroportos",
                                    "Ministério da Previdência Social", 
                                    "Ministério das Relações Exteriores", 
                                    "Ministério da Saúde", 
                                    "Ministério dos Transportes", 
                                    "Ministério do Trabalho e Emprego", 
                                    "Ministério do Turismo")

df_ministerio<- tibble(ministerio = ministerio, ministerio_nome = ministerio_nome)

tabela_trabalho<-
df_ministerio %>%
  inner_join(tabela_final)


tabela_trabalho %>%
  write.csv2("ploa2024_rp.csv")
