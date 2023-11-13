# instala os pacotes necessários
install.packages("corrplot")
install.packages("OpenRepGrid")
install.packages("RColorBrewer")
install.packages("scales")
install.packages("writexl")

# carrega os pacotes
library(corrplot)
library(OpenRepGrid)
library(RColorBrewer)
library(scales)
library(writexl)

# indica diretório atual
getwd()

# importa planilha de excel para o dataframe rgt_grid e transforma em um grid
rgt_grid_pre_teste_covid <- importExcel("/home/marisa/Documentos/acadêmico/UFPR/TCC/usabilidade_ux_paineis_covid/dados/rgt_paineis_covid.xlsx")

# faz a análise de componentes principais (PCA), com padrão de rotação Varimax e reduzindo a duas dimensões
constructPca(rgt_grid_pre_teste_covid, nf = 2)

# gera gráficos Biplot 3D e Biplot simples
#biplot3d(rgt_grid_pre_teste_covid_semacento, labels.c = TRUE, e.cex = 1, c.text.col = "gray49", e.text.col = "darkblue")
biplotSimple(rgt_grid_pre_teste_covid, 
             c.label.col = "#405a78", 
             e.label.cex = 1,  
             c.label.cex = 0.95, 
             zoom = 1) 

# gera matriz ordenável de Bertin e plota
# obs.: células claras correspondem a pontuações baixas (menor preferência) e escuras, a altas (maior preferência)
bertin(rgt_grid_pre_teste_covid[1:12, 1:4], 
       color=c("white", "#405a78"), 
       xlim = c(0.3,0.4), 
       ylim = c(.0, .8), 
       cex.elements = .8, 
       cex.text = .8,
       lheight = 0.8,
       id = c(F, F))

# clusteriza construtos e os painéis para gerar dendrogramas
cluster(rgt_grid_pre_teste_covid, xlim = c(14,0)) 

# configuração para melhor visualização do agrupamento de painéis
cluster(rgt_grid_pre_teste_covid,
        along = 0, dmethod = "euclidean",
        cmethod = "ward",
        p = 2,
        align = TRUE,
        trim = NA,
        main = NULL,
        mar = c(4, 2, 3, 15),
        cex = 0,
        lab.cex = 1.2,
        cex.main = 0.9,
        print = TRUE,
        xlim = c(12,0))

# salva as estatísticas básicas do RGT em um dataframe
estatisticas_pre_teste_covid <- statsConstructs(rgt_grid_pre_teste_covid)

# gera uma matriz de correlação e salva em um dataframe, usando coeficiente de Pearson
# obs.: útil para visualizar rapidamente correlações mais fortes
matriz_correlacao_pre_teste_covid <- constructCor(rgt_grid_pre_teste_covid)

# cria corrplot para visualizar a matriz de correlação
#paleta <-c("#9c3838","#BB4444", "#e66565", "#EE9988", "#ffccc2","#FFFFFF", "#a9c9e8", "#8bbae8","#70a6db", "#5b92c9", "#2a66a3")
#corrplot(matriz_correlacao_pre_teste_covid, method ='color', type = 'lower', number.font = 0.5, addCoef.col = "black", col = paleta,  tl.col = "black", tl.cex = 0.8)       
corrplot(matriz_correlacao_pre_teste_covid, method ='color', type = 'lower', number.font = 0.5, tl.col = "black", tl.cex = 0.8)       

corrplot(matriz_correlacao_pre_teste_covid, method ='color', type = 'lower', tl.pos = "n", tl.cex = 0.8)       

# exporta um csv com a matriz de correlação desejada
write.csv(matriz_correlacao_pre_teste_covid, "matriz_pre_teste_covid.csv")

# passo a passo para criar as visualizações  individuais para os usuários
# extrai subconjuntos do RGT
id1<-rgt_grid_pre_teste_covid[1:4, 1:4]    
id2<-rgt_grid_pre_teste_covid[5:8, 1:4]
id3<-rgt_grid_pre_teste_covid[9:12, 1:4]

# cria as matrizes de correlação individuais
matriz_correlacao_id1 <- constructCor(id1)
matriz_correlacao_id2 <- constructCor(id2)
matriz_correlacao_id3 <- constructCor(id3) 

# plota as matrizes de correlação individuais
corrplot(matriz_correlacao_id1, method ='number', type = "lower", number.cex = 0.8, tl.col = "black",  cl.cex = 0.8)  
corrplot(matriz_correlacao_id2, method ='number', type = "lower", number.cex = 0.8, tl.col = "black",  cl.cex = 0.8)
corrplot(matriz_correlacao_id3, method ='number', type = "lower", number.cex = 0.8, tl.col = "black",  cl.cex = 0.8)

corrplot(matriz_correlacao_id1, method ='color', type = "lower", number.cex = 0.8, tl.pos = "n", cl.cex = 0.8)  
corrplot(matriz_correlacao_id2, method ='color', type = "lower", number.cex = 0.8, tl.pos = "n", cl.cex = 0.8)
corrplot(matriz_correlacao_id3, method ='color', type = "lower", number.cex = 0.8, tl.pos = "n", cl.cex = 0.8)


# gera matrizes ordenáveis de Bertin individuais
bertin(id1,
       color = c("white", "#405a78"), 
       xlim = c(0.3,0.4), 
       ylim = c(.0, 0.5), 
       cex.elements = .8, 
       cex.text = .8,
       lheight = 0.6,
       id = c(F, F), 
       margins = c(0,0.7,0.7))

bertin(id2,
       color = c("white", "#405a78"), 
       xlim = c(0.3,0.4), 
       ylim = c(.0, 0.5), 
       cex.elements = .8, 
       cex.text = .8,
       lheight = 0.9,
       id = c(F, F), 
       margins = c(0,0.7,0.7))

bertin(id3, 
       color = c("white", "#405a78"), 
       xlim = c(0.3,0.4), 
       ylim = c(.0, 0.5), 
       cex.elements = .8, 
       cex.text = .8,
       lheight = 0.6,
       id = c(F, F), 
       margins = c(0,0.7,0.7))

# clusteriza construtos e os painéis para gerar dendrogramas individuais
cluster(id1,lab.cex=1,xlim = c(14,0))
cluster(id2,lab.cex=1,xlim = c(14,0))
cluster(id3,lab.cex=1,xlim = c(14,0))

# configuração para melhor visualização do agrupamento de painéis
cluster(id1,
        along = 0, dmethod = "euclidean",
        cmethod = "ward",
        p = 2,
        align = TRUE,
        trim = NA,
        main = NULL,
        mar = c(4, 2, 3, 15),
        cex = 0,
        lab.cex = 1.2,
        cex.main = 0.9,
        print = TRUE,
        xlim = c(12,0))

cluster(id2,
        along = 0, dmethod = "euclidean",
        cmethod = "ward",
        p = 2,
        align = TRUE,
        trim = NA,
        main = NULL,
        mar = c(4, 2, 3, 15),
        cex = 0,
        lab.cex = 1.2,
        cex.main = 0.9,
        print = TRUE,
        xlim = c(10,0))

cluster(id3,
        along = 0, dmethod = "euclidean",
        cmethod = "ward",
        p = 2,
        align = TRUE,
        trim = NA,
        main = NULL,
        mar = c(4, 2, 3, 15),
        cex = 0,
        lab.cex = 1.2,
        cex.main = 0.9,
        print = TRUE,
        xlim = c(6,0))

# gera gráficos Biplot simples individuais
biplotSimple(id1, c.label.col="darkblue", e.label.cex = 0.8, c.label.cex = 0.7, zoom = 0.90)

biplotSimple(id1,
  c.label.col = "#405a78", 
  e.label.cex = 1,  
  c.label.cex = 0.95, 
  zoom = 0.9) 

biplotSimple(id2, c.label.col="darkblue", e.label.cex = 0.8, c.label.cex = 0.7, zoom = 0.90)
biplotSimple(id2,
             c.label.col = "#405a78", 
             e.label.cex = 1,  
             c.label.cex = 0.95, 
             zoom = 0.9) 

biplotSimple(id3, c.label.col="darkblue", e.label.cex = 0.8, c.label.cex = 0.7, zoom = 0.90)
biplotSimple(id3,
             c.label.col = "#405a78", 
             e.label.cex = 1,  
             c.label.cex = 0.95, 
             zoom = 0.9) 
