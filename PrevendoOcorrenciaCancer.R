# Prevendo a Ocorrência de Câncer
getwd()

##  Coletando os Dados
# Os dados do câncer da mama incluem 569 observações de biópsias de câncer, 
# cada um com 32 características (variáveis). 
#O diagnóstico é codificado como "M" para indicar maligno ou "B" para 
# indicar benigno.
#ID NÃO É RELEVANTE PARA A ANÁLISE

#Carregando dados
dados <- read.csv("dataset.csv", stringsAsFactors = FALSE)
str(dados)
View(dados)

## Etapa 2 - Pré-Processamento
#exclusao da coluna ID atribuindo vaores nulos
dados$id = NULL

# Ajustando o label da variável alvo M --> MALIGO E CONVERTENDO B --> BENIGNO
dados$diagnosis = sapply(dados$diagnosis, function(x){ifelse(x=='M', 'Maligno', 'Benigno')})

# CONVERTENDO EM VARIÁVEIS TIPO FATOR  
table(dados$diagnosis)
dados$diagnosis <- factor(dados$diagnosis, levels = c("Benigno", "Maligno"), labels = c("Benigno", "Maligno"))
str(dados$diagnosis)


# Verificando a proporção entre malignos e benignos
round(prop.table(table(dados$diagnosis)) * 100, digits = 1) 

# Medidas de Tendência Central - vericiando variáveis numéricas
summary(dados[c("radius_mean", "area_mean", "smoothness_mean")])

# Criando um função de normalização ( cada valor recebido subitrai do valor minimo de x e divide pelo valor maximo de x)
#normalizando os dados
normalizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Testando a função de normalização - os resultados devem ser idênticos
normalizar(c(1, 2, 3, 4, 5))
normalizar(c(10, 20, 30, 40, 50))

# Normalizando os dados COLUNA 1 É O ID, NÃO SENDO INCLUÍDA
dados_norm <- as.data.frame(lapply(dados[2:31], normalizar))
#Visualizando dados normalizados
View(dados_norm)

#_______________________________________________________________________________
## Modelo treinado com KNN
#_______________________________________________________________________________
# Carregando o pacote library
# install.packages("class")
library(class)
?knn

# Criando dados de treino e dados de teste
#PRIMEIRA LINHA EM TREINO E SEGUNDA EM TESTE SELECIONOU AS PRIMEIRAS LINHAS
dados_treino <- dados_norm[1:469, ]
dados_teste <- dados_norm[470:569, ]

# Criando os labels para os dados de treino e de teste
dados_treino_labels <- dados[1:469, 1]
#IDENTIFICANDO TREINO E TESTE
dados_teste_labels <- dados[470:569, 1]
length(dados_treino_labels)
length(dados_teste_labels)

# Criando o modelo - KNN INDICANDO DADOS DE TREINO E TESTE INSERINDO O LABEL
modelo_knn_v1 <- knn(train = dados_treino, 
                     test = dados_teste,
                     cl = dados_treino_labels, 
                     k = 21)
#21 DADOS MAIS PROXIMOS DO PONTO DOS DADOS

# SUMARIO DO MODELO
summary(modelo_knn_v1)


## AVALIANDO E TREINANDO MODELO

# INSTALANDO E CARREGANDO PACOTE
library(gmodels)

#MATRIZ DE CONFUSAO
# Criando uma tabela cruzada dos dados previstos x dados atuais
# Usaremos amostra com 100 observações: length(dados_teste_labels)
CrossTable(x = dados_teste_labels, y = modelo_knn_v1, prop.chisq = FALSE)

#VERIFICANDO SE O VALOR OBSERVADO FOI O PREVISTO

##Confusion Matrix		
#_____________________________________________________________________
##               Sim (Observado)	Não (Observado)
#Sim (Previsto)	TP	               FP
#Não (Previsto)	FN	               TN
#legenda
#TP	True Positive
#FP	False Positive
#FN	False Negative
#TN	True Negative
#_________________________________________________________________
#FALSO POSITIVO
#FALSO NEGATIVO
#AMBOS SAO RUINS, ERRO NO MODELO

# Temos:
# Cenário 1: Célula Benigno (Observado) x Benigno (Previsto) - 61 casos - true positive 
# Cenário 2: Célula Maligno (Observado) x Benigno (Previsto) - 00 casos - false positive (o modelo errou)
# Cenário 3: Célula Benigno (Observado) x Maligno (Previsto) - 02 casos - false negative (o modelo errou)
# Cenário 4: Célula Maligno (Observado) x Maligno (Previsto) - 37 casos - true negative 

# Lendo a Confusion Matrix (Perspectiva de ter ou não a doença):

# True Negative  = nosso modelo previu que a pessoa NÃO tinha a doença e os dados mostraram que realmente a pessoa NÃO tinha a doença
# False Positive = nosso modelo previu que a pessoa tinha a doença e os dados mostraram que NÃO, a pessoa tinha a doença
# False Negative = nosso modelo previu que a pessoa NÃO tinha a doença e os dados mostraram que SIM, a pessoa tinha a doença
# True Positive = nosso modelo previu que a pessoa tinha a doença e os dados mostraram que SIM, a pessoa tinha a doença
#__________________________________
# Falso Positivo - Erro Tipo I
# Falso Negativo - Erro Tipo II
#__________________________________
#MODELO ACERTOU 98 EM 100, TAXA DE ERRO 2%
# Taxa de acerto do Modelo: 98% (acertou 98 em 100)


## OTIMIDANDO MODELO

# Usando a função scale() para padronizar o z-score 
#MESMA ESCALA DO ESCORE Z
#CENTRALIZA 
?scale()
dados_z <- as.data.frame(scale(dados[-1]))
#MUDANDO A TECNICA DE PROCESSAMENTO, UTILIZANDO O BANCO ORIGINAL

# Confirmando transformação realizada com sucesso
summary(dados_z$area_mean)
#MEDIA = 0

# Criando novos datasets de treino e de teste
dados_treino <- dados_z[1:469, ]
dados_teste <- dados_z[470:569, ]

dados_treino_labels <- dados[ 1: 469, 1] 
dados_teste_labels <- dados[ 470: 569, 1]

# Reclassificando
modelo_knn_v2 <- knn(train = dados_treino, 
                     test = dados_teste,
                     cl = dados_treino_labels, 
                     k = 21)

# Criando uma tabela cruzada dos dados previstos x dados atuais
CrossTable(x = dados_teste_labels, y = modelo_knn_v2, prop.chisq = FALSE)
#PIOROU O MODELO COM MAIS ERRO.

#_________________________________________________________________________
#OUTRO ALGORITIMO, PODE SER UTILIZADO PARA REGRESSAO OU CLASSIFICAÇAO 
#AJUSTAR APENAS O type = 'C-classification'
# MODELO COM Algoritmo Support Vector Machine (SVM)

# Definindo a semente para resultados reproduzíveis
#REPRODUCAO DOS MESMOS RESULTADOS
set.seed(40) 

# Prepara o dataset
dados <- read.csv("dataset.csv", stringsAsFactors = FALSE)
dados$id = NULL
dados[,'index'] <- ifelse(runif(nrow(dados)) < 0.8,1,0)
View(dados)

# Dados de treino e teste
#DIVIDIDNDO TREINO E TESTE, INDICE RANDOMICO, NAO MANTEM O MESMO;
trainset <- dados[dados$index==1,]
testset <- dados[dados$index==0,]

# Obter o índice 
trainColNum <- grep('index', names(trainset))

# Remover o índice dos datasets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

# Obter índice de coluna da variável target no conjunto de dados // UTILIZADO O INÍCIO DA VARIAVEL
typeColNum <- grep('diag',names(dados))

# Cria o modelo
# Nós ajustamos o kernel para radial, já que este conjunto de dados não tem um 
# plano linear que pode ser desenhado
library(e1071)
?svm
modelo_svm_v1 <- svm(diagnosis ~ ., 
                     data = trainset, 
                     type = 'C-classification', 
                     kernel = 'radial') 


# Previsões

# Previsões nos dados de treino
pred_train <- predict(modelo_svm_v1, trainset) 

# Percentual de previsões corretas com dataset de treino
mean(pred_train == trainset$diagnosis)  


# Previsões nos dados de teste
pred_test <- predict(modelo_svm_v1, testset) 

# Percentual de previsões corretas com dataset de teste
mean(pred_test == testset$diagnosis)  

# Confusion Matrix
table(pred_test, testset$diagnosis)

#_________________________________________________________
#  Modelo com Algoritmo Random Forest
#________________________________________________________

# Criando o modelo
library(rpart)
modelo_rf_v1 = rpart(diagnosis ~ ., data = trainset, control = rpart.control(cp = .0005)) 

# Previsões nos dados de teste
tree_pred = predict(modelo_rf_v1, testset, type='class')

# Percentual de previsões corretas com dataset de teste
mean(tree_pred==testset$diagnosis) 
#PIOROU O MODELO, APESAR DE SER UM BOM MODELO, MAIS PRECISO.

# Confusion Matrix
table(tree_pred, testset$diagnosis)

#FICARIA COM O MODELO SVM COMO MODELO.



