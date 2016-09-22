# Ygor Rodolfo Gomes dos Santos
# 111210452

library(tm)
library(FastKNN)
library(SnowballC)

# Caminho do diretório onde estão os arquivos de legendas
PATH = "~/sri/Lab03/Legendas/"

# Arquivos de legendas
arquivos <- dir(PATH)

# Número (N) de filmes que o usuário irá avaliar se é de seu interesse ou não
NUM_FILMES_TESTE = 5

# Lista que irá conter o(s) filme(s) interessante(s) para o usuário
filmes.usuario = c()

# Seleciona N filmes aleatórios que serão apresentados ao usuário
filmes.oferta <- sample(1:length(arquivos), NUM_FILMES_TESTE, replace=F) 

# Laço onde o sistema irá perguntar se o usuário gosta de determinado filme ou não
for(i in 1: NUM_FILMES_TESTE) {
  filme.indice <- filmes.oferta[i]
  pergunta <- paste0("Você Gosta do Filme: ",arquivos[filme.indice], "? (s ou n) ")
  resposta <- readline(prompt=pergunta)
  # Em caso afirmativo, o filme selecionado será adicionado a lista dos filmes escolhidos
  if (resposta == "s"){
    numero.filmes.usuario <- length(filmes.usuario) + 1
    filmes.usuario[numero.filmes.usuario] <- filme.indice
  }
}

# Documento que conterá o conteúdo de todos os filmes que o usuário demostrou interesse
doc.usuario <- "" 
for(i in 1:length(filmes.usuario)){
  indice = filmes.usuario[i]
  nome <- paste(PATH, arquivos[indice], sep="")
  doc <- readChar(nome,file.info(nome)$size)
  doc.usuario <- paste(doc.usuario,doc) 
}

# Lista que irá conter os documentos formatados exceto os que o usuário selecionou
doc.lista = c()

indice <- 0
# Laço para colocar os arquivos no formato de documento e adiciona-los na lista
for(i in 1:length(arquivos)){
  # Seleciona apenas os filmes que o usuário não se interessou
  if (!(i %in% filmes.usuario)) {
    indice <- indice + 1
    nome <- paste(PATH, arquivos[i], sep="")
    doc <- readChar(nome,file.info(nome)$size)
    doc.lista[indice] <- doc
  } 
}

# Adiciona o documento com o conteúdo dos filmes selecionados pelo usuário na lista de documentos
doc.lista[length(doc.lista)+1] <- doc.usuario

numero.docs <- length(doc.lista)
names(doc.lista) <- paste0("doc", c(1:numero.docs))

# Cria o vetor dos documentos
documentos <- VectorSource(doc.lista)
documentos$Names <- names(doc.lista)

corpus <- Corpus(documentos)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

# Matriz documento-termo com todos os documentos
term.doc.matrix.stm <- DocumentTermMatrix(corpus)
term.doc.matrix <- as.matrix(term.doc.matrix.stm)  

# Matriz tfidf com todos os documentos
tfidf.matrix = weightTfIdf(term.doc.matrix.stm)
colnames(tfidf.matrix) <- colnames(term.doc.matrix)

# Gera a matriz de teste.
matrixTeste = as.matrix(tfidf.matrix[numero.docs,])

# Gera a matriz de treino
matrixTreino <- tfidf.matrix[1:numero.docs-1,]

# Gera a matriz de distancia para o KNN
matrixDistancia <-  Distance_for_KNN_test(matrixTeste, matrixTreino)

# Número de filmes a serem recomendados (TOP K filmes)
TOP_K <- 10

# Mostrar filmes a serem recomendados baseando-se na similaridade
similares <- k.nearest.neighbors(1,distance_matrix = matrixDistancia, k=TOP_K)

# Imprime o nome dos filmes recomendados
print("Filmes Recomendados: ")
for(i in 1:length(similares)){
  print(arquivos[similares[i]])
}
