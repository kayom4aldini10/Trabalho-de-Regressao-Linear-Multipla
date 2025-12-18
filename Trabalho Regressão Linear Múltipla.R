# Pacotes
library(dplyr)
library(ggplot2)

# Leitura dos dados
treino <- read.csv(file.choose(), header = TRUE)
teste  <- read.csv(file.choose(), header = TRUE)

# Remover valores ausentes
treino <- na.omit(treino)
teste  <- na.omit(teste)

# Transformar Sex em fator
treino$Sex <- as.factor(treino$Sex)
teste$Sex  <- as.factor(teste$Sex)

# Gráfico de barras da variável Sex
ggplot(treino, aes(x = Sex)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribuição da variável 'Sex'")

# Criar coluna id no teste, se não existir
if (!"id" %in% names(teste)) {
  teste$id <- 1:nrow(teste)
}

# Selecionar variáveis numéricas
numericas <- treino %>% select(where(is.numeric))

# Matriz de correlação
correlacao <- cor(numericas)
print(round(correlacao, 2))

# Ajuste do modelo de regressão linear múltipla
modelo_melhro <- lm(
  Age ~ Length + Diameter + Height + Shucked.Weight + Viscera.Weight,
  data = treino
)

# Resumo do modelo
summary(modelo_melhro)

# Gráfico dos resíduos
plot(
  modelo_melhro$residuals,
  main = "Resíduos do Modelo",
  ylab = "Resíduos"
)
abline(h = 0, col = "red")

# Coeficientes do modelo
coef(modelo_melhro)

# Predições no conjunto de teste
predicoes <- predict(modelo_melhro, newdata = teste)

# Criar arquivo de submissão
submission <- data.frame(
  id  = teste$id,
  Age = predicoes
)

# Salvar CSV
write.csv(
  submission,
  "D:/usuario/Documents/Estat 3/submission3.csv",
  row.names = FALSE
)
