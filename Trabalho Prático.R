############################################################################################################
###########         Trabalho Pratico - AED         #####################################
#############          IGTI - Professor Mairon Chaves             #################################
############################################################################################################


#Forma o conjunto de dados historico contendo vinte locacoes sorteadas aleatoriamente do banco de dados e a armazena
#em um data frame chamado dados

dados <- data.frame(
  Preco = c(368.384514890573, 446.850186825816, 
            414.72765691978, 434.291090918223, 436.652686535348, 457.65797344255, 
            490.694346597566, 474.881781399868, 458.462395897205, 412.719412673294, 
            448.799032112411, 352.040747235864, 449.461858221104, 416.150953927119, 
            416.499426750268, 551.315803331779, 462.126789471159, 515.957335395508, 
            467.598697162974, 339.548470369391), 
  Portas = c("duas_portas", "quatro_portas", "duas_portas", "quatro_portas", "quatro_portas", 
             "duas_portas", "quatro_portas", "duas_portas", "quatro_portas", 
             "duas_portas", "quatro_portas", "quatro_portas", "duas_portas", 
             "quatro_portas", "duas_portas", "quatro_portas", "quatro_portas", 
             "duas_portas", "quatro_portas", "quatro_portas"),
  Ar_Condicionado = c("sem_ar_condicionado",  "com_ar_condicionado", "com_ar_condicionado", "com_ar_condicionado", 
                      "com_ar_condicionado", "com_ar_condicionado", "com_ar_condicionado", 
                      "com_ar_condicionado", "com_ar_condicionado", "com_ar_condicionado", 
                      "com_ar_condicionado", "sem_ar_condicionado", "com_ar_condicionado", 
                      "com_ar_condicionado", "com_ar_condicionado", "com_ar_condicionado", 
                      "com_ar_condicionado", "com_ar_condicionado", "com_ar_condicionado", 
                      "sem_ar_condicionado"),
  Quadrimestre = c("segundo_quadrimestre","segundo_quadrimestre", "segundo_quadrimestre", "segundo_quadrimestre", 
                   "segundo_quadrimestre", "terceiro_quadrimestre", "primeiro_quadrimestre", 
                   "primeiro_quadrimestre", "terceiro_quadrimestre", "segundo_quadrimestre", 
                   "terceiro_quadrimestre", "segundo_quadrimestre", "terceiro_quadrimestre", 
                   "segundo_quadrimestre", "segundo_quadrimestre", "primeiro_quadrimestre", 
                   "terceiro_quadrimestre", "primeiro_quadrimestre", "primeiro_quadrimestre", 
                   "segundo_quadrimestre"), 
  Idade_Locatario = c(23, 18, 28, 21, 18, 21, 18, 20, 25, 29, 18, 33, 20, 21, 18, 21, 18, 20, 25, 29),
  Quilometragem = c(957.442780544097, 829.533278217768, 923.300215829467, 871.519116905113, 930.704105677958, 554.696695914233, 501.941059782271, 
                    665.435074822519, 568.24079543466, 930.704105677958, 554.696695914233, 
                    829.533278217768, 665.435074822519, 871.519116905113, 930.704105677958, 
                    351.547138218644, 501.941059782271, 447.872006186523, 568.24079543466, 
                    930.704105677958), 
  Dolar = c(4.41147933990862, 5.63014407874318, 
            8.80557934010615, 4.260591319988649, 6.93416279643155, 1.61130694543154, 
            2.57813244655973, 4.66666728709914, 1.6846066723224, 7.33872353619711, 
            4.52300814589177, 2.96689816205009, 9.91448182957733, 8.55577847959413, 
            5.93424935955983, 5.55775429484673, 6.94475470863839, 4.74330294976712, 
            4.723306965757987, 4.7010894862212))

View(dados)

#Explore a variável resposta, que é o Preço, e responda:

#Histograma do preco
hist(dados$Preco)

#Pelo histograma, você diria que a variável Preço segue uma distribuição normal? 
#Sim, pela análise gráfica a variável preço aparenta seguir uma distribuição normal, 
#pois os dados são distribuídos simetricamente em torno de um valor central.



#Boxplot do preco
boxplot(dados$Preco)

#Pelo boxplot do Preço, você consegue visualizar algum outlier?
#Sim, possui um outlier superior e um inferior, ou seja, existe uma locação que o preço foi 
#muito acima do esperado e uma locação cujo preço foi muito abaixo do esperado. Antes de 
#remover estes outliers de uma análise, eles devem ser investigados sobre o que ocorreu 
#naquelas locações que fez com o que o valor fosse tão discrepante da distribuição da variável.


#Estatisticas descritivas do preco
summary(dados$Preco)

#Qual é o valor mediano do Preço e qual a sua interpretação CORRETA?
#A mediana é 447,8. Isso nos diz que 50% dos preços são até este valor e os demais 50% são acima deste valor.



#Explore a relação entre as variáveis  Preço e Quadrimestre, e responda:
#Boxplot entre o Preco e Quadrimestre
boxplot(dados$Preco~ dados$Quadrimestre)

#Através do boxplot, como o Preço se comporta em relação a cada Quadrimestre?
#O primeiro quadrimestre apresenta a maior mediana.
  


#Realiza analise de variancia
anova <- aov(Preco ~ Quadrimestre, data = dados)
summary(anova)


#Através de uma ANOVA, existe diferença significativa entre o Preço médio de pelo menos 
#um Quadrimestre em relação aos outros? Como chegou a essa conclusão? Adote 95% de confiança
#na sua interpretação.

#Sim, considerando o alfa de 0,05 e ao p valor de 0,000126, há evidências para rejeitar a 
#hipótese nula de igualdade de médias, ou seja, pelo menos um dos quadrimestres possui o preço
#médio diferente dos demais.





#Explore a relação entre as variáveis Preço e Portas, responda:

boxplot(dados$Preco ~ dados$Portas)

#Test t de Student
t.test(dados$Preco ~ dados$Portas , 
       paired = FALSE, #amostras nao pareadas
       alternative = 'two.sided', #bilateral
       conf.level = 0.95 #95% de confianca
)


#Através de um teste t de Student para amostras independentes, existe diferença significativa 
#entre o preço médio do aluguel do veículo com duas portas quando comparado com o preço médio 
#do veículo de quatro portas? Adote 95% de confiança ao realizar na sua interpretação.

#Não, pois ao p valor de 0,8884 não há evidências para rejeitar a hipótese nula de igualdade 
#de médias, ou seja, não há diferença significativa entre o preço médio de veículos de duas 
#portas em relação ao preço médio de veículos de quatro portas.






#Explore a relação entre as variáveis Preço e Quilometragem, responda:

plot(y = dados$Preco ,
     x = dados$Quilometragem,
     pch = 16)

#Pelo gráfico de dispersão, você identifica que existe relação linear entre o Preço e a 
#Quilometragem? Se sim, a relação é positiva ou negativa?
#Sim, a relação é linear negativa, pois, à medida que a quilometragem aumenta, o preço diminui.

  


#Coeficiente de correlacao
cor(dados$Preco, dados$Quilometragem)

#Obtenha o valor do coeficiente de correlação linear de Pearson entre o Preço e a Quilometragem. 
#Qual a interpretação CORRETA?
#O coeficiente de correlação linear de Pearson é -0,82, isso nos informa que é uma correlação negativa alta.




#Ajuste regressao linear do Preco em funcao da Quilometragem
regressao_linear <- lm(Preco ~ Quilometragem, data = dados)
summary(regressao_linear)

#Se tentarmos utilizar somente a Quilometragem para prever o valor do Preço, o quanto 
#da variação do Preço a variável Quilometragem consegue explicar? Em outas palavras, 
#interprete o R2 da regressão linear do Preço em função da Quilometragem.

#O R2 é de 67,76%, ou seja, a variável Quilometragem consegue explicar 67,76% da variação do Preço.



#Analise descritiva da variavel quilometragem
summary(dados$Quilometragem)

#Qual o valor do primeiro quartil e qual a sua interpretação CORRETA?
#O primeiro quartil é 554,7, isso nos diz que até 25% dos veículos alugados possuem quilometragem até 554,7.

#Qual o valor do terceiro quartil e qual a sua interpretação CORRETA?
#O terceiro quartil é 925,2, isso nos diz que até 75% dos veículos alugados possuem quilometragem até 925,2.



#Coeficiente de variacao
sd(dados$Quilometragem) / mean(dados$Quilometragem)

#Qual é o valor do coeficiente de variação e qual a sua interpretação CORRETA?
#O coeficiente de variação é 27,74%, ou seja, os valores da variável quilometragem 
#variam em média 27,74% em torno de sua média. 
 

 
#Explore a correlacao entre o Dolar e o Preco
plot(y = dados$Preco,
     x = dados$Dolar,
     pch = 16)

cor(dados$Preco, dados$Dolar)

regressao_linear <- lm(Preco ~ Dolar, data = dados)
summary(regressao_linear)

#A correlação entre as variáveis dólar e preço é positiva ou negativa?
#O gráfico de dispersão não apresenta nenhum padrão entre as duas variáveis, ou seja, 
#na medida em que o Dólar aumenta, o Preço não cresce nem decresce, ou seja, não há correlação 
#entre as duas variáveis.

#Obtenha o coeficiente de correlação linear de Pearson entre o Dolar e o Preço. 
#Qual a interpretação CORRETA?
#O valor do coeficiente de correlação linear de Pearson é -0,06, que indica ausência de correlação linear.


#0.9 para mais ou para menos indica uma correlação muito forte.
#0.7 a 0.9 positivo ou negativo indica uma correlação forte.
#0.5 a 0.7 positivo ou negativo indica uma correlação moderada.
#0.3 a 0.5 positivo ou negativo indica uma correlação fraca.
#0 a 0.3 positivo ou negativo indica uma correlação desprezível.


#Se ajustarmos uma regressão linear entre o Dolar e o Preco, para tentar prever o Preço baseado no 
#Dólar, seria possível?

#Não seria possível, pois o p valor do coeficiente beta do Dolar é de 0,77 (77%), ou seja, independentemente
#do nível de significância adotado, a variável Dolar não exerce influência significativa na variável Preco, 
#portanto, não é possível prever o Preço baseado no Dólar.
  
  


