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

#Explore a vari??vel resposta, que ?? o Pre??o, e responda:

#Histograma do preco
hist(dados$Preco)

#Pelo histograma, voc?? diria que a vari??vel Pre??o segue uma distribui????o normal? 
#Sim, pela an??lise gr??fica a vari??vel pre??o aparenta seguir uma distribui????o normal, 
#pois os dados s??o distribu??dos simetricamente em torno de um valor central.



#Boxplot do preco
boxplot(dados$Preco)

#Pelo boxplot do Pre??o, voc?? consegue visualizar algum outlier?
#Sim, possui um outlier superior e um inferior, ou seja, existe uma loca????o que o pre??o foi 
#muito acima do esperado e uma loca????o cujo pre??o foi muito abaixo do esperado. Antes de 
#remover estes outliers de uma an??lise, eles devem ser investigados sobre o que ocorreu 
#naquelas loca????es que fez com o que o valor fosse t??o discrepante da distribui????o da vari??vel.


#Estatisticas descritivas do preco
summary(dados$Preco)

#Qual ?? o valor mediano do Pre??o e qual a sua interpreta????o CORRETA?
#A mediana ?? 447,8. Isso nos diz que 50% dos pre??os s??o at?? este valor e os demais 50% s??o acima deste valor.



#Explore a rela????o entre as vari??veis  Pre??o e Quadrimestre, e responda:
#Boxplot entre o Preco e Quadrimestre
boxplot(dados$Preco~ dados$Quadrimestre)

#Atrav??s do boxplot, como o Pre??o se comporta em rela????o a cada Quadrimestre?
#O primeiro quadrimestre apresenta a maior mediana.
  


#Realiza analise de variancia
anova <- aov(Preco ~ Quadrimestre, data = dados)
summary(anova)


#Atrav??s de uma ANOVA, existe diferen??a significativa entre o Pre??o m??dio de pelo menos 
#um Quadrimestre em rela????o aos outros? Como chegou a essa conclus??o? Adote 95% de confian??a
#na sua interpreta????o.

#Sim, considerando o alfa de 0,05 e ao p valor de 0,000126, h?? evid??ncias para rejeitar a 
#hip??tese nula de igualdade de m??dias, ou seja, pelo menos um dos quadrimestres possui o pre??o
#m??dio diferente dos demais.





#Explore a rela????o entre as vari??veis Pre??o e Portas, responda:

boxplot(dados$Preco ~ dados$Portas)

#Test t de Student
t.test(dados$Preco ~ dados$Portas , 
       paired = FALSE, #amostras nao pareadas
       alternative = 'two.sided', #bilateral
       conf.level = 0.95 #95% de confianca
)


#Atrav??s de um teste t de Student para amostras independentes, existe diferen??a significativa 
#entre o pre??o m??dio do aluguel do ve??culo com duas portas quando comparado com o pre??o m??dio 
#do ve??culo de quatro portas? Adote 95% de confian??a ao realizar na sua interpreta????o.

#N??o, pois ao p valor de 0,8884 n??o h?? evid??ncias para rejeitar a hip??tese nula de igualdade 
#de m??dias, ou seja, n??o h?? diferen??a significativa entre o pre??o m??dio de ve??culos de duas 
#portas em rela????o ao pre??o m??dio de ve??culos de quatro portas.






#Explore a rela????o entre as vari??veis Pre??o e Quilometragem, responda:

plot(y = dados$Preco ,
     x = dados$Quilometragem,
     pch = 16)

#Pelo gr??fico de dispers??o, voc?? identifica que existe rela????o linear entre o Pre??o e a 
#Quilometragem? Se sim, a rela????o ?? positiva ou negativa?
#Sim, a rela????o ?? linear negativa, pois, ?? medida que a quilometragem aumenta, o pre??o diminui.

  


#Coeficiente de correlacao
cor(dados$Preco, dados$Quilometragem)

#Obtenha o valor do coeficiente de correla????o linear de Pearson entre o Pre??o e a Quilometragem. 
#Qual a interpreta????o CORRETA?
#O coeficiente de correla????o linear de Pearson ?? -0,82, isso nos informa que ?? uma correla????o negativa alta.




#Ajuste regressao linear do Preco em funcao da Quilometragem
regressao_linear <- lm(Preco ~ Quilometragem, data = dados)
summary(regressao_linear)

#Se tentarmos utilizar somente a Quilometragem para prever o valor do Pre??o, o quanto 
#da varia????o do Pre??o a vari??vel Quilometragem consegue explicar? Em outas palavras, 
#interprete o R2 da regress??o linear do Pre??o em fun????o da Quilometragem.

#O R2 ?? de 67,76%, ou seja, a vari??vel Quilometragem consegue explicar 67,76% da varia????o do Pre??o.



#Analise descritiva da variavel quilometragem
summary(dados$Quilometragem)

#Qual o valor do primeiro quartil e qual a sua interpreta????o CORRETA?
#O primeiro quartil ?? 554,7, isso nos diz que at?? 25% dos ve??culos alugados possuem quilometragem at?? 554,7.

#Qual o valor do terceiro quartil e qual a sua interpreta????o CORRETA?
#O terceiro quartil ?? 925,2, isso nos diz que at?? 75% dos ve??culos alugados possuem quilometragem at?? 925,2.



#Coeficiente de variacao
sd(dados$Quilometragem) / mean(dados$Quilometragem)

#Qual ?? o valor do coeficiente de varia????o e qual a sua interpreta????o CORRETA?
#O coeficiente de varia????o ?? 27,74%, ou seja, os valores da vari??vel quilometragem 
#variam em m??dia 27,74% em torno de sua m??dia. 
 

 
#Explore a correlacao entre o Dolar e o Preco
plot(y = dados$Preco,
     x = dados$Dolar,
     pch = 16)

cor(dados$Preco, dados$Dolar)

regressao_linear <- lm(Preco ~ Dolar, data = dados)
summary(regressao_linear)

#A correla????o entre as vari??veis d??lar e pre??o ?? positiva ou negativa?
#O gr??fico de dispers??o n??o apresenta nenhum padr??o entre as duas vari??veis, ou seja, 
#na medida em que o D??lar aumenta, o Pre??o n??o cresce nem decresce, ou seja, n??o h?? correla????o 
#entre as duas vari??veis.

#Obtenha o coeficiente de correla????o linear de Pearson entre o Dolar e o Pre??o. 
#Qual a interpreta????o CORRETA?
#O valor do coeficiente de correla????o linear de Pearson ?? -0,06, que indica aus??ncia de correla????o linear.


#0.9 para mais ou para menos indica uma correla????o muito forte.
#0.7 a 0.9 positivo ou negativo indica uma correla????o forte.
#0.5 a 0.7 positivo ou negativo indica uma correla????o moderada.
#0.3 a 0.5 positivo ou negativo indica uma correla????o fraca.
#0 a 0.3 positivo ou negativo indica uma correla????o desprez??vel.


#Se ajustarmos uma regress??o linear entre o Dolar e o Preco, para tentar prever o Pre??o baseado no 
#D??lar, seria poss??vel?

#N??o seria poss??vel, pois o p valor do coeficiente beta do Dolar ?? de 0,77 (77%), ou seja, independentemente
#do n??vel de signific??ncia adotado, a vari??vel Dolar n??o exerce influ??ncia significativa na vari??vel Preco, 
#portanto, n??o ?? poss??vel prever o Pre??o baseado no D??lar.
  
  


