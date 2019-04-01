
#link kaggle:https://www.kaggle.com/pabloviana/ht-petfinder

datos<-read.csv("train.csv")

library(caret)
library(dummy)


# QUITAMOS AS VARIABLES QUE NO NOS AYUDAN A PREDECIR Y SON IRRELEVANTES EN 
# NUESTRO MODELO
datos$Name<-NULL
datos$RescuerID<-NULL
datos$Description<-NULL
datos$VideoAmt<-NULL
datos$PhotoAmt<-NULL
datos$PetID<-NULL

View(datos)

#dummy(datos$AdoptionSpeed)
datos2<-datos

# AQUI LO QUE HACEMOS ES REDUCIR LA VARIABLE DE "ADOPTION SPEED" DE 5 POSIBLES
# VALORES (0,1,2,3,4) A SOLAMENTE 2 (0,1). NOS BASASMO EN QUE SOLO PODRIAMOS
# PREDECIR SI UNA MASCOTA SERA ADOPTADA DESPUES DE UN MES O NO, FIJANDO COMO
# VALORES MAYORES QUE 3 COMO SI, Y VALORES MENORES QUE 3 COMO NO.
datos2$AdoptionSpeed<-ifelse(datos2$AdoptionSpeed>=3,1,0)
View(datos2)
#datos<cbind(datos,dummy(datos))


# DIVIDIMOS NUESTRO CONJUNTO DE DATOS EN 70% ENTRENAMIENTO Y 30% PRUEBAS
porciento = 0.7
set.seed(123)
muestra<-sample(1:nrow(datos2),porciento*nrow(datos2))
train<-datos2[muestra,]
test<-datos2[-muestra,]

View(train)

#si la mascota sera adoptada o no (>3)
# USAMOS UN MODELO DE REGRESEION LINEAL LOGISTICA PARA LA ADOPTION SPEED Y LA COMPARAMOS
# CONTRA TODAS LAS VARIABLES RESTANTES EN EL DATASET. 
# USAMOS FAMILY=BINOMIAL PARA ESPECIFICAR QUE ES UNU N MODELO DE REGRESEION LINEAL
# LOGISTICA
regLogmodel<-glm(train$AdoptionSpeed~.,data = train[,c(1:17)],family = binomial)

#AQUI SE PUEDEN VER LOS COEFICIENTES Y LOS INTERCEPTOS DE NUESTRO MODELO
regLogmodel

# USAMOS PREDICT PARA PODER PREDECIR EN NUESTRO CONJUNTO DE PRUEBA Y USAMOS EL 
# TIPO="RESPONSE" PARA QUE EL MODELO DE PREDICCION NOS DIERA UN VALOR DE RESPUESTA
# ENTRE 1 Y 0 Y ASI PODER COMPARARLOS.
prediccion<-predict(regLogmodel,test[,1:17],type = "response")
prediccion<-ifelse(prediccion>=0.5,1,0)

#TODAS LAS PREDICCIONES
prediccion
str(test$AdoptionSpeed)
str(prediccion)

# USAMOS UNA MATRIZ DE CONFUSION PARA VER LA EFECTIVIDAD DE NUESTRO  MODELO DE REGRESEION
# LINEAL LOGISTICA.
matrizC<-confusionMatrix(as.factor(test$AdoptionSpeed),as.factor(prediccion))
#SE PUEDE VER QUE NUESTRA ACCURACY NO ESTA TAN ALTA Y QUE SE EQUIVOCO 888 VECES CUANDO
# ERA 0 Y 941 CUANDO ERA 1. ADEMAS, NUESTRO VALOR KAPPA ESTA MUY BAJO
# TODO ESTO NOS INDICA QUE ESTE MODELO DE REGRESION LOGISTICA NO ES EL MEJOR PARA
# PREDECIR CON ESTE CONJUNTO DE DATOS. 

plot(prediccion)
# Confusion Matrix and Statistics


# Reference
# Prediction         0    1
#               0 1386  888
#               1  941 1283
# 
# Accuracy : 0.5934          
# 95% CI : (0.5789, 0.6078)
# No Information Rate : 0.5173          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.1864          
# Mcnemar's Test P-Value : 0.224           
# 
# Sensitivity : 0.5956          
# Specificity : 0.5910          
# Pos Pred Value : 0.6095          
# Neg Pred Value : 0.5769          
# Prevalence : 0.5173          
# Detection Rate : 0.3081          
# Detection Prevalence : 0.5056          
# Balanced Accuracy : 0.5933          
# 
# 'Positive' Class : 0 




# "Después de analizar la eficiencia de los algoritmos concluimos que el más rápido en terminar es el de la hoja de trabajo 5, 
# naive bayes, que termina aproximadamento dos segundos después de su ejecución y tiene 33% de precisión a la hora de predecir los datos
# , como contraparte tenemos el algoritmo de árbol de decisión y regresión lineal, los cuales cuentan con tiempos variables
# de ejecución, que siempre son considerablemente mayores a los de naive bayes y cuya precisión deja bastante que desear (29% y 15% respectivamente).
# Por ultimo tenemos el caso de Random Forest del cual pudimos aproximar un tiempo de 40 segundos en su ejecución; y resulta ser
# el algoritmo con un mayor índice de precisión con 38%. Ahora bien, el algoritmo de Regresion Logistica tiene un 59% de precision
# y un tiempo de ejecucion de menos de 5 segundos. Lo cual concluimos que aunque no tenga una precision muy alta, es el mejor de todos
# los algoritmos probados."