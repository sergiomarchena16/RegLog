datos<-read.csv("train.csv")

library(caret)
library(dummy)

datos$Name<-NULL
datos$RescuerID<-NULL
datos$Description<-NULL
datos$VideoAmt<-NULL
datos$PhotoAmt<-NULL
datos$PetID<-NULL

View(datos)

dummy(datos$AdoptionSpeed)
datos2<-datos
datos2$AdoptionSpeed<-ifelse(datos2$AdoptionSpeed>=3,1,0)
View(datos2)
datos<cbind(datos,dummy(datos))

porciento = 0.7
set.seed(123)
muestra<-sample(1:nrow(datos2),porciento*nrow(datos2))

train<-datos2[muestra,]
test<-datos2[-muestra,]

View(train)
#si la mascota sera adoptada o no (>3)
regLogmodel<-glm(train$AdoptionSpeed~.,data = train[,c(1:17)],family = binomial)
regLogmodel

prediccion<-predict(regLogmodel,test[,1:17],type = "response")
prediccion<-ifelse(prediccion>=0.5,1,0)
prediccion

str(test$AdoptionSpeed)
str(prediccion)
confusionMatrix(as.factor(test$AdoptionSpeed),as.factor(prediccion))
