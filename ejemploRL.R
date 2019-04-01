library(caret)
library(dummy)
install.packages("dummy")
dato <- iris
dato<-cbind(datos,dummy(datos))
View(dato)

porciento = 0.7
set.seed(123)
muestra<-sample(1:nrow(datos),porciento*nrow(datos))

train<-datos[muestra,]
test<-datos[-muestra,]

#si una flor es versicolor o no
regLogmodel<-glm(train$Species_versicolor~.,data = train[,c(1:4,7)],family = binomial)
regLogmodel

prediccion<-predict(regLogmodel,test[,1:4],type = "response")
prediccion<-ifelse(prediccion>=0.5,1,0)
prediccion

confusionMatrix(test$Species_versicolor,as.factor(prediccion))
