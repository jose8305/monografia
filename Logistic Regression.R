#Librerias
# ==============================================================================
library(ggplot2)
library(lattice)
library(caret)
library(Metrics)
library(ROCR)

#Obtener datos
# ==============================================================================
datos = read.csv2("datos.csv")

#Variables cualitativas 
# ==============================================================================
datos$HabitoPago = factor(datos$HabitoPago)
datos$Km_Score = factor(datos$Km_Score)
datos$Target = factor(datos$Target)

#Distribución de los datos
# ==============================================================================
prop.table(table(datos$Target))

# muestra de entrenamiento con el 80% y test del 20% de todas las observaciones
# ==============================================================================
set.seed(2022)
n = round((length(datos$Target)*0.8),0)
datosFiltrados = datos[,c(1,2,5,6,7,8,10,12,13,15,16,18,19)]
IndiceMuestra = sample(1:nrow(datosFiltrados),size=n,replace=FALSE)
Train =  datosFiltrados[IndiceMuestra, ]
Test = datosFiltrados[-IndiceMuestra,]

#Modelo
# ==============================================================================
modelo.log = glm(Target ~ ., family=binomial, data=Train)
summary(modelo.log)


#Importacia general de las variables
# ==============================================================================
V = caret::varImp(modelo.log)

ggplot2::ggplot(V, aes(x=reorder(rownames(V),Overall), y=Overall)) +
  geom_point( color="blue", size=4, alpha=0.6)+
  geom_segment( aes(x=rownames(V), xend=rownames(V), y=0, yend=Overall), 
                color='skyblue') +
  xlab('Variables')+
  ylab('Importancia General')+
  theme_light() +
  coord_flip()

# Predicción
# ==============================================================================
predictRlog = predict(modelo.log, type='response', newdata=Test)

#Matriz de confusion
# ==============================================================================
predictRlog_clasif = ifelse(predictRlog > 0.6, 1, 0) 

expected_value = factor(c(Test$Target))
predicted_value = factor(c(predictRlog_clasif))

matrizConfusion = confusionMatrix(data=predicted_value, reference = expected_value)
matrizConfusion

# AUC = Área bajo la curva ROC
# ==============================================================================
AUC = auc(actual = ifelse(Test$Target == "0", 0, 1),predicted = predictRlog)
AUC

# Curva ROC
# ==============================================================================
ROCRpre <- prediction(predictRlog, Test$Target)
ROCRper <- performance(ROCRpre, "tpr", "fpr")

plot(ROCRper,
     main = "Curva ROC",
     xlab="Tasa de falsos positivos", 
     ylab="Tasa de verdaderos positivos")
abline(a=0,b=1,col="blue",lty=2)
grid()
legend("bottomright",legend=paste(" AUC =",round(AUC,4)))