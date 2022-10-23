#Librerias
# ==============================================================================
library(kknn)
library(caret)
library(Metrics)
library(ROCR)
#Función para normalizar los datos
# ==============================================================================
normalizar  <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

#Obtener datos
# ==============================================================================
datos = read.csv2("datos.csv")

datos = as.data.frame(lapply(datos[,c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19)], normalizar))

# muestra de entrenamiento con el 80% y test del 20% de todas las observaciones
# ==============================================================================
set.seed(2022)
n = round((length(datos$Target)*0.8),0)
datosFiltrados = datos
IndiceMuestra = sample(1:nrow(datosFiltrados),size=n,replace=FALSE)
Train =  datosFiltrados[IndiceMuestra, ]
Test = datosFiltrados[-IndiceMuestra,]
Train_labels = Train$Target

#Modelo
# ==============================================================================
modeloKnn = train.kknn(Target ~ ., data = Train, kmax = 13,
                       kernel ="triangular",
                       distance = 1
)

# Predicción
# ==============================================================================
Predictknn = predict(modeloKnn, Test[,-16])

#Matriz de confusion
# ==============================================================================
predKnn_clasif = ifelse(Predictknn > 0.65, 1, 0)

expected_value = factor(c(Test$Target))
predicted_value = factor(c(predKnn_clasif))

matrizConfusion = confusionMatrix(data=predicted_value, reference = expected_value)
matrizConfusion

# AUC = Área bajo la curva ROC
# ==============================================================================
AUC = auc(actual = ifelse(Test$Target == "1", 1, 0),predicted = Predictknn)
AUC

# Curva ROC
# ==============================================================================
ROCRpre <- prediction(Predictknn, Test$Target)
ROCRper <- performance(ROCRpre, "tpr", "fpr")

plot(ROCRper,
     main = "Curva ROC",
     xlab="Tasa de falsos positivos", 
     ylab="Tasa de verdaderos positivos")
abline(a=0,b=1,col="blue",lty=2)
grid()
legend("bottomright",legend=paste(" AUC =",round(AUC,4)))