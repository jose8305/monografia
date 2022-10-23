#Librerias
# ==============================================================================
library(rpart)
library(rpart.plot)
library(ggplot2)
library(lattice)
library(caret)

#Obtener datos
# ==============================================================================
datos = read.csv2("datos.csv")

#Variables cualitativas 
# ==============================================================================
datos$HabitoPago = factor(datos$HabitoPago)
datos$Km_Score = factor(datos$Km_Score)
datos$Target = factor(datos$Target)

# muestra de entrenamiento con el 80% y test del 20% de todas las observaciones
# ==============================================================================
set.seed(2022)
n = round((length(datos$Target)*0.8),0)
datosFiltrados = datos[,c(1,7,12,15,16,18,19)]
IndiceMuestra = sample(1:nrow(datosFiltrados),size=n,replace=FALSE)
Train =  datosFiltrados[IndiceMuestra, ]
Test = datosFiltrados[-IndiceMuestra,]

#Modelo
# ==============================================================================
arbol = rpart(formula = Target ~ ., data = Train, method = "class")
rpart.plot(arbol)

#Importacia general de las variables
# ==============================================================================
V = caret::varImp(arbol)

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
prediccion = predict(arbol, newdata = Test, type = "class")

#Matriz de confusion
# ==============================================================================
confusionMatrix(prediccion, Test[["Target"]])

# AUC = Área bajo la curva ROC
# ==============================================================================
AUC = auc(actual = ifelse(Test$Target == "0", 0, 1),predicted = prediccion)
AUC

# Curva ROC
# ==============================================================================
tree.preds = predict(arbol, Test, type="prob")[, 2]

ROCRpre <- prediction(tree.preds, Test$Target)
ROCRper <- performance(ROCRpre, "tpr", "fpr")

plot(ROCRper,
     main = "Curva ROC",
     xlab="Tasa de falsos positivos", 
     ylab="Tasa de verdaderos positivos")
abline(a=0,b=1,col="blue",lty=2)
grid()
legend("bottomright",legend=paste(" AUC =",round(AUC,4)))