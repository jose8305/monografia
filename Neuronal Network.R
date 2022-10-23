#Librerias
# ==============================================================================
library(h2o)
library(lattice)
library(caret)

#Obtener datos
# ==============================================================================
datos = read.csv2("datos.csv")

#Variables cualitativas 
# ==============================================================================
datos$Target = factor(datos$Target)
datos$Tercero = factor(datos$Tercero)
datos$CargaNacional = factor(datos$CargaNacional)
datos$VariosVehiculos = factor(datos$VariosVehiculos)
datos$Km_Grupo = factor(datos$Km_Grupo)
datos$Habito = factor(datos$Habito)

# Inicialización del cluster
# ==============================================================================
h2o.init(
  nthreads = 2,
  max_mem_size = "4g"
)

# Se eliminan los datos del cluster por si ya había sido iniciado.
# ==============================================================================
h2o.removeAll()
h2o.no_progress()

#Se transfieren los datos al cluster de H2O.
# ==============================================================================
datos_h2o   = as.h2o(datos[,c(3,5,6,7,8,9,10,11,12,13,14,15,16,18,19)])
particiones = h2o.splitFrame(data = datos_h2o, ratios = c(0.6, 0.2), seed = 2022)
datos_train = h2o.assign(data = particiones[[1]], key = "datos_train")
datos_validation  = h2o.assign(data = particiones[[2]], key = "datos_validacion")
datos_test  = h2o.assign(data = particiones[[3]], key = "datos_test")

# Modelo
# ==============================================================================
modelo = h2o.deeplearning(
  x               = c('Habito','Tercero','CargaNacional','VariosVehiculos','Antiguedad',
                      'Arpu','Km_Recorridos','NroRevisiones','Uso','Balance','AtencionAlarmas',
                      'TiempoActivo','Facturado', 'Grupo'),
  y              = "Target",
  distribution    = "bernoulli",
  training_frame  = datos_train,
  validation_frame= datos_validation,
  activation      = "Rectifier",
  adaptive_rate   = FALSE,
  hidden          = c(12,12),
  epochs          = 1040,
  rate            = 0.0001,
  stopping_rounds = 5,
  seed            = 2022,
  stopping_metric = 'logloss',
  model_id        = "modelo_final"
)

# Predicción
# ==============================================================================
predicciones = h2o.predict(object  = modelo,newdata = datos_test)

#Matriz de confusion
# ==============================================================================
y_pred = as.vector(ifelse(predicciones$predict == '0', 0, 1))
y_test_set = as.vector(ifelse(datos_test$Target == '0', 0, 1))

expected_value <- factor(y_test_set)
predicted_value <- factor(y_pred)

matrizConfusion = confusionMatrix(data=predicted_value, reference = expected_value)
matrizConfusion

# AUC = Área bajo la curva ROC
# ==============================================================================
perf = h2o.performance(modelo, datos_test)
AUC = h2o.auc(perf)
AUC

# Curva ROC
# ==============================================================================
plot(perf,
     main = "Curva ROC",
     xlab="Tasa de falsos positivos", 
     ylab="Tasa de verdaderos positivos")
abline(a=0,b=1,col="blue",lty=2)
grid()
legend("bottomright",legend=paste(" AUC =",round(AUC,4)))

# Se apaga el cluster H2O
# ==============================================================================
h2o.shutdown(prompt = FALSE)

