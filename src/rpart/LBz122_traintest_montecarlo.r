rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(487649, 512899, 663263, 815953, 836839)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset que consiste
#  en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#  crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla)

  # genero el modelo
  # predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_ternaria ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(list(
    "testing" = dataset[fold == 2, .N],
    "testing_pos" = dataset[fold == 2 & clase_ternaria == "BAJA+2", .N],
    "envios" = dataset[fold == 2, sum(prediccion[, "BAJA+2"] > 0.025)],
    "aciertos" = dataset[
      fold == 2,
      sum(prediccion[, "BAJA+2"] > 0.025 & clase_ternaria == "BAJA+2")
    ],
    "ganancia_test" = ganancia_test_normalizada
  ))
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("/Users/leandro/Documents/Personal/Austral/Maestría_en_Ciencia_de_Datos/Laboratorio_1") # Establezco el Working Directory
# cargo los datos

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]


param_basicos <- list(
  "cp" = -1, # complejidad minima
  "minsplit" = 900, # minima cant de registros en un nodo para hacer el split
  "minbucket" = 350, # minima cantidad de registros en una hoja
  "maxdepth" = 5
) # profundidad máxima del arbol

# Un solo llamado, con la semilla 17
ArbolEstimarGanancia(17, param_basicos)


# la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
#  tantas veces como valores tenga el vector  PARAM$semillas
salidas <- mcmapply(ArbolEstimarGanancia,
  PARAM$semillas, # paso el vector de semillas
  MoreArgs = list(param_basicos), # aqui paso el segundo parametro
  SIMPLIFY = FALSE,
  mc.cores = 5  # en Windows este valor debe ser 1
)

# muestro la lista de las salidas en testing
#  para la particion realizada con cada semilla
salidas

# paso la lista a vector
tb_salida <- rbindlist(salidas)

tb_salida

# finalmente calculo la media (promedio)  de las ganancias
tb_salida[, mean(ganancia_test)]

# calculo todos los promedios
tb_salida[, lapply(.SD, mean)]

# desvio estandar Distribucion Binomial   sqrt( n * p * (1-p) )
