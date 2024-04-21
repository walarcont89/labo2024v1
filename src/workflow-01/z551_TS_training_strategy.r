# Experimentos Colaborativos Default
# Workflow  Training Strategy

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")

#------------------------------------------------------------------------------

options(error = function() {
  traceback(20)
  options(error = NULL)
  
  t <- format(Sys.time(), "%Y%m%d %H%M%S")
  cat( t, "\n",
    file = "z-Rabort.txt",
    append = TRUE
  )

  cat( t, "\n",
    file = "z-Rabort-hist.txt",
    append = TRUE
  )

  stop("exiting after script error")
})
#------------------------------------------------------------------------------

# Parametros del script
PARAM <- read_yaml( "parametros.yml" )

OUTPUT <- list()

# si training se establece identico a validation,
#  entonces aguas abajo se hara Cross-Validation
# si training = validation = testing   tambien se hara  Cross-Validation
#------------------------------------------------------------------------------

GrabarOutput <- function() {
  write_yaml(OUTPUT, file = "output.yml") # grabo OUTPUT
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
OUTPUT$PARAM <- PARAM
OUTPUT$time$start <- format(Sys.time(), "%Y%m%d %H%M%S")


GrabarOutput()

PARAM$train$semilla <- PARAM$semilla

  
# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
PARAM$dataset <- paste0( "./", PARAM$input, "/dataset.csv.gz" )
PARAM$dataset_metadata <- read_yaml( paste0( "./", PARAM$input, "/dataset_metadata.yml" ) )

dataset <- fread(PARAM$dataset)

setorder(dataset, foto_mes, numero_de_cliente)


# grabo los datos del futuro
fwrite(dataset[foto_mes %in% PARAM$future, ],
  file = "dataset_future.csv.gz",
  logical01 = TRUE,
  sep = ","
)

# grabo los datos donde voy a entrenar los Final Models
fwrite(dataset[foto_mes %in% PARAM$final_train, ],
  file = "dataset_train_final.csv.gz",
  logical01 = TRUE,
  sep = ","
)



# grabo los datos donde voy a hacer la optimizacion de hiperparametros
set.seed(PARAM$train$semilla, kind = "L'Ecuyer-CMRG")
dataset[
  foto_mes %in% PARAM$train$training,
  azar := runif(nrow(dataset[foto_mes %in% PARAM$train$training]))
]

dataset[, fold_train := 0L]
dataset[
  foto_mes %in% PARAM$train$training &
    (azar <= PARAM$train$undersampling |
      clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  fold_train := 1L
]

dataset[, fold_validate := 0L]
dataset[foto_mes %in% PARAM$train$validation, fold_validate := 1L]

dataset[, fold_test := 0L]
dataset[foto_mes %in% PARAM$train$testing, fold_test := 1L]


fwrite(dataset[fold_train + fold_validate + fold_test >= 1, ],
  file = "dataset_training.csv.gz",
  logical01 = TRUE,
  sep = ","
)


#------------------------------------------------------------------------------

# copia la metadata sin modificar
write_yaml( PARAM$dataset_metadata, 
  file="dataset_metadata.yml" )


#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(
    dataset[fold_train + fold_validate + fold_test >= 1, ],
    function(x) {
      sum(is.na(x))
    }
  ),
  "ceros" = sapply(
    dataset[fold_train + fold_validate + fold_test >= 1, ],
    function(x) {
      sum(x == 0, na.rm = TRUE)
    }
  )
))

fwrite(tb_campos,
  file = "dataset_training.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
OUTPUT$dataset_train$ncol <- ncol(dataset[fold_train > 0, ])
OUTPUT$dataset_train$nrow <- nrow(dataset[fold_train > 0, ])
OUTPUT$dataset_train$periodos <- dataset[fold_train > 0, length(unique(foto_mes))]

OUTPUT$dataset_validate$ncol <- ncol(dataset[fold_validate > 0, ])
OUTPUT$dataset_validate$nrow <- nrow(dataset[fold_validate > 0, ])
OUTPUT$dataset_validate$periodos <- dataset[fold_validate > 0, length(unique(foto_mes))]

OUTPUT$dataset_test$ncol <- ncol(dataset[fold_test > 0, ])
OUTPUT$dataset_test$nrow <- nrow(dataset[fold_test > 0, ])
OUTPUT$dataset_test$periodos <- dataset[fold_test > 0, length(unique(foto_mes))]

OUTPUT$dataset_future$ncol <- ncol(dataset[foto_mes %in% PARAM$future, ])
OUTPUT$dataset_future$nrow <- nrow(dataset[foto_mes %in% PARAM$future, ])
OUTPUT$dataset_future$periodos <- dataset[foto_mes %in% PARAM$future, length(unique(foto_mes))]

OUTPUT$dataset_finaltrain$ncol <- ncol(dataset[foto_mes %in% PARAM$final_train, ])
OUTPUT$dataset_finaltrain$nrow <- nrow(dataset[foto_mes %in% PARAM$final_train, ])
OUTPUT$dataset_finaltrain$periodos <- dataset[foto_mes %in% PARAM$final_train, length(unique(foto_mes))]

OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

# dejo la marca final
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
  file = "z-Rend.txt",
  append = TRUE
)
