# Corrida general del Workflow de Guantes Blancos
# con AUCROC en lugar de Ganancia
# para aprender lo conceptual, sin ensuciarse las manos

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("rlang")
require("yaml")
require("data.table")
require("ParamHelpers")

# creo environment global
envg <- env()

envg$EXPENV <- list()
envg$EXPENV$exp_dir <- "~/buckets/b1/exp/"
envg$EXPENV$wf_dir <- "~/buckets/b1/flow/"
envg$EXPENV$wf_dir_local <- "~/flow/"
envg$EXPENV$repo_dir <- "~/labo2024v1/"
envg$EXPENV$datasets_dir <- "~/buckets/b1/datasets/"
envg$EXPENV$arch_sem <- "mis_semillas.txt"

# default
envg$EXPENV$gcloud$RAM <- 64
envg$EXPENV$gcloud$cCPU <- 8

#------------------------------------------------------------------------------
# Error catching

options(error = function() {
  traceback(20)
  options(error = NULL)
  
  cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
    file = "z-Rabort.txt",
    append = TRUE 
    )

  stop("exiting after script error")
})
#------------------------------------------------------------------------------
# inicializaciones varias

dir.create( envg$EXPENV$wf_dir, showWarnings = FALSE)
dir.create( envg$EXPENV$wf_dir, showWarnings = FALSE)
dir.create( envg$EXPENV$wf_dir_local, showWarnings = FALSE)
setwd( envg$EXPENV$wf_dir_local )

#------------------------------------------------------------------------------
# cargo la  "libreria" de los experimentos

exp_lib <- paste0( envg$EXPENV$repo_dir,"/src/lib/z590_exp_lib_01.r")
source( exp_lib )

#------------------------------------------------------------------------------

# pmyexp <- "DT0002"
# parch <- "competencia_2024.csv.gz"
# pserver <- "local"

DT_incorporar_dataset_default <- function( pmyexp, parch, pserver="local")
{
  if( -1 == (param_local <- exp_init_datos( pmyexp, parch, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z511_DT_incorporar_dataset.r"

  param_local$primarykey <- c("numero_de_cliente", "foto_mes" )
  param_local$entity_id <- c("numero_de_cliente" )
  param_local$periodo <- c("foto_mes" )
  param_local$clase <- c("clase_ternaria" )

  return( exp_correr_script( param_local ) ) # linea fija}
}
#------------------------------------------------------------------------------

# pmyexp <- "CA0001"
# pinputexps <- "DT0002"
# pserver <- "local"

CA_catastrophe_default <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z521_CA_reparar_dataset.r"

  # Opciones MachineLearning EstadisticaClasica Ninguno
  param_local$metodo <- "MachineLearning" # MachineLearning EstadisticaClasica Ninguno

  return( exp_correr_script( param_local ) ) # linea fija}
}
#------------------------------------------------------------------------------
# Data Drifting de Guantes Blancos


# pmyexp <- "DR0001"
# pinputexps <- "CA0001"
# pserver <- "local"

DR_drifting_guantesblancos <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z531_DR_corregir_drifting.r"

  # No me engraso las manos con Feature Engineering manual
  param_local$variables_intrames <- TRUE
  # valores posibles
  #  "ninguno", "rank_simple", "rank_cero_fijo", "deflacion", "estandarizar"
  param_local$metodo <- "rank_cero_fijo"

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------

# pmyexp <- "FE0001"
# pinputexps <- "DR0001"
# pserver <- "local"

FE_historia_guantesblancos <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z541_FE_historia.r"

  param_local$lag1 <- TRUE
  param_local$lag2 <- FALSE # no me engraso con los lags de orden 2
  param_local$lag3 <- FALSE # no me engraso con los lags de orden 3

  # no me engraso las manos con las tendencias
  param_local$Tendencias1$run <- TRUE  # FALSE, no corre nada de lo que sigue
  param_local$Tendencias1$ventana <- 6
  param_local$Tendencias1$tendencia <- TRUE
  param_local$Tendencias1$minimo <- FALSE
  param_local$Tendencias1$maximo <- FALSE
  param_local$Tendencias1$promedio <- FALSE
  param_local$Tendencias1$ratioavg <- FALSE
  param_local$Tendencias1$ratiomax <- FALSE

  # no me engraso las manos con las tendencias de segundo orden
  param_local$Tendencias2$run <- FALSE
  param_local$Tendencias2$ventana <- 6
  param_local$Tendencias2$tendencia <- FALSE
  param_local$Tendencias2$minimo <- FALSE
  param_local$Tendencias2$maximo <- FALSE
  param_local$Tendencias2$promedio <- FALSE
  param_local$Tendencias2$ratioavg <- FALSE
  param_local$Tendencias2$ratiomax <- FALSE


  # No me engraso las manos con las variables nuevas agregadas por un RF
  # esta parte demora mucho tiempo en correr, y estoy en modo manos_limpias
  param_local$RandomForest$run <- TRUE
  param_local$RandomForest$num.trees <- 20
  param_local$RandomForest$max.depth <- 4
  param_local$RandomForest$min.node.size <- 1000
  param_local$RandomForest$mtry <- 40

  # no me engraso las manos con los Canaritos Asesinos
  # varia de 0.0 a 2.0, si es 0.0 NO se activan
  param_local$CanaritosAsesinos$ratio <- 0.0
  # desvios estandar de la media, para el cutoff
  param_local$CanaritosAsesinos$desvios <- 4.0

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Training Strategy de Guantes Blancos
#   entreno en solo tres meses   ( mas guantes blancos no se puede )
#   y solo incluyo en el dataset al 5% de los CONTINUA

TS_strategy_guantesblancos_202109 <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z551_TS_training_strategy.r"


  param_local$future <- c(202109)
  param_local$final_train <- c(202107, 202106, 202105, 202104, 202103, 202102, 202101, 202012, 202011)


  param_local$train$training <- c(202105, 202104, 202103, 202102, 202101, 202012, 202011, 202010, 202009)
  param_local$train$validation <- c(202106)
  param_local$train$testing <- c(202107)

  # Atencion  0.1  de  undersampling de la clase mayoritaria,  los CONTINUA
  # 1.0 significa NO undersampling ,  0.1  es quedarse con el 10% de los CONTINUA
  param_local$train$undersampling <- 0.2

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Training Strategy de Guantes Blancos
#   entreno en solo tres meses ( mas guantes blancos no se puede )
#   y solo incluyo en el dataset al 5% de los CONTINUA

TS_strategy_guantesblancos_202107 <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z551_TS_training_strategy.r"


  param_local$future <- c(202107)
  param_local$final_train <- c(202105, 202104, 202103, 202102, 202101, 202012, 202011, 202010, 202009)


  param_local$train$training <- c(202103, 202102, 202101, 202012, 202011, 202010, 202009, 202008, 202007)
  param_local$train$validation <- c(202104)
  param_local$train$testing <- c(202105)

  # Atencion  0.1  de  undersampling de la clase mayoritaria,  los CONTINUA
  # 1.0 significa NO undersampling ,  0.1  es quedarse con el 10% de los CONTINUA
  param_local$train$undersampling <- 0.2

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------

# Hyperparamteter Tuning de Guantes Blancos
#  donde la Bayuesian Optimization solo considera 4 hiperparámetros
#  y apenas se hacen 15 iteraciones inteligentes

HT_tuning_guantesblancos <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z561_HT_lightgbm.r"

  # En caso que se haga cross validation, se usa esta cantidad de folds
  param_local$lgb_crossvalidation_folds <- 5

  # Hiperparametros  del LightGBM
  #  los que tienen un solo valor son los que van fijos
  #  los que tienen un vector,  son los que participan de la Bayesian Optimization
  
  param_local$lgb_param <- list(
    boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE, # para reducir warnings
    verbosity = -100,
    max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
    min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
    min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
    lambda_l1 = 0.0, # lambda_l1 >= 0.0
    lambda_l2 = 0.0, # lambda_l2 >= 0.0
    max_bin = 31L, # lo debo dejar fijo, no participa de la BO
    num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds

    bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
    pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
    neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
    is_unbalance = FALSE, #
    scale_pos_weight = 1.0, # scale_pos_weight > 0.0

    drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
    max_drop = 50, # <=0 means no limit
    skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

    extra_trees = FALSE,
    # White Gloves Bayesian Optimization, with a happy narrow exploration
    learning_rate = c( 0.02, 0.5 ),
    feature_fraction = c( 0.5, 0.9 ),
    num_leaves = c( 8L, 2048L,  "integer" ),
    min_data_in_leaf = c( 100L, 2000L, "integer" )
  )


  # una Beyesian de Guantes Blancos, solo hace 15 iteraciones
  param_local$bo_iteraciones <- 50 # iteraciones de la Optimizacion Bayesiana

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Hyperparamteter Tuning de Guantes Blancos
#  donde la Bayuesian Optimization solo considera 4 hiperparámetros
#  y apenas se hacen 15 iteraciones inteligentes

HT_tuning_AUCROC <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z562_HT_lightgbm_AUCROC.r"

  # En caso que se haga cross validation, se usa esta cantidad de folds
  param_local$lgb_crossvalidation_folds <- 5

  # Hiperparametros  del LightGBM
  #  los que tienen un solo valor son los que van fijos
  #  los que tienen un vector,  son los que participan de la Bayesian Optimization
  
  param_local$lgb_param <- list(
    boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE, # para reducir warnings
    verbosity = -100,
    max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
    min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
    min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
    lambda_l1 = 0.0, # lambda_l1 >= 0.0
    lambda_l2 = 0.0, # lambda_l2 >= 0.0
    max_bin = 31L, # lo debo dejar fijo, no participa de la BO
    num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds

    bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
    pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
    neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
    is_unbalance = FALSE, #
    scale_pos_weight = 1.0, # scale_pos_weight > 0.0

    drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
    max_drop = 50, # <=0 means no limit
    skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

    extra_trees = FALSE,
    # White Gloves Bayesian Optimization, with a happy narrow exploration
    learning_rate = c( 0.02, 0.5 ),
    feature_fraction = c( 0.5, 0.9 ),
    num_leaves = c( 8L, 2048L,  "integer" ),
    min_data_in_leaf = c( 100L, 2000L, "integer" )
  )


  # una Beyesian de Guantes Blancos, solo hace 15 iteraciones
  param_local$bo_iteraciones <- 50 # iteraciones de la Optimizacion Bayesiana

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------

# proceso ZZ_final  de Guantes Blancos

ZZ_final_guantesblancos <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z571_ZZ_final1.r"

  # Que modelos quiero, segun su posicion en el ranking e la Bayesian Optimizacion, ordenado por ganancia descendente
  param_local$modelos_rank <- c(1)

  param_local$kaggle$envios_desde <-  9500L
  param_local$kaggle$envios_hasta <- 11500L
  param_local$kaggle$envios_salto <-   500L

  # para el caso que deba graficar
  param_local$graficar$envios_desde <-  8000L
  param_local$graficar$envios_hasta <- 20000L
  param_local$graficar$ventana_suavizado <- 2001L

  # Una corrida de Guantes Blancos solo usa 5 semillas
  param_local$qsemillas <- 5

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# A partir de ahora comienza la seccion de Workflows Completos
#------------------------------------------------------------------------------
# Este es el  Workflow de Guantes Blancos
# Que predice 202109
# y ya genera archivos para Kaggle

corrida_guantesblancos_202109_AUC <- function( pnombrewf, pvirgen=FALSE )
{
  if( -1 == exp_wf_init( pnombrewf, pvirgen) ) return(0) # linea fija

  DT_incorporar_dataset_default( "DT0901BLF", "competencia_2024.csv.gz")
  CA_catastrophe_default( "CA0901BLF", "DT0901BLF" )

  DR_drifting_guantesblancos( "DR0901BLF", "CA0901BLF" )
  FE_historia_guantesblancos( "FE0901BLF", "DR0901BLF" )

  TS_strategy_guantesblancos_202109( "TS0901BLF", "FE0901BLF" )

  HT_tuning_AUCROC( "HT0901BLF", "TS0901BLF" )

  # El ZZ depente de HT y TS
  ZZ_final_guantesblancos( "ZZ0901BLF", c("HT0901BLF","TS0901BLF") )


  exp_wf_end( pnombrewf, pvirgen ) # linea fija
}
#------------------------------------------------------------------------------
# Este es el  Workflow de Guantes Blancos
# Que predice 202107
# genera completas curvas de ganancia
#   NO genera archivos para Kaggle
# por favor notal como este script parte de FE0901

corrida_guantesblancos_202107_AUC <- function( pnombrewf, pvirgen=FALSE )
{
  if( -1 == exp_wf_init( pnombrewf, pvirgen) ) return(0) # linea fija

  # Ya tengo corrido FE0901 y parto de alli
  TS_strategy_guantesblancos_202107( "TS0902BLF", "FE0901BLF" )

  HT_tuning_AUCROC( "HT0902BLF", "TS0902BLF" )

  # El ZZ depente de HT y TS
  ZZ_final_guantesblancos( "ZZ0902BLF", c("HT0902BLF", "TS0902BLF") )


  exp_wf_end( pnombrewf, pvirgen ) # linea fija
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa


# Hago primero esta corrida que me genera los experimentos
corrida_guantesblancos_202109_AUC( "gb01_AUCBLF" )


# Luego partiendo de  FE0001

corrida_guantesblancos_202107_AUC( "gb02_AUCBLF" )

 