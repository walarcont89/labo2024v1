# Este script almacena definitivamente sus cinco semillas
# en el bucket, de forma que NO deba cargarlas en cada script

require( "data.table" )

# reemplazar aqui por SUS semillas 
mis_semillas <- c(299909, 299977, 300007, 300017, 300089)

tabla_semillas <- as.data.table(list( semilla = mis_semillas ))

fwrite( tabla_semillas,
    file = "~/buckets/b1/datasets/mis_semillas.txt",
    sep = "\t"
)
