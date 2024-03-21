# Definir la matriz P con cadenas de texto
P <- matrix(c("P1", "P2", "P3", "Pk", "P1", "P2", "P3", "Pk"), nrow = 2, byrow = TRUE)

# Función para convertir cadenas de texto a números
convertir_a_numero <- function(matriz) {
  # Crear una copia de la matriz
  matriz_numerica <- matriz
  
  # Convertir cada elemento de la matriz a número (si es posible)
  for (i in 1:nrow(matriz)) {
    for (j in 1:ncol(matriz)) {
      # Intentar convertir el elemento a número
      numero <- as.numeric(as.character(matriz[i, j]))
      
      # Verificar si la conversión fue exitosa
      if (!is.na(numero)) {
        matriz_numerica[i, j] <- numero
      }
    }
  }
  
  return(matriz_numerica)
}

# Convertir la matriz P a números
P_numerica <- convertir_a_numero(P)

# Mostrar la matriz P numérica
print(P_numerica)



# Función para calcular la matriz resultante según la operación dada
calcular_matriz_resultante <- function(P, k) {
  # Crear una matriz vacía para almacenar el resultado
  resultado <- matrix(0, nrow = k, ncol = k)
  
  # Calcular los elementos de la matriz resultado
  for (i in 1:k) {
    for (j in 1:k) {
      # Calcular el elemento en la posición (i, j)
      resultado[i, j] <- sum(P[i, ] * P[, j])  # Realiza la operación dada para cada elemento
    }
  }
  
  return(resultado)
}

# Definir la matriz P con números
P <- matrix(1:12, nrow = 3)  # Cambia los números y el número de filas según sea necesario
k <- nrow(P)

# Calcular la matriz resultante
matriz_resultante <- calcular_matriz_resultante(P, k)

# Mostrar la matriz resultante
print(matriz_resultante)


#Demostar que una una matriz p^2 = p
# Instalar y cargar el paquete expm si no está instalado
if (!requireNamespace("expm", quietly = TRUE)) {
  install.packages("expm")
}
library(expm)

# Definir una matriz aleatoria P de 5x5
set.seed(123)  # Fijar la semilla para reproducibilidad
P <- matrix(rnorm(25), nrow = 5)

# Elevar P a diferentes potencias y verificar si es igual a P
n_max <- 10  # Cambia el valor máximo de n según sea necesario

for (n in 1:n_max) {
  P_n <- expm::`%^%`(P, n)
  if (all.equal(P_n, P)) {
    cat("Se ha encontrado que P elevado a la potencia", n, "es igual a P:\n\n")
    print(P_n)
    break
  } else {
    cat("P elevado a la potencia", n, "NO es igual a P.\n")
  }
}

