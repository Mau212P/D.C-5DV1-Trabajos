# Definir la matriz de transición P y la distribución inicial a
P <- matrix(c(0, 0, 0, 0, 1,
              0, 8/13, 3/13, 1/13, 1/13,
              1/16, 3/16, 3/8, 1/4, 1/8,
              0, 1/11, 4/11, 5/11, 1/11,
              0, 1/8, 1/2, 1/8, 1/4), 
            nrow = 5, byrow = TRUE)
colnames(P)<-c("80", "135", "139","445","No attack")
row.names(P)<-c("80", "135", "139","445","No attack")
P
a <- c(0, 0, 0, 0, 1)

# Calcular la distribución después de 2 semanas
a_2semanas <- a %*% (P %*% P)
a_2semanas 

# Responder a la pregunta (a)
puerto_menos_probable <- which.min(a_2semanas[1:4])
puerto_mas_probable <- which.max(a_2semanas[1:4])

print(paste("Puerto menos probable después de 2 semanas:", row.names(P)[puerto_menos_probable]))
print(paste("Puerto más probable después de 2 semanas:", row.names(P)[puerto_mas_probable]))

# Encontrar la distribución a largo plazo de los puertos atacados
eigen_resultado <- eigen(t(P))
distribucion_estacionaria <- eigen_resultado$vectors[, which.max(abs(eigen_resultado$valores - 1))]
distribucion_estacionaria <- distribucion_estacionaria / sum(distribucion_estacionaria)

print("Distribución a largo plazo de los puertos atacados:")
print(distribucion_estacionaria)



# 2

# Definir la matriz de transición
P <- matrix(c(0, 0, 0, 0, 1,
              0, 8/13, 3/13, 1/13, 1/3,
              1/16, 3/16, 3/8, 1/4, 1/8,
              0, 1/11, 4/11, 5/8, 1/11,
              0, 1/8, 1/2, 1/8, 1/4), byrow = TRUE, nrow = 5)

# Definir la distribución inicial
a <- c(0, 0, 0, 0, 1)

# Calcular la distribución después de 2 semanas
a_2 <- a %*% (P %^% P)

# Imprimir los resultados
cat("Distribución después de 2 semanas:\n")
print(a_2)

# Calcular la distribución estacionaria
pi <- a
old_pi <- rep(0, length(pi))

while(!all(abs(pi - old_pi) < 1e-10)) {
  old_pi <- pi
  pi <- pi %*% P
}

# Imprimir la distribución estacionaria
cat("\nDistribución estacionaria de los puertos atacados:\n")
print(pi[1:4])





#3
# Definir la matriz de transición
P <- matrix(c(0, 0, 0, 0, 1,
              0, 8/13, 3/13, 1/13, 1/3,
              1/16, 3/16, 3/8, 1/4, 1/8,
              0, 1/11, 4/11, 5/8, 1/11,
              0, 1/8, 1/2, 1/8, 1/4), 
            nrow = 5, byrow = TRUE)

# Definir la distribución inicial
a <- c(0, 0, 0, 0, 1)

# Multiplicar la matriz de transición por sí misma dos veces para calcular el estado después de 2 semanas
estado_despues_2_semanas <- a %*% P %*% P

# Imprimir los resultados para (a)
cat("Puertos menos atacados después de 2 semanas:", which.min(estado_despues_2_semanas), "\n")
cat("Puertos más probables después de 2 semanas:", which.max(estado_despues_2_semanas), "\n")

# Calcular la distribución a largo plazo multiplicando la distribución inicial por la matriz de transición elevada a una potencia grande
distribucion_largo_plazo <- a %*% P
for (i in 1:1000) {
  distribucion_largo_plazo <- distribucion_largo_plazo %*% P
}

# Imprimir los resultados para (b)
cat("Distribución a largo plazo de los puertos atacados:\n")
print(distribucion_largo_plazo)

