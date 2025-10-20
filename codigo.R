## Trabajo práctico R
## Denisse Blasco y Nicolás Castelao

set.seed(17101945)

############################################
#                                          #
#     Ejercicio 1: Análisis de datos       #
#                                          #
############################################

library(tidyverse)

# Del sitio web https://www.football-data.co.uk/englandm.php extraimos datos sobre la Premier League de fútbol.
# Nuestras preguntas de interés son:
#     1. ¿Cuál es el efecto de la localía en el resultado?
#     2. ¿Qué tan comúnes son las remontadas?
#     3. ¿Cuáles son los árbitros con mayor severidad disciplinaria? ¿Hay relación con la localidad?
# Para esto, utilizamos datasets de partidos de fútbol de la Premier League.
# Restringimos la información desde la temporada 2010-2011 a 2023-2024, teniendo en cuenta cambios en el desarrollo
# del deporte y lo incompleto de los datasets futuros.
# Para la pregunta acerca de la localía, separadamente realizamos un análisis sin tener en cuenta los años de pandemia, donde no hubo público local.

# Tenemos un dataset por cada temporada. Queremos juntarlos. 
# El dataset t_i corresponde a la temporada del año i-i+1. Es decir, t_2023 tiene la temporada 2023-2024
t_2010 <- read.csv("C:/Users/gusta/Downloads/2010.csv", header = TRUE)
t_2011 <- read.csv("C:/Users/gusta/Downloads/2011.csv", header = TRUE)
t_2012 <- read.csv("C:/Users/gusta/Downloads/2012.csv", header = TRUE)
t_2013 <- read.csv("C:/Users/gusta/Downloads/2013.csv", header = TRUE)
t_2014 <- read.csv("C:/Users/gusta/Downloads/2014.csv", header = TRUE)
t_2015 <- read.csv("C:/Users/gusta/Downloads/2015.csv", header = TRUE)
t_2016 <- read.csv("C:/Users/gusta/Downloads/2016.csv", header = TRUE)
t_2017 <- read.csv("C:/Users/gusta/Downloads/2017.csv", header = TRUE)
t_2018 <- read.csv("C:/Users/gusta/Downloads/2018.csv", header = TRUE)
t_2019 <- read.csv("C:/Users/gusta/Downloads/2019.csv", header = TRUE)
t_2020 <- read.csv("C:/Users/gusta/Downloads/2020.csv", header = TRUE)
t_2021 <- read.csv("C:/Users/gusta/Downloads/2021.csv", header = TRUE)
t_2022 <- read.csv("C:/Users/gusta/Downloads/2022.csv", header = TRUE)
t_2023 <- read.csv("C:/Users/gusta/Downloads/2023.csv", header = TRUE)


# Nos quedaremos con las variables de interés.

vars_interes <- c(         
  "FTR",                   # resultado final
  "HTR",                   # resultado al entretiempo.
  "Referee","HY","AY","HR","AR" # disciplina. No ajustamos por cantidad de faltas pues suponemos que la elección de árbitros y la intensidad del juego no están correlacionadas.
)

df_2010 <- t_2010[, vars_interes]
df_2011 <- t_2011[, vars_interes]
df_2012 <- t_2012[, vars_interes]
df_2013 <- t_2013[, vars_interes]
df_2014 <- t_2014[, vars_interes]
df_2015 <- t_2015[, vars_interes]
df_2016 <- t_2016[, vars_interes]
df_2017 <- t_2017[, vars_interes]
df_2018 <- t_2018[, vars_interes]
df_2019 <- t_2019[, vars_interes]
df_2020 <- t_2020[, vars_interes]
df_2021 <- t_2021[, vars_interes]
df_2022 <- t_2022[, vars_interes]
df_2023 <- t_2023[, vars_interes]

# Los uno
data <- rbind(
  df_2010,
  df_2011,
  df_2012,
  df_2013,
  df_2014,
  df_2015,
  df_2016,
  df_2017,
  df_2018,
  df_2019,
  df_2020,
  df_2021,
  df_2022,
  df_2023
)

# Le saco la única fila con missing data
data <- data[-1901, ]
sum(is.na(data))


# Pregunta 1: ¿Cuál es el efecto de la localía en el resultado?
# Grafico frecuencia de cada resultado

barplot(table(data$FTR),
        main = "Frecuencia de resultados ",
        xlab = "Resultado final", ylab = "Frecuencia",
        col = c(rgb(0.7, 0.85, 1),  rgb(0.4, 0.6, 0.9), rgb(0.1, 0.3, 0.7) ),
        border = "black")

cat("Victorias locales: ", sum(data$FTR == "H"), "\n")
cat("Empates:         ", sum(data$FTR == "D"), "\n")
cat("Victorias visitantes:", sum(data$FTR == "A"), "\n")

# Por lo tanto, se observa que, dado que hay igual cantidad de partidos de visitante que de local (cada partido tiene un visitante y un local en la Premier),
# entonces las probabilidades pueden calcularse de la siguiente manera.
# P(ganar | soy local) = #victorias como local \ #partidos totales
# P(ganar | soy visitante) = #victorias como visitante \ #partidos totales
# P(empatar | soy local) = #empates como local \ #partidos totales = P(empatar | siendo visitante) = P(empatar)
# Calculemos todo esto.

p_ganar_local = sum(data$FTR == "H") / nrow(data)
p_ganar_visita = sum(data$FTR == "A") / nrow(data)
p_empate = sum(data$FTR == "D") / nrow(data)

cat("P(gana local)  = ", round(100*p_ganar_local, 4), "%\n")
cat("P(empate)      = ", round(100*p_empate,       4), "%\n")
cat("P(gana visita) = ", round(100*p_ganar_visita, 4), "%\n")


# Es decir, en el 45% de los partidos gana el local, mientras que el visitante solo gana en el 30% de ellos.

# Pregunta 2: ¿Qué tan comúnes son las remontadas?
# Para ver esto, agregamos una columna que valga 1 si hubo remontada y valga 0 si no.
# Se considera remontada un partido que se estaba perdiendo en el primer tiempo y se termina ganando hacia el final del partido.

data$remontada = as.integer((data$HTR == "H" & data$FTR == "A") | (data$HTR == "A" & data$FTR == "H"))

# Grafico la frecuencia de las remontadas.

barplot(table(data$remontada),
        main = "Frecuencia de remontadas ",
        xlab = "Remontada", ylab = "Frecuencia",
        col = c(rgb(0.7, 0.85, 1), rgb(0.1, 0.3, 0.7) ),
        border = "black")

cat("Remontadas: ", sum(data$remontada == 1), "\n")
cat("No remontadas:         ", sum(data$remontada == 0), "\n")
cat("P(remontar)  = ", round(100*(sum(data$remontada == 1)/nrow(data)), 4), "%\n")
# Es decir, se remontan aproximadamente 4.5% de los partidos. 
# Si queremos ver la probabilidad de remontar dado que llegamos al entretiempo perdiendo, para local y visitante, el procedimiento es:
cat("P(remontar | perdía el local al ET) = ",round(100 * (sum(data$HTR == "A" & data$FTR == "H") / sum(data$HTR == "A")), 4), "%\n")
cat("P(remontar | perdía el visitante al ET) = ",round(100 * (sum(data$HTR == "H" & data$FTR == "A") / sum(data$HTR == "H")), 4), "%\n")
# Tal como se ve, el local tiene el doble de probabilidades de remontar si llega perdiendo al entretiempo.


# Pregunta 3: ¿Cuáles son los árbitros con mayor severidad disciplinaria? ¿Hay relación con la localidad?
arbitros <- unique(data$Referee)
n_arbitros <- length(arbitros)
cat("Hay",n_arbitros,"arbitros distintos")

# Armo un indice de severidad disciplinaria, asignando 10 = tarjeta amarilla y 25 = tarjeta roja. También junto otras columnas.
data$disciplina <- 10*(data$HY + data$AY) + 25*(data$HR + data$AR)
data$tot_yellow <- data$HY + data$AY

arbitros_df <- data.frame(Referee = arbitros,disciplina = numeric(n_arbitros))
for (i in 1:n_arbitros){
  arbitros_df$disciplina[i] = mean(data$disciplina[data$Referee == arbitros_df$Referee[i]])
}

# Barplot: los 15 árbitros más severos.

top_severos <- order(arbitros_df$disciplina, decreasing = TRUE)[1:15]
barplot(
  arbitros_df$disciplina[top_severos],
  col = rgb(0.1,0.83,0.7,0.85),
  main = "Árbitros con mayor severidad disciplinaria",
  ylab = "Promedio por partido",
  names.arg = arbitros_df$Referee[top_severos],
  las = 2, cex.names = 0.7, border = "black"
)


# tarjetas amarillas según resultado 
y_H <- data$tot_yellow[data$FTR == "H"]
y_A <- data$tot_yellow[data$FTR == "A"]
y_D <- data$tot_yellow[data$FTR == "D"]

hist(y_H,
     col = rgb(0.10, 0.55, 0.10, 0.35), border = rgb(0.10, 0.55, 0.10, 0.9),
     main = "Distribución de amarillas (HY+AY) por resultado",
     xlab = "Amarillas totales")
hist(y_D, add = TRUE,
     col = rgb(0.35, 0.70, 0.35, 0.35), border = rgb(0.35, 0.70, 0.35, 0.9))
hist(y_A, add = TRUE,
     col = rgb(0.40, 0.60, 0.90, 0.35), border = rgb(0.40,0.60,0.90,0.9))
legend("topright",
       fill = c(rgb(0.10, 0.55, 0.10, 0.35), rgb(0.35, 0.70, 0.35, 0.35), rgb(0.40, 0.60, 0.90, 0.35)),
       border = c(rgb(0.10, 0.55, 0.10, 0.9),  rgb(0.35, 0.70, 0.35, 0.9),  rgb(0.40,0.60,0.90,0.9)),
       legend = c("Gana local", "Empate", "Gana visita"),
       bty = "n")

# Se observa que hay una mayor cantidad de amarillas cuando gana el local, intermedia cuando gana la visita y menor cantidad en partidos que resultan en empate.

# Efecto de la localidad en las tarjetas amarillas
plot(jitter(data$HY, amount = 0.15),  #dispersa los puntos aleatoriamente para que se puedan visualizar mejor
     jitter(data$AY, amount = 0.15),
     pch = 16, col = rgb(0.10,0.30,0.70,0.25),
     xlab = "Amarillas del local",
     ylab = "Amarillas del visitante",
     main = "Amarillas: local vs visitante")
abline(0, 1, col = "red", lwd = 2, lty = 2) # La linea que refleja la imparcialidad.

# No se observa un sesgo claro hacia la localidad o la visita, pero sí se ve que una mayor cantidad de amarillas para un equipo correlaciona con una mayor cantidad de amarillas para el otro




################################################
#                                              #
#     Ejercicio 2: Análisis econométrico       #
#                                              #
################################################

data <- read.csv("C:/Users/gusta/Downloads/gapminder.csv", header = TRUE)
library(ggplot2)

# Inciso 1.

arg <- subset(data, country=="Argentina")

ggplot(arg,aes(x = year, y = income_per_person)) +
  geom_line() + 
  geom_point() + 
  labs(title = "Ingreso per cápita argentino", x = "Año", y = "Ingreso per cápita") +
  theme_minimal()

# Se observa que antes de 1875 la tendencia era creciente. Luego, de 1875 hasta 1990, el ingreso per cápita
# fue disminuyendo, hasta volver a aumentar sostenidamnete desde 1990 a 1998, para descender desde 1998 a 2002. 
# A partir de 2002 que el ingreso per cápita aumenta, a excepción de 2009.

# Inciso 2.

train   <- arg[1:(nrow(arg) - 10), ]
test <- arg[(nrow(arg)-9):nrow(arg),]


# Modelo lineal
lineal <- lm(income_per_person ~ year, data = train)

# Modelo polinómico de grado 2
pol2 <- lm(income_per_person ~ poly(year, 2, raw = TRUE), data = train)

# Modelo polinómico de grado 10
pol10 <- lm(income_per_person ~ poly(year, 10, raw = TRUE), data = train)


cat("- - - Resumen de los modelos - - -\n")
cat("\n--- Lineal ---\n");  print(summary(lineal))
cat("\n--- Grado 2 ---\n"); print(summary(pol2))
cat("\n--- Grado 10 ---\n"); print(summary(pol10))

# Veo el error in-sample
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))

pred_lin <- predict(lineal,   newdata = test)
pred_pol2  <- predict(pol2,  newdata = test)
pred_pol10 <- predict(pol10, newdata = test)

rmse_lin <- rmse(test$income_per_person, pred_lin)
rmse_pol2 <- rmse(test$income_per_person, pred_pol2)
rmse_pol10 <- rmse(test$income_per_person, pred_pol10)

cat("\n- - - RMSE en el dataset para testeo - - -\n")
cat(sprintf("Lineal     : %.4f\n", rmse_lin))
cat(sprintf("Cuadrático : %.4f\n", rmse_pol2))
cat(sprintf("Grado 10   : %.4f\n", rmse_pol10))

# Armo un nuevo dataframe para visualizar los resultados sobre todos los años
grid <- data.frame(year = arg$year)
grid$y_lin   <- predict(lineal,   newdata = grid)
grid$y_pol2  <- predict(pol2,  newdata = grid)
grid$y_pol10 <- predict(pol10, newdata = grid)

ggplot() +
  geom_point(data = arg, aes(year, income_per_person), alpha = 0.6) +
  geom_line(data = grid, aes(year, y_lin,   color = "Lineal")) +
  geom_line(data = grid, aes(year, y_pol2,  color = "Grado 2"), linewidth = 1) +
  geom_line(data = grid, aes(year, y_pol10, color = "Grado 10"),   linewidth = 1) +
  labs(
    title = "Modelos en base a todos los años del dataset.",
    x = "Año", y = "Ingreso per cápita", color = "Modelo"
  ) +
  scale_x_continuous(breaks = seq(min(arg$year), max(arg$year), by = 5)) +
  scale_color_manual(values = c("Lineal" = "steelblue", "Grado 2" = "darkorange", "Grado 10" = "darkgreen")) +
  theme_minimal()


# Ahora veo las predicciones sobre el conjunto de testeo
grid2 <- data.frame(year = test$year)
grid2$y_lin   <- predict(lineal,   newdata = grid2)
grid2$y_pol2  <- predict(pol2,  newdata = grid2)
grid2$y_pol10 <- predict(pol10, newdata = grid2)

ggplot() +
  geom_point(data = test, aes(year, income_per_person), alpha = 0.6) +
  geom_line(data = grid2, aes(year, y_lin,   color = "Lineal")) +
  geom_line(data = grid2, aes(year, y_pol2,  color = "Grado 2"), linewidth = 1) +
  geom_line(data = grid2, aes(year, y_pol10, color = "Grado 10"),   linewidth = 1) +
  labs(
    title = "Predicciones en base a tres modelos distintos.",
    x = "Año", y = "Ingreso per cápita", color = "Modelo"
  ) +
  scale_x_continuous(breaks = seq(min(test$year), max(test$year), by = 1)) +
  scale_color_manual(values = c("Lineal" = "steelblue", "Grado 2" = "darkorange", "Grado 10" = "darkgreen")) +
  theme_minimal()

  
# Inciso 3.
library(tidyverse)
paises <-c("Argentina","Brazil","Chile","Peru","Uruguay")

# Lo hago primero con niveles de ingreso per cápita
paises_df <- data %>%
  filter(country %in% paises) %>%
  select(year, country, income_per_person) %>% #Selecciona los paises con la variable income_per_person
  pivot_wider(names_from = country, values_from = income_per_person) %>% # Pone a los países como columnas para poder realizar el análisis.
  arrange(year)

correlaciones <- cor(paises_df)[2:nrow(cor(paises_df)),2:ncol(cor(paises_df))]

cat("- - - La matriz de correlaciones es - - -\n")
print(correlaciones)

# Ahora con variaciones porcentuales anuales

paises_df_pct <- paises_df %>%
  mutate(across(-year,
                ~ . / dplyr::lag(.), #. para hablar de la columna actual, lag para hacer referencia al año anterior, la división para calcular la tasa.
                .names = "{.col}_pct"))

paises_df_pct <- paises_df_pct[2:nrow(paises_df_pct),-c(2:6)]

correlaciones_pct <- cor(paises_df_pct)[2:nrow(cor(paises_df_pct)),2:ncol(cor(paises_df_pct))]

cat("- - - La matriz de correlaciones porcentuales es - - -\n")
print(correlaciones_pct)

# Las correlaciones entre niveles de ingreso per cápita parecen elevadas y más o menos parecidas entre los diferentes países, con algunas excepciones. 
# Todos los países muestran correlación positiva en niveles. Esto sugiere que sus economías se mueven en direcciones parecidas en términos de ingreso per cápita.
# Para el caso argentino, hay correlacion alta y parecida con los 4 países restantes.

# Las correlaciones entre variaciones porcentuales del ingreso per cápita son chicas y heterogéneas, llegando desde números menores a 0.01 a mayores a 0.50
# Esto muestra que algunos paises tienen una correlación mediana en términos de variación porcentual del ingreso per cápita, mientras que otros países no tienen relación en sus movimientos.
# Todas las correlaciones son positivas, aunque entre Brasil y Chile es casi cero.
# Para el caso de Argentina, el mayor par es Uruguay, seguido por Perú, Brasil y, por último, Chile.


# Inciso 5
# Elijo el año 2010.
yearChosen = 2010
year_df <- data[data$year == yearChosen,]
year_df <- year_df %>% drop_na(life_expectancy) # Habia NAs en esta columna
year_df <- year_df[year_df$life_expectancy_female != "-",]
year_df$life_expectancy_female <- as.numeric(year_df$life_expectancy_female)

ggplot(year_df, aes(x = life_expectancy_female, y = life_expectancy)) +
  geom_point() +
  geom_line() +
  geom_abline(color = "red") +
  geom_smooth(method = lm, color = "blue") +
  scale_x_continuous(breaks = seq(min(year_df$life_expectancy_female),max(year_df$life_expectancy_female),by = 5)) +
  theme_minimal() +
  xlab("Expectativa de vida femenina") +
  ylab("Expectativa de vida")  


# Se observa que los países con mayor expectativa de vida tienen a su vez mayor expectativa de vida femenina, y que esta relación
# es menor a 1, ya que la recta azul (regresión) tiene menor pendiente que la recta roja (y = x).


# Inciso 6
modelo <- lm(life_expectancy ~ life_expectancy_female, data = year_df)
cat("- - - El modelo tiene las siguientes características: ' ' '\n")
print(summary(modelo))

# Calculo los valores relevantes
beta0_hat <- summary(modelo)$coefficients["(Intercept)", "Estimate"]
beta1_hat <- summary(modelo)$coefficients["life_expectancy_female", "Estimate"]
R2 <- summary(modelo)$r.squared
cat("El intercepto estimado del modelo es:", beta0_hat)
cat("El coeficiente asociado a la expectativa de vida femenina estimado del modelo es:", beta1_hat)
cat("El R2 del modelo es:", R2)

# Que el coeficiente beta1 sea 0.9037 indica que  cuando la expectativa de vida femenina aumenta 1 año, la expectativa de vida total aumenta 0.9 años aproximadamente.
# El intercepto no tiene interpretación causal, solo ajusta el modelo.
# El R2 es muy alto, lo cual dice que la expectativa de vida femenina explica en gran parte cambios en el nivel de expectativa de vida total.


# Inciso 7
# H0: life_expectancy_female = life_expectancy
# H1: life_expectancy_female > life_expectancy

# Por esa misma razón, uso un test pareado a la derecha unilateralmente.
tt <- t.test(year_df$life_expectancy_female, year_df$life_expectancy, paired = TRUE, alternative = "greater")
print(tt)
# El t-test pareado unilateral entre female y total dio t = 7.105, p muy cercano a cero. La diferencia media (female − total) de 1.72 años con un intervalo de confianza del 95%. 
# Como el intervalo queda estrictamente por encima de 0 y el p-valor es demasiado pequeño, rechazamos la hipótesis nula y concluimos que la esperanza de vida femenina es mayor que la total en estos datos.

# Inciso 8
modelo_multiple <- lm(life_expectancy ~ life_expectancy_female + income_per_person, data = year_df)
cat("- - - El modelo tiene las siguientes características: ' ' '\n")
print(summary(modelo_multiple))

# Vemos que el coeficiente asociado a la expectativa de vida femenina es 0.866 y es significativo. Esto significa que un año más de expectativa de vida femenina se asocia a 0.866 años más de expectativa de vida total.
# El coeficiente asociado al ingreso per cápita es positivo pero casi cero, con cierta significatividad. Suponiendo que el ingreso per cápita está en miles de dolares, esto significa que un aumento en 1000 dolares al ingreso per cápita produce un aumento positivo pero casi cero en años de expectativa de vida total.
# El R2 de esta regresión es 0.8772, por lo que las variables independientes explican con fortaleza a la variable dependiente.
# Al controlar por ingreso, el coeficiente de la expectativa de vida femenina disminuye, pasando de 0.904 a 0.866. Esto muestra que parte del efecto anteriormente atribuido a la expectativa de vida femenina en realidad era explicado por el ingreso per cápita.
# Sin embargo, el efecto de la expectativa de vida femenina permanece siendo grande y significativo.
# Luego, vemos que incluir la variable de ingreso per cápita mejora la estimación central y aumenta el R2 del modelo. Sin embargo, el aumento del R2 es muy chico, por lo que a fines prácticos podría ser un problema incluir esta variable en la regresión.


# Inciso 9
# Las covariables a elegir son: logaritmo del ingreso per cápita, la población y la religión principal.

year_df$log_income <- log(year_df$income_per_person) # creo una nueva columna con el ln del ingreso per cápita.

# Hay errores en la codificación de las religiones, por lo que lo arreglo.
unique(year_df$main_religion) # Vemos que hay varias religiones mal codificadas.
year_df$main_religion <- trimws(year_df$main_religion) 
year_df$main_religion <- gsub("\\s+", " ", year_df$main_religion) # sin espacios y colapsado
tmp <- tolower(year_df$main_religion) # minúsculas 
tmp[tmp %in% c("", "na", "n/a")]              <- NA # unifico la forma de llamar a cada religión
tmp[tmp %in% c("christian")]                  <- "Christian"
tmp[tmp %in% c("muslim")]                     <- "Muslim"
tmp[tmp %in% c("eastern religions")]          <- "Eastern religions"
year_df$main_religion <- factor(tmp) # finalmente paso tmp como factor al dataset con el que trabajo, ya que quiero que las religiones estén en factor y no en character para poder regresarlas.
unique(year_df$main_religion) # se solucionaron los problemas

modelo_propio <- lm(life_expectancy ~ log_income + main_religion + population, data = year_df) # creo el modelo.
cat("- - - El modelo tiene las siguientes características: ' ' '\n")
print(summary(modelo_propio))

# Vemos que el coeficiente asociado al logaritmo natural del ingreso per cápita es 5.691. Esto significa que un aumento en una unidad del log_income genera un aumento en 5.691 años de la expectativa de vida total. Este coeficiente es altamente significativo.
# Esto significa que un aumento del 1% en el ingreso per cápita esta asociado con 0.0569 años más de expectativa de vida, cercano a 21 dias.
# El coeficiente asociado a la población es -4.274*10^(-11), negativo pero muy cercano a cero. Aunque esto no es significativo, por lo que podemos asumir que la estimación es cero. Esto significa que no hay relación entre la cantidad de población y la expectativa de vida total según nuestros datos.
# Los coeficientes de las religiones usan al cristianismo como religión base.
# Luego, que el coeficiente de religiones del Este sea 3.312, con cierta significatividad, significa que hay un efecto positivo marginal con respecto al cristianismo en la expectativa de vida.
# El coeficiente de los musulmanes es 0.673 sin significatividad. Luego, no podemos decir mucho de este regresor.
# El R2 de la regresión es de 0.5901, lo cual muestra que los regresores elegidos solamente explican el 59% de la variación en la expectativa de vida.
# Además, hubo 17 observaciones eliminadas por faltantes en alguna variable.


#####################################
#                                   #
#     Ejercicio 3: Simulación       #
#                                   #
#####################################

## Inciso 1
simular_ingreso <- function(k,n){
  Y <- rchisq(n, df = k) 
  return(Y)
}

# Teoricamente, los valores son
media_teo <- k
varianza_teo <- 2*k
sd_teo <- sqrt(varianza_teo)
# Esto me dice que la elección de k afecta tanto a la media de los ingresos como a su desvío estándar (dispersión)
# Luego, elijo k de forma tal que sea consistente con los datos en estas mediciones observables.

# Veamos cómo cambia para distintos valores de k. Se realiza un gráfico para cada k.
ks = c(5,10,20)

for (k in ks) {
  Y <- simular_ingreso(k,1000)
  hist(Y, freq = FALSE, col = "lightblue",
       main = paste("Ingreso ~ Chi-cuadrado(k =", k, ")"),
       xlab = "Y")
  curve(dchisq(x, df = k), add = TRUE, col = "red", lwd = 2)
}


## Inciso 2
Y <- simular_ingreso(5,1000)


demanda_cd <- function(Y, p1, p2, a1, a2){
  if (p1 <= 0 || p2 <= 0 || a1 <= 0 || a2 <= 0 || Y < 0){
    print("Los valores ingresados deben ser positivos o, en el caso del ingreso, no negativo")
  } else if (a1 + a2 - 1 > 1e-8){ #Tuve que usar un redondeo con 1e-8 porque, de lo contrario, me saltaba siempre este error
    print("la suma de los alphas debe dar 1")
  }  else {
    x_1 <- (a1 * Y) / p1
    x_2 <- (a2 * Y) / p2
    V <- (x_1 ^ a1) * (x_2 ^ a2)
    return(c(x_1,x_2,V))
  }
}

demanda_cd(mean(Y),10,5,0.5,0.5)


## Inciso 3
n = 10000
k = 5
p1 = 10
p2 = 8
a1 = 0.3
a2 = 0.7
Y <- simular_ingreso(k,n)
resultados <- matrix(NA_real_,n,3)

for (j in 1:n){
  for (i in 1:3){
    resultados[j,i] = demanda_cd(Y[j],p1,p2,a1,a2)[i]
  }
}

# Extraigo los resultados como vectores para poder graficar
x1 <- resultados[,1]
x2 <- resultados[,2]
V <- resultados[,3]


hist(x1, freq = FALSE, col = "lightblue",
     main = "x_1* (empírico)", xlab = "x_1*")
abline(v = mean(x1), col = "blue", lwd = 2)
abline(v = quantile(x1, c(0.25, 0.5, 0.75)), col = "red", lty = 2)

hist(x2, freq = FALSE, col = "lightblue",
     main = "x_2* (empírico)", xlab = "x_2*")
abline(v = mean(x2), col = "blue", lwd = 2)
abline(v = quantile(x2, c(0.25, 0.5, 0.75)), col = "red", lty = 2)

hist(V, freq = FALSE, col = "lightblue",
     main = "V* (empírico)", xlab = "V*")
abline(v = mean(V), col = "blue", lwd = 2)
abline(v = quantile(V, c(0.25, 0.5, 0.75)), col = "red", lty = 2)

# Reporto los valores.

qx1 <- quantile(x1, c(0.25, 0.5, 0.75))
qx2 <- quantile(x2, c(0.25, 0.5, 0.75))
qV  <- quantile(V,  c(0.25, 0.5, 0.75))

cat("\n--- Medias (empíricas) ---\n")
cat("mean(x1*) =", mean(x1), "\n")
cat("mean(x2*) =", mean(x2), "\n")
cat("mean(V*)  =", mean(V), "\n")

cat("\n--- Cuartiles (empíricos: Q1, Q2, Q3) ---\n")
cat("x1*:", round(qx1, 4), "\n")
cat("x2*:", round(qx2, 4), "\n")
cat("V* :", round(qV,  4), "\n")


# Inciso 4
prob_bajo_consumo <- function(c,j){
  if (j == 1){
    x = x1
  } else {x = x2}
  # Uso que probabilidad_estimada = cantidad_aciertos / cantidad_total
  aciertos <- 0
  for (i in 1:n){
    if (x[i] < c){
      aciertos = aciertos + 1
    }
  }
  return(aciertos/n)
}


# Inciso 5
p1_nuevo = p1*1.2
resultados_nuevos <- matrix(NA_real_,n,3)

for (j in 1:n){
  for (i in 1:3){
    resultados_nuevos[j,i] = demanda_cd(Y[j],p1_nuevo,p2,a1,a2)[i]
  }
}

x1_nuevos <- resultados_nuevos[,1]
x2_nuevos <- resultados_nuevos[,2] 
V_nuevos <- resultados_nuevos[,3]

qx1_nuevos <- quantile(x1_nuevos, c(0.25, 0.5, 0.75))
qx2_nuevos <- quantile(x2_nuevos, c(0.25, 0.5, 0.75))
qV_nuevos  <- quantile(V_nuevos,  c(0.25, 0.5, 0.75))

# Comparo las distribuciones a través de indicadores empíricos

cat("\n--- Diferencia de medias (empíricas) ---\n")
cat("mean(x1*) - mean(x1*_nuevos) =", mean(x1) - mean(x1_nuevos), "\n")
cat("mean(x2*) - mean(x2*_nuevos) =", mean(x2) - mean(x2_nuevos), "\n") # no cambia pues x2* no depende de p1
cat("mean(V*) - mean(V*_nuevos) =", mean(V) - mean(V_nuevos), "\n") # baja pues se enfrenta a un mayor precio


cat("\n--- Diferencia de cuartiles (empíricos) ---\n")
cat("x1* - x1*_nuevos:", round(qx1, 4) - round(qx1_nuevos,4), "\n")
cat("x2* - x2*_nuevos:", round(qx2, 4) - round(qx2_nuevos,4), "\n")
cat("V* - V*_nuevos:", round(qV, 4) - round(qV_nuevos,4), "\n")


# Inciso 6


hist(x1, freq = FALSE, col = "lightblue",
     main = "x1*: antes vs. después (shock p1 +20%)", xlab = "x1*")
hist(x1_nuevos, freq = FALSE, col = NA, border = "red", add = TRUE, lty = 2)

abline(v = mean(x1),        col = "blue", lwd = 2)
abline(v = mean(x1_nuevos), col = "darkred", lwd = 2, lty = 2)  

legend("topright",
       legend = c("Antes", "Después (borde)"),
       fill   = c("lightblue", "red"),
       border = c("black", "lightblue"),
       bty    = "n")

# Vemos que la distribución tiene mayor densidad en valores bajos y menor densidad en valores altos.
# Esto también se visualiza en un menor promedio, tal como es mostrado por las rectas verticales.
# La intuición económica es que el precio del bien 1 aumentó, y todo lo demás quedó constante. Por lo tanto,
# el individuo decidirá consumir menos del bien 1. Dado que el consumo del bien 1 también depende del ingreso,
# el resultado es que toda la distribución se mueve hacia valores menores de consumo del bien 1.
# Lógicamente, ante menor consumo, con todo lo demás constante, la utilidad indirecta V cae. Esto fue mostrado empíricamente en el inciso 5.


## Inciso 7
alpha1 <- numeric(n)
a = 1
b = 2
# Elijo a y b arbitrarios, sabiendo que la esperanza es a/(a+b) y la varianza es ab/((a+b)^2*(a+b+1))
for(i in 1:n){
  alpha1[i] <- rbeta(n = 1,shape1 = a, shape2 = b)
}

resultados_het <- matrix(NA_real_,n,3)

for (j in 1:n){
  for (i in 1:3){
    resultados_het[j,i] = demanda_cd(Y[j],p1,p2,alpha1[j],1-alpha1[j])[i]
  }
}

# Extraigo los resultados como vectores para poder graficar
x1_het <- resultados_het[,1]
x2_het <- resultados_het[,2]
V_het <- resultados_het[,3]

qx1_het <- quantile(x1_het, c(0.25, 0.5, 0.75))
qx2_het <- quantile(x2_het, c(0.25, 0.5, 0.75))
qV_het  <- quantile(V_het,  c(0.25, 0.5, 0.75))

# Comparo las distribuciones iniciales con las heterogeneas a través de indicadores empíricos

cat("\n--- Diferencia de medias (empíricas) ---\n")
cat("mean(x1*) - mean(x1*_het) =", mean(x1) - mean(x1_het), "\n")
cat("mean(x2*) - mean(x2*_het) =", mean(x2) - mean(x2_het), "\n") # no cambia pues x2* no depende de p1
cat("mean(V*) - mean(V*_het) =", mean(V) - mean(V_het), "\n") # baja pues se enfrenta a un mayor precio


cat("\n--- Diferencia de cuartiles (empíricos) ---\n")
cat("x1* - x1*_het:", round(qx1, 4) - round(qx1_het,4), "\n")
cat("x2* - x2*_het:", round(qx2, 4) - round(qx2_het,4), "\n")
cat("V* - V*_het:", round(qV, 4) - round(qV_het,4), "\n")

# Ahora veo el cambio de precio en las heterogeneas.

resultados_nuevos_het <- matrix(NA_real_,n,3)

for (j in 1:n){
  for (i in 1:3){
    resultados_nuevos_het[j,i] = demanda_cd(Y[j],p1_nuevo,p2,alpha1[j],1-alpha1[j])[i]
  }
}

x1_nuevos_het <- resultados_nuevos_het[,1]
x2_nuevos_het <- resultados_nuevos_het[,2] 
V_nuevos_het <- resultados_nuevos_het[,3]

qx1_nuevos_het <- quantile(x1_nuevos_het, c(0.25, 0.5, 0.75))
qx2_nuevos_het <- quantile(x2_nuevos_het, c(0.25, 0.5, 0.75))
qV_nuevos_het  <- quantile(V_nuevos_het,  c(0.25, 0.5, 0.75))

# Comparo las distribuciones a través de indicadores empíricos

cat("\n--- Diferencia de medias (empíricas) ---\n")
cat("mean(x1*) - mean(x1*_nuevos_het) =", mean(x1) - mean(x1_nuevos_het), "\n")
cat("mean(x2*) - mean(x2*_nuevos_het) =", mean(x2) - mean(x2_nuevos_het), "\n") # no cambia pues x2* no depende de p1
cat("mean(V*) - mean(V*_nuevos_het) =", mean(V) - mean(V_nuevos_het), "\n") # baja pues se enfrenta a un mayor precio


cat("\n--- Diferencia de cuartiles (empíricos) ---\n")
cat("x1* - x1*_nuevos_het:", round(qx1, 4) - round(qx1_nuevos_het,4), "\n")
cat("x2* - x2*_nuevos_het:", round(qx2, 4) - round(qx2_nuevos_het,4), "\n")
cat("V* - V*_nuevos_het:", round(qV, 4) - round(qV_nuevos_het,4), "\n")

# Intuitivamente, un mayor alpha1 simboliza mayor importancia del bien 1 en la canaste del consumidor.
# Racionalmente, el consumidor destinará mayor parte de su ingreso al bien 1 a costa de consumo del bien 2, el cual se volvió menos deseado.
# Entonces, es esperable que un aumento de alpha1 tenga como consecuencia un aumento del consumo de x1 y una disminución del consumo del bien 2, a igual ingreso y precios.



