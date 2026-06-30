dinosaurios <- read.csv("data/dinosaur.csv",header=TRUE)
colnames(dinosaurios)
class(dinosaurios)



dinosaurios$Country[
  grepl("\\.$", dinosaur_limpio$Country) |
    grepl("&", dinosaur_limpio$Country) |
    grepl(",",dinosaur_limpio$Country)
] <- "unknown"
# Comprobar estado de cateogrías en Country 
unique(dinosaur_limpio$Country)
sum(grepl("\\.$", dinosaurios$Country))
sum(grepl(",", dinosaurios$Country))
sum(grepl("&", dinosaurios$Country))


# Pregunta 1
dino_dieta <-dinosaur_limpio %>% 
  dplyr::group_by(Diet) %>% 
  summarise(
    num_dino=n(),
    .groups = "drop"
  )

# Gráfico 
library(ggplot2)
dino_dieta %>%
  arrange(desc(num_dino)) %>%
  mutate(Diet = factor(Diet, levels = Diet)) %>%
  ggplot(aes(x = Diet, 
             y = num_dino,
            fill=num_dino)) +
  geom_col() +
  scale_fill_gradient(
    low = "skyblue",
    high = "blue")+
  labs(
    title = "Número de dinosaurios por dieta",
    x = "Tipo de dieta",
    y = "Número de dinosaurios registrados",
    fill="No.dinosaurios"
  ) +
  theme_minimal() 

#Pregunta 2
dino_paises <-dinosaur_limpio %>% 
  dplyr::group_by(Country) %>% 
  summarise( num_paises=n(), 
             .groups = "drop" )

library(ggplot2)
dino_paises %>%
  arrange(desc(num_paises)) %>%
  mutate(Country = factor(Country, levels = rev(Country))) %>%
  ggplot(aes(x = num_paises, y = Country)) +
  geom_point(size = 3, color = "steelblue") +
  labs(
    title = "Número de fósiles registrados por país",
    x = "Número de dinosaurios",
    y = "País"
  ) +
  theme_minimal()

# Pregunta 3 
# Prueba chi-cuadrada 

library(dplyr)
library(forcats)
library(stringr)

dinosaur_limpio <- dinosaur_limpio %>%
  mutate(
    Period = str_trim(Period),
    Period = fct_collapse(
      Period,
      "Cretaceous" = c("Cretaceous",
                       "Early Cretaceous",
                       "Late Cretaceous",
                       "Early-Late Cretaceous"),
      "Jurassic" = c("Jurassic",
                     "Middle Jurassic",
                     "Early Jurassic"),
      "Triassic" = c("Triassic",
                     "Late Triassic"),
      "Indeterminated" = c("Jurassic/Cretaceous",
                           "Triassic or Jurassic",
                           "Triassic/Jurassic"),
      "unknown" = "unknown"
    )
  )



tabla_chi <-  table(dinosaur_limpio$Period, dinosaur_limpio$Diet)
tabla_chi
resultado <- chisq.test(tabla_chi)

if (resultado$p.value < 0.05) {
  cat("Se rechaza H₀. Existe evidencia estadísticamente significativa de una relación entre el período geológico y la dieta de los dinosaurios (p =", 
      round(resultado$p.value, 4), ").")
} else {
  cat("No se rechaza H₀. No existe evidencia estadísticamente significativa de una relación entre el período geológico y la dieta de los dinosaurios (p =", 
      round(resultado$p.value, 4), ").")
}


ggplot(dinosaur_limpio,
       aes(x = Period, fill = Diet)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proporción de dietas por período geológico",
    x = "Período geológico",
    y = "Proporción",
    fill = "Dieta"
  ) +
  theme_minimal()




