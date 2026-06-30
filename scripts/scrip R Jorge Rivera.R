

dinosaur <- read.csv("data/dinosaur.csv")

dinosaurios <- read.csv("data/dinosaur.csv",header=TRUE)
colnames(dinosaurios)
class(dinosaurios)

ncol(dinosaurios)
nrow(dinosaurios)

#Limpieza de datos

colSums(is.na(dinosaurios))
class(dinosaurios$Name)
class(dinosaurios$Period)
class(dinosaurios$Diet)
class(dinosaurios$Country)

library(dplyr)

dinosaurs_limpio <- dinosaurios %>%
  tidyr::drop_na()

#Corrección de datos:

  #dieta

dinosaur_limpio <- dinosaurios %>%
  mutate(Diet = recode(Diet, "herbivore" = "Herbivore")) %>%
  mutate(Diet = recode(Diet, "(herbivore)" = "Herbivore")) %>%
  mutate(Diet = recode(Diet, "omnivorous" = "Omnivorous")) %>%
  mutate(Diet = recode(Diet, "omnivore" = "Omnivorous")) %>%
  mutate(Diet = recode(Diet, "(carnivore)" = "Carnivore")) %>%
  mutate(Diet = recode(Diet, "carnivore" = "Carnivore")) %>%
  mutate(Diet = recode(Diet, "carnivore?" = "Carnivore")) %>%
  mutate(Diet = recode(Diet, "(unknown)" = "unknown")) %>%
  mutate(Diet = recode(Diet, "?" = "unknown")) %>%
  mutate(Period = recode(Period, "Cretaceous       " = "Cretaceous"))%>%
  mutate(Period = recode(Period, "(unknown)" = "unknown"))

unique(dinosaurios$Diet)

  #paises

dinosaurios$Country[
  grepl("\\.$", dinosaur_limpio$Country) |
    grepl("&", dinosaur_limpio$Country) |
    grepl(",",dinosaur_limpio$Country)
] <- "unknown"



#Pregunta 4: ¿Existe alguna relación entre la dieta y la región continental de los registros?

# Los registros de dinosaurios están distribuidos a travéz de varias regiones del mundo, incluyendo 63 paises, debido a esto, se agruparon los datos de sitio de colecta (Country) de acuerdo a su región continental para tener una mejor visivilidad de los datos

library(dplyr)
library(forcats)
library(stringr)

dinosaur_limpio <- dinosaurios %>%
  mutate(
    Country = str_trim(Country),
    Country = fct_collapse(
      Country,
      "America" = c("North America",
                    "South America"),
      "Europe" = c("England",
                   "Spain",
                   "France",
                   "Hungary",
                   "Portugal",
                   "Germany",
                   "Romania",
                   "Spain, Portugal, England",
                   "Netherlands",
                   "Czech Republic",
                   "England, France, Switzerland, Morocco",
                   "Belgium",
                   "England, France, Spain, Portugal",
                   "Denmark",
                   "Croatia",
                   "England, France",
                   "England and France",
                   "Belgium, England, Germany",
                   "England, France, Austria, Romania, Belgium",
                   "Austria",
                   "Netherlands, Belgium",
                   "France, Czech Republic, Romania, Spain",
                   "Austria, Romania, France, Hungary",
                   "Italy",
                   "Austria, France, Romania, Hungary",
                   "Portugal, Spain",
                   "Spain, Portugal",
                   "Poland"),
      "Africa" = c("South Africa",
                   "Egypt, Niger",
                   "Morocco",
                   "South Africa, Lesotho, Zimbabwe",
                   "Niger",
                   "Angola",
                   "Madagascar",
                   "Tanzania",
                   "Egypt,Niger, Morocco, Algeria",
                   "Egypt, Niger, Algeria, Morocco",
                   "Algeria",
                   "Morocco, Algeria, Egypt",
                   "South Africa, Zimbabwe, Lesotho",
                   "Lesotho",
                   "Egypt",
                   "Malawi",
                   "Tunisia",
                   "Zimbabwe"),
      "Asia" = c("China",
                 "Mongolia",
                 "Japan",
                 "India",
                 "Kazakhstan",
                 "Russia",
                 "Pakistan",
                 "Uzbekistan",
                 "South Korea",
                 "Kyrgyzstan",
                 "Laos",
                 "Thailand",
                 "Korea"),
      "Antarctica" = c("Antarctica"),
      "Australia" = c("Australia"),
      "unknown" = c("unknown")
    )
  )

# Verificar los niveles de categoría

unique(dinosaur_limpio$Country)

# Elaboración de tablas de contingencia y prueba de Chi cuadrada

datos <- dinosaur_limpio[, c("Diet", "Country")]

tabla <- table(datos$Diet, datos$Country)

print(tabla)


resultado_chi <- chisq.test(tabla)


print(resultado_chi)

#gráfica

tabla <- matrix(
  c(24,  2,  4, 64, 110, 129,  17,
    0,   0,  0,  0,   0,   1,   0,
    0,   0,  0,  0,   4,   0,   0,
    53,  4, 17,113, 224, 278,  32,
    0,   0,  0,  2,   7,   1,   0,
    0,   0,  0,  0,   0,   1,   0,
    4,   0,  0,  1,  36,  12,   3,
    1,   0,  2,  1,   6,   1,   0),
  nrow = 8, byrow = TRUE
)

rownames(tabla) <- c("Carnivore", "carnivore/insectivore", "carnivore/omnivore",
                     "Herbivore", "herbivore/omnivore", "insectivore",
                     "Omnivorous", "unknown")
colnames(tabla) <- c("Africa", "Antarctica", "Australia", "Europe",
                     "Asia", "America", "unknown")


tabla_df <- as.data.frame(as.table(tabla))
colnames(tabla_df) <- c("Dieta", "Continente", "Frecuencia")


library(ggplot2)


ggplot(tabla_df, aes(x = Continente, y = Frecuencia, fill = Dieta)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Porcentaje", 
       title = "Distribución de dieta por continente") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pregunta 5: ¿Cómo se distribuyen los taxones de acuerdo con la escala geológica?

#Agrupación de paises por región continental

class(dinosaur_limpio$Country)

library(dplyr)
library(forcats)
library(stringr)

dinosaur_limpio <- dinosaur %>%
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
      "Unknown" = c ("unknown", "(unknown)")
    )
  )

# verificación de datos

unique(dinosaur_limpio$Period)

# tabla de frecuencias 

tabla_frecuencias <- dinosaur_limpio %>%
  count(Period, name = "Frecuencia_Absoluta") %>%
  mutate(
    Frecuencia_Relativa = Frecuencia_Absoluta / sum(Frecuencia_Absoluta),
    Porcentaje = Frecuencia_Relativa * 100
  )

print(tabla_frecuencias)

# Tabla de distribución

datos <- as.data.frame(tabla_frecuencias)
colnames(datos) <- c("Edad", "Frecuencia")

library(ggplot2)

ggplot(tabla_frecuencias, aes(x = Period, y = Frecuencia_Absoluta)) +
  geom_col(fill = "steelblue", color = "black") +
  labs(title = "Distribución de taxones por edad geocronológica", 
       x = "Edades", 
       y = "Frecuencia") +
  theme_minimal()

