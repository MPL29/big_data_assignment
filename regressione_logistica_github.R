# importo il mio file .cvs da github e lo salvo come coffe.csv
coffe.csv <- read.csv("https://raw.githubusercontent.com/MPL29/big_data_assigment/main/Assignment-del-20-giugno-2022---Statistica-per-i-big-data-main/data-modelling/coffe.csv.csv?token=GHSAT0AAAAAABV63LCTKGZHAJ7ZLL3SBN3GYVZUH6A")

#escludo la variabile total_cup_points dal mio dataframe
library(dplyr)  # carico lalibreria dplyr che mi permette di usare la funzione select() per escludere la colonna selezionata
coffe <- select(coffe.csv, -total_cup_points) # elimino la colonna total_cup_points e creo un nuovo df

##### data manipulation

# separo il dataset in train e test set
# separo il 75% dei dati in train e il restante 25% in test

# install.packages("caret") # installo il pachetto caret 
library(caret) # carico la libreria

# creo la ripartizione del mio dataset coffe 
dataset <- (createDataPartition(coffe$high_quality,
                                times = 1,
                                p = 0.75,
                                list = FALSE))

training <- coffe[ dataset, ] # tranining set
test <- coffe[ -dataset, ] # set test

# visualizzo i dati contenuti in training e test set
head(training, 5) # per comodità visualizzo solo le prime 5 righe per entrambi
head(test, 5)

##### regressione logisica binomiale

# creo diversi modelli numerandoli progressivamente e usando di volta in volta variabili diverse
# infine calcolo l'AIC per poter confrontare i modelli ed infine scegliere il migliore

# model_1: high_quality ~ flavor
model_1 <- glm(high_quality ~ flavor, 
               family = "binomial", # la funzione "binomial usa di default il modello "logit" quindi non è necessario dichiararla
               data = training)

#visualizzo il risultato della regressione
summary(model_1)

# aic per il primo modello
# anche se summary mi restituisce il valore ma calcolandolo a parte lo potrò raggrupare con quelli degli altri modelli in un df
aic_1 <- AIC(model_1)

# model_2: high_quality ~ country_of_origin
model_2 <- glm(high_quality ~ country_of_origin, 
               family = "binomial", 
               data = training)

summary(model_2)

aic_2 <- AIC(model_2)

# model_3: high_quality ~ aroma
model_3 <- glm(high_quality ~ aroma, 
               family = "binomial", 
               data = training)

summary(model_3)

aic_3 <- AIC(model_3)

# model_4: high_quality ~ flavor + aroma + aftertaste
model_4 <- glm(high_quality ~ flavor + aroma + aftertaste, 
               family = "binomial", 
               data = training)

summary(model_4)

aic_4 <- AIC(model_4)

# model_5: high_quality ~ country_of_origin + flavor + aroma + aftertaste
model_5 <- glm(high_quality ~ country_of_origin + flavor + aroma + aftertaste, 
               family = "binomial", 
               data = training)

summary(model_5)

aic_5 <- AIC(model_5)

##### confronto modelli

# creo il df comprensivo dei risultati aic di tutti i modelli
aic_results <- t(data.frame(aic_1, aic_2, aic_3, aic_4, aic_5))

colnames(aic_results) <- "valore AIC Training Set" # cambio il nome della colonna

# per valutare il modello migliore si sceglie quello con l'AIC minore
# però dato che uso i dati di training i risultati della regressioni cambiano
# e il migliore cambia in base ai dati usati, in ogni caso i modelli migliori sono 
# il model_4 e model_5 a seconda dei casi

# se invece dovessi usare l'intero dataset "coffe" i risultati non cambierebbero

##### modelli con intero dataset

# rifaccio esattamente gli stessi modelli con l'intero dataset
# in questo caso li numero usando le lettere dell'alfabeto

# model_a: high_quality ~ flavor
model_a <- glm(high_quality ~ flavor, 
               family = "binomial", # la funzione "binomial usa di default il modello "logit" quindi non è necessario dichiararlo
               data = coffe)

summary(model_a)

aic_a <- AIC(model_a)

# model_b: high_quality ~ country_of_origin
model_b <- glm(high_quality ~ country_of_origin, 
               family = "binomial", 
               data = coffe)

summary(model_b)

aic_b <- AIC(model_b)

# model_c: high_quality ~ aroma
model_c <- glm(high_quality ~ aroma, 
               family = "binomial", 
               data = coffe)

summary(model_c)

aic_c <- AIC(model_c)

# model_d: high_quality ~ flavor + aroma + aftertaste
model_d <- glm(high_quality ~ flavor + aroma + aftertaste, 
               family = "binomial", 
               data = coffe)

summary(model_d)

aic_d <- AIC(model_d)

# model_5: high_quality ~ country_of_origin + flavor + aroma + aftertaste
model_e <- glm(high_quality ~ country_of_origin + flavor + aroma + aftertaste, 
               family = "binomial", 
               data = coffe)

summary(model_e)

aic_e <- AIC(model_e)

### ricreo il df dei risultati e poi unisco tutto in un unico df
aic_results_coffe <- t(data.frame(aic_a, aic_b, aic_c, aic_d, aic_e))

colnames(aic_results_coffe) <- "valore AIC intero df"

# unisco i due df degli aic per permetterne un più facile confronto
aic_all_results <- t(data.frame(aic_results, aic_results_coffe))


# quello che noto è che:
# - gli aic del modello completo sono molto più alti rispetto a quelli usando il training set
# - il modello migliore è il: model_5


