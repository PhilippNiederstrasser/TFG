library(tseries)
library(moments)
library(dplyr)
library(ggplot2)
library(zoo)
library(tidyr)
library(lubridate)
library(readr)
library(forecast)
library(rugarch)
library(xts)

#data import (hecho con readr)
IBEX <- read_csv("TFG PHILIPP NIEDERSTRASSER (ESC06)/data/^IBEX-3.csv", 
                 col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                  Open = col_number(), High = col_number(), 
                                  Low = col_number(), Close = col_number(), 
                                  `Adj Close` = col_number(), Volume = col_integer()))
View(IBEX)

CAC <- read_csv("TFG PHILIPP NIEDERSTRASSER (ESC06)/data/^FCHI.csv", 
                col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                 Open = col_number(), High = col_number(), 
                                 Low = col_number(), Close = col_number(), 
                                 `Adj Close` = col_number(), Volume = col_integer()))
View(CAC)

FTSEMIB <- read_csv("TFG PHILIPP NIEDERSTRASSER (ESC06)/data/FTSE MIB Historical Data.csv", 
                    col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                     Price = col_number(), Open = col_number(), 
                                     High = col_number(), Low = col_number()))
View(FTSEMIB)

PSI <- read_csv("TFG PHILIPP NIEDERSTRASSER (ESC06)/data/PSI20.LS.csv", 
                col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                 Open = col_number(), High = col_number(), 
                                 Low = col_number(), Close = col_number(), 
                                 `Adj Close` = col_number(), Volume = col_integer()))
View(PSI)



#Modificando el dataset FTSEMIB siendo de un proveedor diferente que el de los otros 3 datasets
colnames(FTSEMIB)[colnames(FTSEMIB) == "Price"] <- "Close"
colnames(FTSEMIB)[colnames(FTSEMIB) == "Vol."] <- "Volume"
FTSEMIB$`Change %` <- NULL
FTSEMIB$`Adj Close` <- FTSEMIB$Close
FTSEMIB <- FTSEMIB[c("Date", "Open", "High", "Low", "Close", "Adj Close", "Volume")]

#invertiendo filas en FTSEMIB para que coincida con los otros 3 dataframes
FTSEMIB <- FTSEMIB[order(FTSEMIB$Date), ]

#Elimando la columna "Volume" de los datasets siendo irrelevantes
FTSEMIB$Volume <- NULL
CAC$Volume <- NULL
IBEX$Volume <- NULL
PSI$Volume <- NULL

#invertiendo filas en FTSEMIB para que coincida con los otros 3 dataframes
FTSEMIB <- FTSEMIB[order(FTSEMIB$Date), ]

#igualando número de obs en todos los datasets
CAC <- tail(CAC, 1274)
IBEX <- tail(IBEX, 1274)
PSI <- tail(PSI, 1274)

#cambiando el nombre de la columna "Adj Close"
names(CAC)[names(CAC) == "Adj Close"] <- "Price"
names(IBEX)[names(IBEX) == "Adj Close"] <- "Price"
names(PSI)[names(PSI) == "Adj Close"] <- "Price"
names(FTSEMIB)[names(FTSEMIB) == "Adj Close"] <- "Price"

#tratamiento NAs
sum(!complete.cases(IBEX))
sum(!complete.cases(CAC))
sum(!complete.cases(FTSEMIB))
sum(!complete.cases(PSI))

which(is.na(IBEX), arr.ind=TRUE)
which(is.na(CAC), arr.ind=TRUE)
which(is.na(PSI), arr.ind=TRUE)

IBEX <- data.frame(lapply(IBEX, function(column) {
  if(is.numeric(column)) {
    return(na.approx(column, na.rm = FALSE))} 
  else {
    return(column) }}))

CAC <- data.frame(lapply(CAC, function(column) {
  if(is.numeric(column)) {
    return(na.approx(column, na.rm = FALSE))} 
  else {
    return(column) }}))

PSI <- data.frame(lapply(PSI, function(column) {
  if(is.numeric(column)) {
    return(na.approx(column, na.rm = FALSE))} 
  else {
    return(column) }}))

#chequeo final
sum(!complete.cases(IBEX))
sum(!complete.cases(CAC))
sum(!complete.cases(FTSEMIB))
sum(!complete.cases(PSI))

#análisis exploratorio de los datos
ggplot(IBEX, aes(x = Date, y = Price)) +
  geom_line() +
  labs(x = "Date", y = "Price", title = "Evolución del Precio IBEX") +
  theme_minimal() +
  geom_vline(xintercept = as.Date("2020-02-01"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.Date("2020-03-31"), linetype = "dashed", color = "red") +
  annotate("text", x = as.Date("2020-11-25"), y = max(IBEX$Price, na.rm = TRUE) * 0.95, label = "COVID-19 Crash", size = 4, angle = 0, vjust = 0, color = "red")

ggplot(CAC, aes(x = Date, y = Price)) +
  geom_line() +
  labs(x = "Date", y = "Price", title = "Evolución del Precio CAC") +
  theme_minimal()

ggplot(FTSEMIB, aes(x = Date, y = Price)) +
  geom_line() +
  labs(x = "Date", y = "Price", title = "Evolución del Precio FTSEMIB") +
  theme_minimal() 

ggplot(PSI, aes(x = Date, y = Price)) +
  geom_line() +
  labs(x = "Date", y = "Price", title = "Evolución del Precio PSI") +
  theme_minimal() 

#añadiendo la columna de "returns" a cada dat set
#como la diferencia de los logaritmos de precios consecutivos multiplicado por 100
#Asumiendo que df$price contiene tus precios
IBEX$return <- c(NA, diff(log(IBEX$Price))) * 100
FTSEMIB$return <- c(NA, diff(log(FTSEMIB$Price))) * 100
CAC$return <- c(NA, diff(log(CAC$Price))) * 100
PSI$return <- c(NA, diff(log(PSI$Price))) * 100

#plotting returns
ggplot(IBEX, aes(x = Date, y = return)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Retornos Diarios del IBEX", x = "Fecha", y = "Retornos (%)") + 
  theme_minimal()

ggplot(CAC, aes(x = Date, y = return)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Retornos Diarios del CAC", x = "Fecha", y = "Retornos (%)") + 
  theme_minimal()

ggplot(PSI, aes(x = Date, y = return)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Retornos Diarios del PSI", x = "Fecha", y = "Retornos (%)") + 
  theme_minimal()

ggplot(FTSEMIB, aes(x = Date, y = return)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Retornos Diarios del FTSEMIB", x = "Fecha", y = "Retornos (%)") + 
  theme_minimal()

#Eliminar la primera fila de cada dataframe (debido a return NA)
IBEX <- IBEX[-1, ]
CAC <- CAC[-1, ]
PSI <- PSI[-1, ]
FTSEMIB <- FTSEMIB[-1, ]

#análisis descriptivo 
#ADF Test
adf.test(IBEX$Price)
adf.test(CAC$Price)
adf.test(PSI$Price)
adf.test(FTSEMIB$Price)

adf.test(IBEX$return)
adf.test(CAC$return)
adf.test(PSI$return)
adf.test(FTSEMIB$return)

#histogramas returns
IBEX$Index <- 'IBEX'
CAC$Index <- 'CAC'
PSI$Index <- 'PSI'
FTSEMIB$Index <- 'FTSEMIB'

all_returns <- bind_rows(IBEX, CAC, PSI, FTSEMIB)

ggplot(all_returns, aes(x = return)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ Index, scales = "free_y") +
  labs(title = "Histograms of Returns", x = "Returns", y = "Frequency") +
  theme_minimal()

#moments
descrIBEX <- as.matrix( c( mean(IBEX$return), median(IBEX$return), sd(IBEX$return), skewness(IBEX$return), kurtosis(IBEX$return) ) )
rownames(descrIBEX) <- c("Mean", "Median", "SD", "Skewness", "Kurtosis")
print(descrIBEX)

descrCAC <- as.matrix( c( mean(CAC$return), median(CAC$return), sd(CAC$return), skewness(CAC$return), kurtosis(CAC$return) ) )
rownames(descrCAC) <- c("Mean", "Median", "SD", "Skewness", "Kurtosis")
print(descrCAC)

descrPSI <- as.matrix( c( mean(PSI$return), median(PSI$return), sd(PSI$return), skewness(PSI$return), kurtosis(PSI$return) ) )
rownames(descrPSI) <- c("Mean", "Median", "SD", "Skewness", "Kurtosis")
print(descrPSI)

descrFTSEMIB <- as.matrix( c( mean(FTSEMIB$return), median(FTSEMIB$return), sd(FTSEMIB$return), skewness(FTSEMIB$return), kurtosis(FTSEMIB$return) ) )
rownames(descrFTSEMIB) <- c("Mean", "Median", "SD", "Skewness", "Kurtosis")
print(descrFTSEMIB)

#Convertir matrices en data frames y añadir columna de índice
df_IBEX <- data.frame(Value = descrIBEX[,1], Moment = rownames(descrIBEX), Index = 'IBEX')
df_CAC <- data.frame(Value = descrCAC[,1], Moment = rownames(descrCAC), Index = 'CAC')
df_FTSEMIB <- data.frame(Value = descrFTSEMIB[,1], Moment = rownames(descrFTSEMIB), Index = 'FTSEMIB')
df_PSI <- data.frame(Value = descrPSI[,1], Moment = rownames(descrPSI), Index = 'PSI')

#Combinar data frames
combined_df <- bind_rows(df_IBEX, df_CAC, df_FTSEMIB, df_PSI)

#Separar para ggplots
mean_df <- combined_df[combined_df$Moment == 'Mean', ]
sd_df <- combined_df[combined_df$Moment == 'SD', ]
skewness_df <- combined_df[combined_df$Moment == 'Skewness', ]
kurtosis_df <- combined_df[combined_df$Moment == 'Kurtosis', ]
median_df <- combined_df[combined_df$Moment == 'Median', ]

#Plotting
ggplot(mean_df, aes(x = Index, y = Value, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("IBEX" = "cadetblue", "CAC" = "cadetblue1", "FTSEMIB" = "cadetblue2", "PSI" = "cadetblue3")) +
  theme_minimal() +
  labs(x = "Índice", y = "Media", title = "Comparación de las Medias de los Índices")

ggplot(sd_df, aes(x = Index, y = Value, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("IBEX" = "cadetblue", "CAC" = "cadetblue1", "FTSEMIB" = "cadetblue2", "PSI" = "cadetblue3")) +
  theme_minimal() +
  labs(x = "Índice", y = "SD", title = "Comparación de las Desviaciónes Estándar de los Índices")

ggplot(skewness_df, aes(x = Index, y = Value, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("IBEX" = "cadetblue", "CAC" = "cadetblue1", "FTSEMIB" = "cadetblue2", "PSI" = "cadetblue3")) +
  theme_minimal() +
  labs(x = "Índice", y = "Skewness", title = "Comparación de Skewness de los Índices")

ggplot(kurtosis_df, aes(x = Index, y = Value, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("IBEX" = "cadetblue", "CAC" = "cadetblue1", "FTSEMIB" = "cadetblue2", "PSI" = "cadetblue3")) +
  theme_minimal() +
  labs(x = "Índice", y = "Kurtosis", title = "Comparación de Kurtosis de los Índices")

ggplot(median_df, aes(x = Index, y = Value, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("IBEX" = "cadetblue", "CAC" = "cadetblue1", "FTSEMIB" = "cadetblue2", "PSI" = "cadetblue3")) +
  theme_minimal() +
  labs(x = "Índice", y = "Mediana", title = "Comparación de las Medianas de los Índices")

#kernel plots
mean_ret_IBEX <- mean(IBEX$return)
sd_ret_IBEX <- sd(IBEX$return)

ggplot(IBEX, aes(x = return)) + 
  geom_density(fill = "cadetblue", alpha = 0.5) + 
  stat_function(fun = dnorm, args = list(mean = mean_ret_IBEX, sd = sd_ret_IBEX), color = "red", size = 1) +
  labs(title = "Densidad Kernel de los Retornos para IBEX con Curva Normal", x = "Retorno", y = "Densidad") +
  theme_minimal()

mean_ret_CAC <- mean(CAC$return)
sd_ret_CAC <- sd(CAC$return)

ggplot(CAC, aes(x = return)) + 
  geom_density(fill = "cadetblue", alpha = 0.5) + 
  stat_function(fun = dnorm, args = list(mean = mean_ret_CAC, sd = sd_ret_CAC), color = "red", size = 1) +
  labs(title = "Densidad Kernel de los Retornos para CAC con Curva Normal", x = "Retorno", y = "Densidad") +
  theme_minimal()

mean_ret_FTSEMIB <- mean(FTSEMIB$return)
sd_ret_FTSEMIB <- sd(FTSEMIB$return)

ggplot(FTSEMIB, aes(x = return)) + 
  geom_density(fill = "cadetblue", alpha = 0.5) + 
  stat_function(fun = dnorm, args = list(mean = mean_ret_FTSEMIB, sd = sd_ret_FTSEMIB), color = "red", size = 1) +
  labs(title = "Densidad Kernel de los Retornos para FTSEMIB con Curva Normal", x = "Retorno", y = "Densidad") +
  theme_minimal()

mean_ret_PSI <- mean(PSI$return)
sd_ret_PSI <- sd(PSI$return)

ggplot(PSI, aes(x = return)) + 
  geom_density(fill = "cadetblue", alpha = 0.5) + 
  stat_function(fun = dnorm, args = list(mean = mean_ret_PSI, sd = sd_ret_PSI), color = "red", size = 1) +
  labs(title = "Densidad Kernel de los Retornos para PSI con Curva Normal", x = "Retorno", y = "Densidad") +
  theme_minimal()

#qqplots
qqnorm(IBEX$return,col='cadetblue',main='IBEX')
qqline(IBEX$return,lwd=2,lty=3)

qqnorm(CAC$return,col='cadetblue',main='CAC')
qqline(CAC$return,lwd=2,lty=3)

qqnorm(FTSEMIB$return,col='cadetblue',main='FTSEMIB')
qqline(FTSEMIB$return,lwd=2,lty=3)

qqnorm(PSI$return,col='cadetblue',main='PSI')
qqline(PSI$return,lwd=2,lty=3)

#Jarque-Bera test
jarque.test(IBEX$return)
jarque.test(CAC$return)
jarque.test(PSI$return)
jarque.test(FTSEMIB$return)

#análisis de la volatilidad 
#volatilidad diaria para cada índice
volatilidad_diaria_IBEX <- sd(IBEX$return, na.rm = TRUE)
volatilidad_diaria_CAC <- sd(CAC$return, na.rm = TRUE)
volatilidad_diaria_FTSEMIB <- sd(FTSEMIB$return, na.rm = TRUE)
volatilidad_diaria_PSI <- sd(PSI$return, na.rm = TRUE)

#volatilidad anualizada para cada índice
volatilidad_anualizada_IBEX <- volatilidad_diaria_IBEX * sqrt(252)
volatilidad_anualizada_CAC <- volatilidad_diaria_CAC * sqrt(252)
volatilidad_anualizada_FTSEMIB <- volatilidad_diaria_FTSEMIB * sqrt(252)
volatilidad_anualizada_PSI <- volatilidad_diaria_PSI * sqrt(252)

resultados_volatilidad <- list(
  IBEX = list(Volatilidad_Diaria = volatilidad_diaria_IBEX, Volatilidad_Anualizada = volatilidad_anualizada_IBEX),
  CAC = list(Volatilidad_Diaria = volatilidad_diaria_CAC, Volatilidad_Anualizada = volatilidad_anualizada_CAC),
  FTSEMIB = list(Volatilidad_Diaria = volatilidad_diaria_FTSEMIB, Volatilidad_Anualizada = volatilidad_anualizada_FTSEMIB),
  PSI = list(Volatilidad_Diaria = volatilidad_diaria_PSI, Volatilidad_Anualizada = volatilidad_anualizada_PSI)
)

print(resultados_volatilidad)

#volatilidad anualizada para 2020, 2021, 2022 y 2023 para cada índice
volatilidad_anual_IBEX <- IBEX %>%
  mutate(Year = year(Date)) %>%
  filter(Year >= 2020 & Year <= 2023) %>%
  group_by(Year) %>%
  summarise(Volatilidad_Anual = sd(return, na.rm = TRUE) * sqrt(252)) %>%
  ungroup()

print(volatilidad_anual_IBEX)


volatilidad_anual_CAC <- CAC %>%
  mutate(Year = year(Date)) %>%
  filter(Year >= 2020 & Year <= 2023) %>%
  group_by(Year) %>%
  summarise(Volatilidad_Anual = sd(return, na.rm = TRUE) * sqrt(252)) %>%
  ungroup()

print(volatilidad_anual_CAC)


volatilidad_anual_FTSEMIB <- FTSEMIB %>%
  mutate(Year = year(Date)) %>%
  filter(Year >= 2020 & Year <= 2023) %>%
  group_by(Year) %>%
  summarise(Volatilidad_Anual = sd(return, na.rm = TRUE) * sqrt(252)) %>%
  ungroup()

print(volatilidad_anual_FTSEMIB)


volatilidad_anual_PSI <- PSI %>%
  mutate(Year = year(Date)) %>%
  filter(Year >= 2020 & Year <= 2023) %>%
  group_by(Year) %>%
  summarise(Volatilidad_Anual = sd(return, na.rm = TRUE) * sqrt(252)) %>%
  ungroup()

print(volatilidad_anual_PSI)


#volatilidad móvil
ventana <- 30
IBEX$rolling_volatility <- rollapply(IBEX$return, width = ventana, FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252), by.column = TRUE, align = 'right', fill = NA)
CAC$rolling_volatility <- rollapply(CAC$return, width = ventana, FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252), by.column = TRUE, align = 'right', fill = NA)
FTSEMIB$rolling_volatility <- rollapply(FTSEMIB$return, width = ventana, FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252), by.column = TRUE, align = 'right', fill = NA)
PSI$rolling_volatility <- rollapply(PSI$return, width = ventana, FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252), by.column = TRUE, align = 'right', fill = NA)

ggplot(IBEX, aes(x = Date, y = rolling_volatility)) + 
  geom_line() + 
  geom_point(size = 0.5, alpha = 0.5) +  
  theme_minimal() +  
  labs(title = "Volatilidad Móvil Anualizada del IBEX",
       x = "Fecha",
       y = "Volatilidad Móvil Anualizada (%)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(CAC, aes(x = Date, y = rolling_volatility)) + 
  geom_line() + 
  geom_point(size = 0.5, alpha = 0.5) +  
  theme_minimal() +  
  labs(title = "Volatilidad Móvil Anualizada del CAC",
       x = "Fecha",
       y = "Volatilidad Móvil Anualizada (%)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(FTSEMIB, aes(x = Date, y = rolling_volatility)) + 
  geom_line() + 
  geom_point(size = 0.5, alpha = 0.5) +  
  theme_minimal() +  
  labs(title = "Volatilidad Móvil Anualizada del FTSE MIB",
       x = "Fecha",
       y = "Volatilidad Móvil Anualizada (%)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(PSI, aes(x = Date, y = rolling_volatility)) + 
  geom_line() + 
  geom_point(size = 0.5, alpha = 0.5) +  
  theme_minimal() +  
  labs(title = "Volatilidad Móvil Anualizada del PSI",
       x = "Fecha",
       y = "Volatilidad Móvil Anualizada (%)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



combined_df_vm <- bind_rows(
  IBEX %>% mutate(Index = "IBEX"),
  CAC %>% mutate(Index = "CAC"),
  FTSEMIB %>% mutate(Index = "FTSEMIB"),
  PSI %>% mutate(Index = "PSI"))

ggplot(combined_df_vm, aes(x = Date, y = rolling_volatility, color = Index)) + 
  geom_line() +  
  theme_minimal() + 
  labs(title = "Comparación de la Volatilidad Móvil Anualizada entre Índices",
       x = "Fecha",
       y = "Volatilidad Móvil Anualizada (%)") +
  scale_color_manual(values = c("IBEX" = "firebrick1", "CAC" = "cadetblue1", "FTSEMIB" = "springgreen4", "PSI" = "tan1"))

#añadiendo el VIX
VIX <- read_csv("TFG PHILIPP NIEDERSTRASSER (ESC06)/data/^VIX.csv", 
                col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                 Open = col_number(), High = col_number(), 
                                 Low = col_number(), Close = col_number(), 
                                 `Adj Close` = col_number(), Volume = col_integer()))
VIX$Volume <- NULL
names(VIX)[names(VIX) == "Price"] <- "rolling_volatility"
sum(!complete.cases(VIX))
VIX <- data.frame(lapply(VIX, function(column) {
  if(is.numeric(column)) {
    return(na.approx(column, na.rm = FALSE))} 
  else {
    return(column) }}))
sum(!complete.cases(VIX))

View(VIX)

VIX <- VIX %>% mutate(Index = "VIX")

combined_df_vmVIX <- bind_rows(
  IBEX %>% mutate(Index = "IBEX"),
  CAC %>% mutate(Index = "CAC"),
  FTSEMIB %>% mutate(Index = "FTSEMIB"),
  PSI %>% mutate(Index = "PSI"),
  VIX)

combined_df_vmVIX <- combined_df_vmVIX %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         AdjustedDate = if_else(Index != "VIX", Date - 15, Date))

ggplot(combined_df_vmVIX, aes(x = Date, y = rolling_volatility, color = Index)) + 
  geom_line() +  
  theme_minimal() + 
  labs(title = "Comparación de la Volatilidad Móvil Anualizada entre Índices y VIX",
       x = "Fecha",
       y = "Volatilidad Móvil Anualizada (%)") +
  scale_color_manual(values = c("IBEX" = "firebrick1", 
                                "CAC" = "cadetblue1", 
                                "FTSEMIB" = "springgreen4", 
                                "PSI" = "tan1",
                                "VIX" = "purple"))

#comparación volatilidad móvil FTSEMIB vs PSI20
vol_diff <- merge(FTSEMIB[, c("Date", "rolling_volatility")], PSI[, c("Date", "rolling_volatility")], by = "Date", suffixes = c("_FTSEMIB", "_PSI20"))
vol_diff$volatility_difference <- vol_diff$rolling_volatility_FTSEMIB - vol_diff$rolling_volatility_PSI20
head(vol_diff)

ggplot(vol_diff, aes(x = Date, y = volatility_difference)) + 
  geom_line(color = "cadetblue") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  
  theme_minimal() +  
  labs(title = "Diferencia en la Volatilidad Móvil Anualizada entre FTSEMIB y PSI20",
       x = "Fecha",
       y = "Diferencia de Volatilidad Móvil (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

vol_diff$volatility_ratio <- vol_diff$rolling_volatility_FTSEMIB / vol_diff$rolling_volatility_PSI20

ggplot(vol_diff, aes(x = Date, y = volatility_ratio)) + 
  geom_line(color = "cadetblue") +  
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  
  theme_minimal() +  
  labs(title = "Ratio de la Volatilidad Móvil Anualizada entre FTSEMIB y PSI20",
       x = "Fecha",
       y = "Ratio de Volatilidad Móvil") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#análisis ACF/PACF
acf(IBEX$return, na.action = na.pass, main = "ACF para IBEX")
pacf(IBEX$return, na.action = na.pass, main = "PACF para IBEX")
acf(CAC$return, na.action = na.pass, main = "ACF para CAC")
pacf(CAC$return, na.action = na.pass, main = "PACF para CAC")
acf(FTSEMIB$return, na.action = na.pass, main = "ACF para FTSEMIB")
pacf(FTSEMIB$return, na.action = na.pass, main = "PACF para FTSEMIB")
acf(PSI$return, na.action = na.pass, main = "ACF para PSI")
pacf(PSI$return, na.action = na.pass, main = "PACF para PSI")

acf(IBEX$return^2, na.action = na.pass, main = "ACF SQ para IBEX")
pacf(IBEX$return^2, na.action = na.pass, main = "PACF SQ para IBEX")
acf(CAC$return^2, na.action = na.pass, main = "ACF SQ para CAC")
pacf(CAC$return^2, na.action = na.pass, main = "PACF SQ para CAC")
acf(FTSEMIB$return^2, na.action = na.pass, main = "ACF SQ para FTSEMIB")
pacf(FTSEMIB$return^2, na.action = na.pass, main = "PACF SQ para FTSEMIB")
acf(PSI$return^2, na.action = na.pass, main = "ACF SQ para PSI")
pacf(PSI$return^2, na.action = na.pass, main = "PACF SQ para PSI")

#Ljung-Box Test
Box.test(IBEX$return, lag = 20, type = "Ljung-Box")
Box.test(CAC$return, lag = 20, type = "Ljung-Box")
Box.test(FTSEMIB$return, lag = 20, type = "Ljung-Box")
Box.test(PSI$return, lag = 20, type = "Ljung-Box") 

#Modeling & Forecasting
cutoff_index <- floor(nrow(IBEX) * 0.8)
IBEX_in_sample <- IBEX[1:cutoff_index, ]
IBEX_out_of_sample <- IBEX[(cutoff_index + 1):nrow(IBEX), ]
str(IBEX_in_sample)
str(IBEX_out_of_sample)
returnsISIBEX_xts <- xts(IBEX_in_sample$return, order.by = IBEX_in_sample$Date)
specIBEX <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                       distribution.model = "norm")
fitIBEX <- ugarchfit(spec = specIBEX, data = returnsISIBEX_xts)
show(fitIBEX)



cutoff_index_CAC <- floor(nrow(CAC) * 0.8)
CAC_in_sample <- CAC[1:cutoff_index_CAC, ]
CAC_out_of_sample <- CAC[(cutoff_index_CAC + 1):nrow(CAC), ]
returnsISCAC_xts <- xts(CAC_in_sample$return, order.by = CAC_in_sample$Date)
specCAC <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                      distribution.model = "norm")
fitCAC <- ugarchfit(spec = specCAC, data = returnsISCAC_xts)
show(fitCAC)



cutoff_index_FTSEMIB <- floor(nrow(FTSEMIB) * 0.8)
FTSEMIB_in_sample <- FTSEMIB[1:cutoff_index_FTSEMIB, ]
FTSEMIB_out_of_sample <- FTSEMIB[(cutoff_index_FTSEMIB + 1):nrow(FTSEMIB), ]
returnsISFTSEMIB_xts <- xts(FTSEMIB_in_sample$return, order.by = FTSEMIB_in_sample$Date)
specFTSEMIB <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                          distribution.model = "norm")
fitFTSEMIB <- ugarchfit(spec = specFTSEMIB, data = returnsISFTSEMIB_xts)
show(fitFTSEMIB)



cutoff_index_PSI <- floor(nrow(PSI) * 0.8)
PSI_in_sample <- PSI[1:cutoff_index_PSI, ]
PSI_out_of_sample <- PSI[(cutoff_index_PSI + 1):nrow(PSI), ]
returnsISPSI_xts <- xts(PSI_in_sample$return, order.by = PSI_in_sample$Date)
specPSI <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                      mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                      distribution.model = "norm")
fitPSI <- ugarchfit(spec = specPSI, data = returnsISPSI_xts)
show(fitPSI)



specTARCH_IBEX <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                             mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                             distribution.model = "norm")
fitTARCH_IBEX <- ugarchfit(spec = specTARCH_IBEX, data = returnsISIBEX_xts)
show(fitTARCH_IBEX)



specTARCH_CAC <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                            distribution.model = "norm")
fitTARCH_CAC <- ugarchfit(spec = specTARCH_CAC, data = returnsISCAC_xts)
show(fitTARCH_CAC)



specTARCH_FTSEMIB <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                                mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                                distribution.model = "norm")
fitTARCH_FTSEMIB <- ugarchfit(spec = specTARCH_FTSEMIB, data = returnsISFTSEMIB_xts)
show(fitTARCH_FTSEMIB)



specTARCH_PSI <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                            distribution.model = "norm")
fitTARCH_PSI <- ugarchfit(spec = specTARCH_PSI, data = returnsISPSI_xts)
show(fitTARCH_PSI)



plot(fitIBEX, which = 12)
plot(fitTARCH_IBEX, which = 12)

residuos_estandarizadosTARCHIBEX <- residuals(fitTARCH_IBEX, standardize = TRUE)
fechasTARCHIBEX <- index(returnsISIBEX_xts)[1:length(residuos_estandarizadosTARCHIBEX)]
df_residuos_estandarizadosTARCHIBEX <- data.frame(Fecha = fechasTARCHIBEX, Residuos = residuos_estandarizadosTARCHIBEX)

ggplot(df_residuos_estandarizadosTARCHIBEX, aes(x = Fecha, y = Residuos)) +
  geom_line() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  labs(title = "Residuos Estandarizados del Modelo TARCH para IBEX",
       x = "Fecha", y = "Residuos Estandarizados")

residuos_estandarizadosTARCHCAC <- residuals(fitTARCH_CAC, standardize = TRUE)
fechasTARCHCAC <- index(returnsISCAC_xts)[1:length(residuos_estandarizadosTARCHCAC)]
df_residuos_estandarizadosTARCHCAC <- data.frame(Fecha = fechasTARCHCAC, Residuos = residuos_estandarizadosTARCHCAC)

ggplot(df_residuos_estandarizadosTARCHCAC, aes(x = Fecha, y = Residuos)) +
  geom_line() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Residuos Estandarizados del Modelo TARCH para CAC",
       x = "Fecha", y = "Residuos Estandarizados")

residuos_estandarizadosTARCHFTSEMIB <- residuals(fitTARCH_FTSEMIB, standardize = TRUE)
fechasTARCHFTSEMIB <- index(returnsISFTSEMIB_xts)[1:length(residuos_estandarizadosTARCHFTSEMIB)]
df_residuos_estandarizadosTARCHFTSEMIB <- data.frame(Fecha = fechasTARCHFTSEMIB, Residuos = residuos_estandarizadosTARCHFTSEMIB)

ggplot(df_residuos_estandarizadosTARCHFTSEMIB, aes(x = Fecha, y = Residuos)) +
  geom_line() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Residuos Estandarizados del Modelo TARCH para FTSEMIB",
       x = "Fecha", y = "Residuos Estandarizados")

residuos_estandarizadosTARCHPSI <- residuals(fitTARCH_PSI, standardize = TRUE)
fechasTARCHPSI <- index(returnsISPSI_xts)[1:length(residuos_estandarizadosTARCHPSI)]
df_residuos_estandarizadosTARCHPSI <- data.frame(Fecha = fechasTARCHPSI, Residuos = residuos_estandarizadosTARCHPSI)

ggplot(df_residuos_estandarizadosTARCHPSI, aes(x = Fecha, y = Residuos)) +
  geom_line() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Residuos Estandarizados del Modelo TARCH para PSI",
       x = "Fecha", y = "Residuos Estandarizados")


plot(fitTARCH_IBEX, which = 8)
plot(fitTARCH_CAC, which = 8)
plot(fitTARCH_FTSEMIB, which = 8)
plot(fitTARCH_PSI, which = 8)

plot(fitTARCH_IBEX, which = 9)
plot(fitTARCH_CAC, which = 9)
plot(fitTARCH_FTSEMIB, which = 9)
plot(fitTARCH_PSI, which = 9)

jarque.bera.test(residuos_estandarizadosTARCHIBEX)
jarque.bera.test(residuos_estandarizadosTARCHCAC)
jarque.bera.test(residuos_estandarizadosTARCHFTSEMIB)
jarque.bera.test(residuos_estandarizadosTARCHPSI)

jarque.test(IBEX$return)
jarque.test(CAC$return)
jarque.test(FTSEMIB$return)
jarque.test(PSI$return)

plot(fitTARCH_IBEX, which = 11)
plot(fitTARCH_CAC, which = 11)
plot(fitTARCH_FTSEMIB, which = 11)
plot(fitTARCH_PSI, which = 11)

Box.test(residuos_estandarizadosTARCHIBEX^2, lag = 20, type = "Ljung-Box")
Box.test(residuos_estandarizadosTARCHCAC^2, lag = 20, type = "Ljung-Box")
Box.test(residuos_estandarizadosTARCHFTSEMIB^2, lag = 20, type = "Ljung-Box")
Box.test(residuos_estandarizadosTARCHPSI^2, lag = 20, type = "Ljung-Box")

Box.test(IBEX$return^2, lag = 20, type = "Ljung-Box")
Box.test(CAC$return^2, lag = 20, type = "Ljung-Box")
Box.test(FTSEMIB$return^2, lag = 20, type = "Ljung-Box")
Box.test(PSI$return^2, lag = 20, type = "Ljung-Box") 

plot(fitTARCH_IBEX, which = 1)
plot(fitTARCH_CAC, which = 1)
plot(fitTARCH_FTSEMIB, which = 1)
plot(fitTARCH_PSI, which = 1)

plot(fitTARCH_IBEX, which = 3)
plot(fitTARCH_CAC, which = 3)
plot(fitTARCH_FTSEMIB, which = 3)
plot(fitTARCH_PSI, which = 3)


#for-loop IBEX35
H <- nrow(IBEX_out_of_sample) 
N <- nrow(IBEX) - H 

volatility_forecasts_OOS <- numeric(H)

specTARCH <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

for(m in 0:(H)){
  current_data <- IBEX[1:(N+m), "return"]
  
  fitTARCH <- ugarchfit(spec = specTARCH, data = current_data)
  
  forecast_result <- ugarchforecast(fitTARCH, n.ahead = 1)
  volatility_forecasts_OOS[m] <- forecast_result@forecast$sigmaFor[1]
}

volatility_forecasts_OOS


IBEX$return_volatility10 <- rollapply(IBEX$return, width = 10, FUN = sd, align = "right", fill = NA)
IBEX$return_volatility10an <- IBEX$return_volatility10 * sqrt(252)
selected_values <- IBEX$return_volatility10an[1020:1028]

IBEX$return_volatility <- rollapply(IBEX$return, width = 2, FUN = sd, align = "right", fill = NA)
IBEX$return_volatilityan <- IBEX$return_volatility * sqrt(252)
selected_values1 <- IBEX$return_volatilityan[1020]

IBEX_out_of_sample$return_volatility <- rollapply(IBEX_out_of_sample$return, width = 2, FUN = sd, align = "right", fill = NA)
IBEX_out_of_sample$return_volatility10 <- rollapply(IBEX_out_of_sample$return, width = 10, FUN = sd, align = "right", fill = NA)
IBEX_out_of_sample$return_volatility30 <- rollapply(IBEX_out_of_sample$return, width = 30, FUN = sd, align = "right", fill = NA)
IBEX_out_of_sample$volatility_forecasts <- volatility_forecasts_OOS
IBEX_out_of_sample$return_volatilityan <- IBEX_out_of_sample$return_volatility * sqrt(252)
IBEX_out_of_sample$return_volatility10an <- IBEX_out_of_sample$return_volatility10 * sqrt(252)
IBEX_out_of_sample$volatility_forecastsan <-IBEX_out_of_sample$volatility_forecasts * sqrt(252)
IBEX_out_of_sample$return_volatility10an[1:9] <- selected_values
IBEX_out_of_sample$return_volatilityan[1] <- selected_values1

ggplot(IBEX_out_of_sample, aes(x = Date)) +
  geom_line(aes(y = return_volatility10an), color = "cadetblue", linetype = "solid", size = 0.8) +
  geom_line(aes(y = volatility_forecastsan), color = "red", linetype = "dashed", size = 0.6) +
  labs(title = "Comparación de Volatilidad Real vs Pronosticada",
       y = "Volatilidad Anualizada",
       color = "Tipo") +
  scale_color_manual(values = c("cadetblue", "red")) +
  theme_minimal() +
  theme(legend.position = "top",  
        axis.text.x = element_text(angle = 45, hjust = 1))  


errors <- IBEX_out_of_sample$volatility_forecastsan - IBEX_out_of_sample$return_volatility10an
RMSE <- sqrt(mean(errors^2))
print(paste("RMSE:", round(RMSE, 4)))

SST <- sum((IBEX_out_of_sample$return_volatility10an - mean(IBEX_out_of_sample$return_volatility10an))^2)
SSE <- sum(errors^2)
R_squared <- 1 - SSE / SST
print(paste("R-squared:", round(R_squared, 4)))

ggplot(IBEX_out_of_sample, aes(x = Date)) +
  geom_line(aes(y = return_volatilityan), color = "cadetblue", linetype = "solid", size = 0.8) +
  geom_line(aes(y = volatility_forecastsan), color = "red", linetype = "dashed", size = 0.6) +
  labs(title = "Comparación de Volatilidad Real vs Pronosticada",
       y = "Volatilidad Anualizada",
       color = "Tipo") +
  scale_color_manual(values = c("cadetblue", "red")) +
  theme_minimal() +
  theme(legend.position = "top",  
        axis.text.x = element_text(angle = 45, hjust = 1)) 

errors1 <- IBEX_out_of_sample$volatility_forecastsan - IBEX_out_of_sample$return_volatilityan
RMSE1 <- sqrt(mean(errors1^2))
print(paste("RMSE1:", round(RMSE1, 4)))

SST1 <- sum((IBEX_out_of_sample$return_volatilityan - mean(IBEX_out_of_sample$return_volatilityan))^2)
SSE1 <- sum(errors1^2)
R_squared1 <- 1 - SSE1 / SST1
print(paste("R-squared1:", round(R_squared1, 4)))