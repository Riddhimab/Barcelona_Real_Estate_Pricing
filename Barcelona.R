library(readxl)
library(ggplot2)
library(dplyr)

data<- read_excel(file.choose(), sheet = 3)

#####################################

# Create dummy variables for the city zones 
Sant_Andreu <- ifelse(data$`City Zone` == "Sant Andreu", 1, 0)
Sarria_Sant_Gervasi <- ifelse(data$`City Zone` == "Sarria - Sant Gervasi", 1, 0)
Eixample <- ifelse(data$`City Zone` == "Eixample", 1, 0)
Horta_Guinardó <- ifelse(data$`City Zone` == "Horta - Guinardó", 1, 0)
Gràcia <- ifelse(data$`City Zone` == "Gràcia", 1, 0)
Sants_Montjuïc <- ifelse(data$`City Zone` == "Sants - Montjuïc", 1, 0)
Ciutat_Vella <- ifelse(data$`City Zone` == "Ciutat Vella", 1, 0)
Nou_Barris <- ifelse(data$`City Zone` == "Nou Barris", 1, 0)
Sant_Marti <- ifelse(data$`City Zone` == "Sant Marti", 1, 0)

###adding dummy values to the data

data <- data.frame(data, "SA" = Sant_Andreu, "SSG" = Sarria_Sant_Gervasi, "E" = Eixample, 
                   "HG" = Horta_Guinardó, "G" = Gràcia,  "SM" = Sants_Montjuïc, 
                   "CV" = Ciutat_Vella, "NB" =Nou_Barris, "SM2" =Sant_Marti)

model10 <- lm(Price ~ m.2 + Bathrooms + X.Atico. + Parking + Kitchen + Yard + SA + SSG + E + HG + G + SM + CV + NB + SM2, data = data)
summary(model10)
hist(residuals(model10), breaks = 20)

data2<-read_excel(file.choose(), sheet = 4)
Sant_Andreu <- ifelse(data2$`City Zone` == "Sant Andreu", 1, 0)
Sarria_Sant_Gervasi <- ifelse(data2$`City Zone` == "Sarria - Sant Gervasi", 1, 0)
Eixample <- ifelse(data2$`City Zone` == "Eixample", 1, 0)
Horta_Guinardó <- ifelse(data2$`City Zone` == "Horta - Guinardó", 1, 0)
Gràcia <- ifelse(data2$`City Zone` == "Gràcia", 1, 0)
Sants_Montjuïc <- ifelse(data2$`City Zone` == "Sants - Montjuïc", 1, 0)
Ciutat_Vella <- ifelse(data2$`City Zone` == "Ciutat Vella", 1, 0)
Nou_Barris <- ifelse(data2$`City Zone` == "Nou Barris", 1, 0)
Sant_Marti <- ifelse(data2$`City Zone` == "Sant Marti", 1, 0)
data2 <- data.frame(data2, "SA" = Sant_Andreu, "SSG" = Sarria_Sant_Gervasi, "E" = Eixample, 
                   "HG" = Horta_Guinardó, "G" = Gràcia,  "SM" = Sants_Montjuïc, 
                   "CV" = Ciutat_Vella, "NB" =Nou_Barris, "SM2" =Sant_Marti)


forecast <- predict(model10, newdata = data2, interval = "prediction")
data3<-data.frame(data2$Price,forecast)
plot(fitted.values(model10), residuals(model10))
qplot(data3$fit,data3$data2.Price)
data3<-data.frame(data3,((data3$data2.Price-data3$fit)/data3$data2.Price))

mean_r<-mean(data3$data2.Price-data3$fit)
r2_r<-1-(sum((data3$data2.Price-data3$fit)^2)/sum((data3$data2.Price-mean_r)^2))
r2_r
##sum of squared errors
anova(model10)
aov(model10)

plot(fitted.values(model10), residuals(model10))
plot(data$Price, residuals(model10))

##choosing the Predictions file
data_final<-read_excel(file.choose(), sheet = 1)
m.2<-data_final$`m^2`
X.Atico.<-data_final$`"Atico"`

data_final<-data.frame(data_final,m.2,X.Atico.)
Sant_Andreu <- ifelse(data_final$City.Zone== "Sant Andreu", 1, 0)
Sarria_Sant_Gervasi <- ifelse(data_final$City.Zone == "Sarria - Sant Gervasi", 1, 0)
Eixample <- ifelse(data_final$City.Zone == "Eixample", 1, 0)
Horta_Guinardó <- ifelse(data_final$City.Zone == "Horta - Guinardó", 1, 0)
Gràcia <- ifelse(data_final$City.Zone == "Gràcia", 1, 0)
Sants_Montjuïc <- ifelse(data_final$City.Zone== "Sants - Montjuïc", 1, 0)
Ciutat_Vella <- ifelse(data_final$City.Zone == "Ciutat Vella", 1, 0)
Nou_Barris <- ifelse(data_final$City.Zone == "Nou Barris", 1, 0)
Sant_Marti <- ifelse(data_final$City.Zone == "Sant Marti", 1, 0)
data_final <- data.frame(data_final, "SA" = Sant_Andreu, "SSG" = Sarria_Sant_Gervasi, "E" = Eixample, 
                    "HG" = Horta_Guinardó, "G" = Gràcia,  "SM" = Sants_Montjuïc, 
                    "CV" = Ciutat_Vella, "NB" =Nou_Barris, "SM2" =Sant_Marti)
forecast1 <- predict(model10, newdata = data_final, interval = "prediction")

forecast1
