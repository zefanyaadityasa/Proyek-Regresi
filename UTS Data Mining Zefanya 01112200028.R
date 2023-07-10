library(dplyr);library(lmtest);library(graphics);library(corrplot);
library(mlrpro); library(My.stepwise); library(readxl);library(utils);
library(misty); library(corrplot); library(My.stepwise);
library(car); library(naniar);library(linea);library(Metrics)
library(readxl)
data = read_xlsx("/Users/CATUR/Documents/Bahan Belajar/Kuliah 2023/Data Mining dan Analisis Prediktif/UTS/autompg/auto-mpg-excel.xlsx")
dim(data)
data_mentah = data[-c(9)]
data_mentah = data_mentah[-c(7)]
data_mentah$horsepower = as.integer(data_mentah$horsepower)
mcar_test(data_mentah)
data_mentah$horsepower = impute_mean(data_mentah$horsepower)
variabel_dummy <- model.matrix(~ factor(data$origin) - 1, data = data_mentah)
colnames(variabel_dummy) <- c("variabelsatu", "variabeldua", "variabeltiga")
data_mentah = data_mentah[-c(7)]
data_mentah = cbind(data_mentah, variabel_dummy)
boxplot(data_mentah$acceleration)
corrplot(cor(data_mentah), method = c("number"))
My.stepwise.lm(Y = "mpg", variable.list = c("mpg", "cylinders", "displacement", "horsepower",
                                            "weight", "acceleration", "variabelsatu", "variabeldua",
                                            "variabeltiga"), data = data_mentah)
model = lm(data_mentah$mpg ~ data_mentah$horsepower + data_mentah$weight + data_mentah$variabeldua + data_mentah$variabeltiga)
summary1 = summary(model)

data_mentah$mpg = as.double(data_mentah$mpg)
data_terpakai = cbind(data_mentah$mpg,data_mentah$horsepower, data_mentah$weight, data_mentah$variabeldua, data_mentah$variabeltiga)
data_terpakai = as.data.frame(data_terpakai)
horsepower = sqrt(data_terpakai$V2);weight = as.double(sqrt(data_terpakai$V3));
variabeldua = sqrt(data_terpakai$V4);variabeltiga = sqrt(data_terpakai$V5);
mpg = data_terpakai$V1
data_tertransformasi = data.frame(mpg,horsepower,weight,variabeldua,variabeltiga)
data_tertransformasi = as.data.frame(data_tertransformasi)
m2 = lm(data_tertransformasi$mpg ~ data_tertransformasi$horsepower + data_tertransformasi$weight
        + data_tertransformasi$variabeldua + data_tertransformasi$variabeltiga)
ANOVA2 = anova(m2)

summary2 = summary(m2)

ANOVA = anova(m2)
ANOVA$`Sum Sq`[4]
hist(model[["residuals"]])
ks.test(m2[["residuals"]],"pnorm",mean(m2[["residuals"]]), sd(m2[["residuals"]]))

durbinWatsonTest(m2)

vif(m2)

bptest(m2, studentize = FALSE)

plot(model$fitted.values, model$residuals,main = "Analisis Residu",
     xlab = "Fitted values", ylab = "Residuals")
ANOVA1 = anova(model)
ANOVA
ANOVA$`Sum Sq`[4]
hist(model[["residuals"]])

regresi = function(horsepower, weight, variabeldua, variabeltiga){
  for(i in 1:length(horsepower))
    m2 <- lm(data_tertransformasi$mpg ~ data_tertransformasi$horsepower + data_tertransformasi$weight + data_tertransformasi$variabeldua + data_tertransformasi$variabeltiga)
  prediksi = coef(m2)[1] + coef(m2)[2]*horsepower + coef(m2)[3]*weight + coef(m2)[4]*variabeldua + coef(m2)[5]*variabeltiga
  return(prediksi)
}

regresi(12,41,1,0)
regresi(12,40,0,0)

predicted1 = model[["fitted.values"]]
predicted2 = m2[["fitted.values"]]
baris_pertama = paste("Didapatkan R kuadrat dari model pertama adalah:",summary1[["adj.r.squared"]])
baris_kedua = paste("Didapatkan R kuadrat dari model final adalah:", summary2[["adj.r.squared"]])
baris_ketiga = paste("didapatkan MSE dari model pertama adalah:", ANOVA1$`Mean Sq`[5])
baris_keempat = paste("didapatkan MSE dari model final adalah:", ANOVA2$`Mean Sq`[5])
baris_kelima = paste("didapatkan MAPE dari model pertama adalah:", mape(data$mpg, predicted1))
baris_keenam = paste("didapatkan MAPE dari model final adalah", mape(data$mpg, predicted2))
baris_ketujuh = paste("didapatkan RMSE dari model pertama adalah", rmse(data$mpg,predicted1))
baris_kedelapan = paste("didapatkan RMSE dari model final adalah:", rmse(data$mpg, predicted2))
tabel = rbind(baris_pertama,baris_kedua,baris_ketiga,baris_keempat,baris_kelima, baris_keenam, baris_ketujuh,baris_kedelapan)
tabel


run_model(data = data_tertransformasi, dv = 'mpg',ivs = c('horsepower','weight','variabeldua','variabeltiga')) %>% fit_chart()

boxplot(data_mentah$displacement);boxplot(data_mentah$horsepower);boxplot(data_mentah$weight);boxplot(data_mentah$acceleration)
