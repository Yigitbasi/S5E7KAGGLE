# Veriyi okuma
traindata<-read.csv("C:\\train.csv")
testdata<-read.csv("C:\\test.csv")

# Veriyi hazırlama (Eksik değerleri doldurma, aykırı değerleri ele alma vb.)
trainid<-traindata$id
testid<-testdata$id #test id'lerini de kaydet

# Hedef değişkeni (Personality) ayırma
target <- traindata$Personality
traindata <- traindata[, !names(traindata) %in% c("id", "Personality", "Drained_after_socializing")] # Hedef ve id hariç tut
testdata <- testdata[, !names(testdata) %in% c("id", "Drained_after_socializing")] #test datasından da id  hariç tut. Personality yok çünkü tahmin edeceğiz.

# Birleştirme
combined_data <- rbind(traindata, testdata)

# Sayısal ve kategorik değişkenleri ayırma
numeric_vars <- names(combined_data)[sapply(combined_data, is.numeric)]
categorical_vars <- names(combined_data)[sapply(combined_data, is.character) | sapply(combined_data, is.factor)] #Hem karakter hem de factor tipindekiler

#Kategorik değişkenleri faktöre dönüştür
for (var in categorical_vars) {
  combined_data[[var]] <- as.factor(combined_data[[var]])
}


# Eksik değerleri doldur
for (var in numeric_vars){
  combined_data[[var]][is.na(combined_data[[var]])]<-mean(combined_data[[var]], na.rm = TRUE)
}

for (var in categorical_vars) {
  mode_val<-names(sort(table(combined_data[[var]]),decreasing = TRUE))[1]
  combined_data[[var]][is.na(combined_data[var])]<-mode_val
}

#Aykırı değer kontrolü ve düzeltme (Tek değişken için örnek)
outliertimespent<-boxplot.stats(combined_data$Time_spent_Alone)$out
medianimespent<-median(combined_data$Time_spent_Alone)
combined_data$Time_spent_Alone[combined_data$Time_spent_Alone %in% outliertimespent] <- medianimespent

# Kategorik değişkenleri sayısallaştırma (One-Hot Encoding)
library(caret)
#dummyVars'ı tüm combined_data üzerinde eğit
dmy <- dummyVars(" ~ .", data = combined_data, fullRank = TRUE)

#predict fonksiyonunu tüm combined_data üzerinde çalıştır
combined_encoded <- data.frame(predict(dmy, newdata = combined_data))

#combined_data'yı encoded data ile değiştir
combined_data <- combined_encoded


# Veriyi ölçeklendirme
preprocessParams <- preProcess(combined_data, method=c("center", "scale"))
combined_scaled <- predict(preprocessParams, combined_data)

# Veriyi train ve test olarak ayırma
train_scaled <- combined_scaled[1:nrow(traindata),]
test_scaled <- combined_scaled[(nrow(traindata)+1):nrow(combined_data),]

# Hedef değişkeni (Personality) sayısal hale getir (Gerekli değil, kategorik kalabilir)
library(nnet)
target <- as.factor(target) #Hedefi faktöre dönüştür

model <- multinom(target ~ ., data = as.data.frame(train_scaled))

# Tahmin yap
predictions <- predict(model, newdata = as.data.frame(test_scaled), type = "class")

# Sonuçları kaydetme
submission <- data.frame(id = testid, Personality = predictions)
write.csv(submission, "submission.csv", row.names = FALSE)

print("Submission dosyası oluşturuldu.")
