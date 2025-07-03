Veri Yükleme: Eğitim ve test veri kümelerini okur.
Veri Ön İşleme:
Eksik değerleri, sayısal özellikler için ortalama ve kategorik özellikler için mod ile doldurarak işler.
Time_spent_Alone özelliğindeki aykırı değerleri tespit eder ve düzeltir.
Kategorik değişkenleri, one-hot encoding kullanarak sayısal forma dönüştürür.
Sayısal özellikleri, merkezleme ve ölçekleme kullanarak ölçeklendirir.
Model Eğitimi: nnet paketi kullanılarak çok terimli lojistik regresyon modeli eğitilir.
Tahmin: Test veri kümesi için kişilik tiplerini tahmin eder.
Gönderim Dosyası Oluşturma: Tahmin edilen kişilik tiplerini içeren bir gönderim dosyası (submission.csv) oluşturur.

nnet kütüphanesinden multinomiyal lojistik regresyon kullanılmıştır.
