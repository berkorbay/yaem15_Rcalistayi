aci = c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5)
ilac = c(rep("A",9), rep("B",9), rep("C",9))
migren = data.frame(aci,ilac)

migren
str(migren)

plot(aci ~ ilac, data=migren)

sonuc= aov(aci ~ ilac, data=migren)

sonuc
summary(sonuc)

#çoklu karşılaştırma 1
pairwise.t.test(aci, ilac, p.adjust="bonferroni")

#çoklu karşılaştırma 2
TukeyHSD(sonuc, conf.level = 0.95)

#Two-way ANOVA
#Paket yüklü değilse önce yüklemek gerekir
install.packages("xlsx")
#Sonra da çağırmak gereklidir
require(xlsx)

#çalışma klasörünü tanımlıyoruz
setwd("C:/mustafa/tutorial/R")
veri=read.xlsx("dataset_anova_twoWay.xlsx", sheetName = "sayfa1")

#Stres azalmasında yaş grubu ve tedavi çeşidi ve bunların etkileşimleri etkili midir?
sonuc=anova(lm(StresAzligi ~ Tedavi * Yas	, veri))