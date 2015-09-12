#install.packages("MASS")
require(MASS)
set.seed(11)
#3 farklı parametre ile iki boyutlu bir veri yaratalım
ortalama=c(2,5)
sapma=matrix(c(3,2,2,2),nrow=2)
kume1=mvrnorm(n = 100, ortalama, sapma)
ortalama=c(10,0)
kume2=mvrnorm(n = 100, ortalama, sapma)
ortalama=c(-1,10)
sapma=matrix(c(6,4,4,6),nrow=2)
kume3=mvrnorm(n = 100, ortalama, sapma)

#verileri birleştir
veri=rbind(kume1,kume2,kume3)
#küme idlerini oluştur
verikume=rep(c(1:3),rep(100,3))
#çiz 
plot(veri,col=verikume)

#kumele
kumele=kmeans(veri,3)

#yapısına bak
str(kumele)

#sonucunu çizdir
plot(veri,col=kumele$cluster)