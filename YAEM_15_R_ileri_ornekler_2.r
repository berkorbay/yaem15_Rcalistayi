#Bu kodlar YAEM_15_R_ileri_ornekler.r dosyasının devamıdır

#Aşağıda basit bir doğrusal programlama problemini çözeceğiz
#Giapetto Problemi için http://faculty.kutztown.edu/vasko/MAT121/MAT121web/Example_2.html adresine bakabilirsiniz.

install.packages("lpSolve")

library(lpSolve)

amac_fonk<-c(3,2)

kisitlar<-matrix(c(2,1,
	 			   1,1,
				   1,0),nrow=3,byrow=TRUE)

isaretler<-c("<=",
			 "<=",
			 "<=")

sag_taraf<-c(100,
			  80,
			  40)

cozum<-lp("max",amac_fonk,kisitlar,isaretler,sag_taraf,compute.sens=TRUE)

print(paste0("Amaç fonksiyonu değeri: ",cozum$objval))
print(paste0("Çözüm: ",paste0("X",1:length(cozum$solution)," = ",cozum$solution,collapse=" ")))
print(paste0("Duyarlılık Alt Limit: ",paste0("X",1:length(cozum$sens.coef.from)," = ",cozum$sens.coef.from,collapse=" ")))
print(paste0("Duyarlılık Üst Limit: ",paste0("X",1:length(cozum$sens.coef.to)," = ",cozum$sens.coef.to,collapse=" ")))
print(paste0("Dual: ",paste0("Y",1:length(cozum$duals)," = ",cozum$duals,collapse=" ")))


#######

#######
## optim() ile Gezgin Satıcı Problemi'ni (Traveling Salesman Problem - TSP) Benzetim Tavlaması Yöntemi ile Çözmek
## Orijinali R yardım dosyasında bulunmaktadır ?optim yazarak probleme ulaşabilirsiniz
#######


#Eurodist veri paketi 21 Avrupa şehrinin birbirlerine olan yol uzaklıklarını km cinsinden verir verir
# R'ın içinde halihazırda yüklenmiş olarak gelmektedir
eurodistmat <- as.matrix(eurodist) 


uzaklik <- function(sq) {  # Amaç fonksiyonu
    sq2 <- embed(sq, 2) #İkili ilişkilere çevir 1-2, 15-17 gibi
    sum(eurodistmat[cbind(sq2[,2], sq2[,1])])  #Uzaklığı hesapla
}

rota_degisikligi <- function(sq) {  # Yeni bir kombinasyon hesapla
    idx <- seq(2, nrow(eurodistmat)-1) #vektörün içerisindeki pozisyon sayısını endeks olarak tut
    changepoints <- sample(idx, size = 2, replace = FALSE) #değiştirilecek iki şehri rastgele seç
    tmp <- sq[changepoints[1]] #yerlerini değiştir
    sq[changepoints[1]] <- sq[changepoints[2]]
    sq[changepoints[2]] <- tmp
    sq #yeni rotayı döndür
}

sq <- c(1:nrow(eurodistmat), 1)  # ilk rota normal sırada
uzaklik(sq) 

# rotate for conventional orientation
loc <- -cmdscale(eurodist, add = TRUE)$points #grafik için noktaların yerlerini koordinat haline getir
x <- loc[,1]; y <- loc[,2] #x ve y koordinatlarını vektörlere koy
s <- seq_len(nrow(eurodistmat)) #1'den şehir sayısına göre çıkar
tspinit <- loc[sq,] #okların pozisyonlarını ayarla

plot(x, y, type = "n", asp = 1, xlab = "", ylab = "", #ilk rotanın grafiği yazdır
     main = "initial solution of traveling salesman problem", axes = FALSE)
arrows(tspinit[s,1], tspinit[s,2], tspinit[s+1,1], tspinit[s+1,2], #okları çizdir
       angle = 10, col = "green")
text(x, y, labels(eurodist), cex = 0.8) #şehir isimlerini ekle


set.seed(123) # deneyi sabitlemek için bir rassallık tohumu belirleniyor
res <- optim(par=sq,fn=uzaklik,gr=rota_degisikligi, method = "SANN", #metod benzetim tavlaması (simulated annealing)
             control = list(maxit = 30000, temp = 2000, trace = TRUE,
                            REPORT = 500))

print(paste0("Sonuç uzaklık değeri: ",res$value))  # Optimale en yakın uzaklık 12842
print(paste0("Sonuç Rota: ",paste0(res$par,collapse="-")))  # Sonuç rotası


tspres <- loc[res$par,] #nihai sonucun grafiğini çizdir
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "",
     main = "optim() 'solving' traveling salesman problem", axes = FALSE)
arrows(tspres[s,1], tspres[s,2], tspres[s+1,1], tspres[s+1,2],
       angle = 10, col = "red")
text(x, y, labels(eurodist), cex = 0.8)


########
### K-Ortalamalar Yöntemi ile Kümeleme ve Grafik Gösterim
########

set.seed(4321)
kume_nesne<-kmeans(iris[,-5],centers=3,iter.max=10^9)

benzerlik_tablosu<-table(iris$Species,kume_nesne$cluster)
benzerlik_tablosu

esleme<-sort(apply(benzerlik_tablosu,1,which.max))

iris_tahmin<-data.frame(iris[,-5],gercek_tur=as.factor(iris$Species),tahmini_tur=as.factor(names(esleme)[kume_nesne$cluster]))

slw<-ggplot(data=iris_tahmin,aes(x=Sepal.Length,y=Sepal.Width,color=tahmini_tur,shape=gercek_tur)) + geom_point() + theme(legend.position="none")
plw<-ggplot(data=iris_tahmin,aes(x=Petal.Length,y=Petal.Width,color=tahmini_tur,shape=gercek_tur)) + geom_point() + theme(legend.position="none")
slpl<-ggplot(data=iris_tahmin,aes(x=Sepal.Length,y=Petal.Length,color=tahmini_tur,shape=gercek_tur)) + geom_point() + theme(legend.position="none")
swpw<-ggplot(data=iris_tahmin,aes(x=Sepal.Width,y=Petal.Width,color=tahmini_tur,shape=gercek_tur)) + geom_point() + theme(legend.position="none")
legend<-ggplotGrob(swpw + theme(legend.position="bottom",legend.box="horizontal"))$grobs
legend<-legend[[which(sapply(legend, function(x) x$name) == "guide-box")]]

# install.packages("gridExtra")
library(gridExtra)

marrangeGrob(list(slw,plw,slpl,swpw,legend),layout_matrix=matrix(c(1,2,3,4,5,5),ncol=2,byrow=TRUE),nrow=3,ncol=2,top="",heights=c(3,3,1))
