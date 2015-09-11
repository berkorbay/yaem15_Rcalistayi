#YAEM 2015 R Çalıştayı İleri Düzet dplyr ve ggplot2 kodları

#Aşağıdaki kodlar bazı R opsiyonlarını yeniden ayarlama ve paket yükleme için bulunuyor
rm(list=ls(all=TRUE)) #çalışma çevresindeki bütün değişkenleri temizliyor
gc() #garbage collection. hafızada yer açıyor.

options(repos="http://cran.rstudio.com/") #Paket deposu olarak RStudio'yu kullan
options(dplyr.width = Inf) #Dplyr komutlarını kullanırken bütün sütunları göster
options(scipen = 7) #Ondalıklardaki basamak sayısı
options(java.parameters = "-Xmx8g") #xlsx paketi için Java hafıza limitini 8g'ye çıkart

#Önceden ayarlanmış bir rastgelelik tohumu varsa sil
if(exists(".Random.seed"))
	rm(.Random.seed, envir=globalenv())


# install.packages("readxl") #xlsx dosyalarını hızlıca okumak için
# install.packages("xlsx") #xlsx dosyaları üzerinde yazma okuma yapmak için
# install.packages("dplyr") #veri setleri üzerinde oynama yapmak için
# install.packages("reshape2") #cast ve melt fonksiyonları için
# install.packages("ggplot2") #grafikler için
# install.packages("scales") #grafikler için


library(readxl)
library(xlsx)
library(dplyr)
library(reshape2)
library(scales)
library(ggplot2)

ana_dizin<-"~/Dropbox/PhD_Workshop/Reports/YAEM Calistay - 20150912/" #kendi çalışma ortamınızla değiştirin
#ana_dizin<-"C:/....." #Windows kullanicilari icin
#ana_dizin<-paste0(getwd(),"/yaem15_Rcalistayi-master/") #Windows kullanicilari icin


# xlsx dosyasından veriyi oku ve emeklilik_sirketler değişkenine kaydet
emeklilik_sirketler<-read_excel(paste0(ana_dizin,"emeklilik_verileri_egm_2011_2015.xlsx"),sheet="sirket_temel_gostergeler") 

#Veri setini incele
head(emeklilik_sirketler)

#Sütun türlerini gör ve örnek verileri incele
glimpse(emeklilik_sirketler)

#Tarih formatını ayarla
emeklilik_sirketler$Tarih <- as.Date(emeklilik_sirketler$Tarih,format="%d.%m.%Y")

#Veri setindeki emeklilik şirketlerini gör
emeklilik_sirketler %>% 
						select(`Emeklilik Şirketi`) %>% #select only Emeklilik Şi
						distinct(`Emeklilik Şirketi`) %>%
						print(n=25)

#Sektör toplamını filtrele
emeklilik_bilgi <- emeklilik_sirketler %>% 
						filter(grepl("Sektör Toplamı",`Emeklilik Şirketi`))

#Sektör toplamının katılımcı sayısı gelişimini çizgi grafiğine dök 
sektor_toplam_cizgi<- ggplot(data=emeklilik_bilgi,aes(x=Tarih)) + geom_line(aes(y=`Katılımcı Sayısı`,color=`Emeklilik Şirketi`)) 

sektor_toplam_cizgi

#Dikey çizgi indir (devlet desteğinin başladığı zaman 7 haziran 2013)
sektor_toplam_cizgi <- sektor_toplam_cizgi + geom_vline(xintercept = as.numeric(as.Date("2013-06-07")))
sektor_toplam_cizgi

#21 Ağustos'ta emeklilik şirketlerinin katılımcı sayısının sütun grafiği için 
#21 Ağustos verilerini bul ve sektör toplamını veri setinden çıkar
emeklilik_bilgi <- emeklilik_sirketler %>% 
										filter(Tarih == "2015-08-21" & !grepl("Sektör Toplamı",`Emeklilik Şirketi`))

sirketler_katilimci_sutun <- ggplot(data=emeklilik_bilgi) + geom_bar(aes(x=`Emeklilik Şirketi`,y=`Katılımcı Sayısı`,fill=`Emeklilik Şirketi`),stat="identity") 
sirketler_katilimci_sutun

#Şekilsel değişiklikler yap
#yatay düzlemin etiketlerini küçült, açısını değiştir ve hizala
sirketler_katilimci_sutun <- sirketler_katilimci_sutun + theme(legend.position="none",axis.text.x=element_text(size=8,angle=30,hjust=1))
sirketler_katilimci_sutun 

#grafik başlığı koy
sirketler_katilimci_sutun <- sirketler_katilimci_sutun + ggtitle("21 Ağustos 2015 BES Katılımcı Sayıları")
sirketler_katilimci_sutun 


#Pasta grafiği ekleme örneği
#Bes katılımcılarının yaş gruplarını bul
yas_gruplari <- read_excel(paste0(ana_dizin,"emeklilik_verileri_egm_2011_2015.xlsx"),sheet="yas_dagilim") 
yas_gruplari$Tarih <- as.Date(yas_gruplari$Tarih,format="%d.%m.%Y")

glimpse(yas_gruplari)

#Toplam sütununu çıkar, 21 Ağustos 2015 tarihini filtrele, sütunları satırlara çevir
yas_gruplari_bilgi<- yas_gruplari %>%
							select(-Toplam) %>%
							filter(Tarih == "2015-08-21") %>%
							melt(id.vars="Tarih",variable.name="Yaş Aralığı",value.name="Katılımcı Sayısı") %>% 
							mutate(yuzde=paste0(round(100*`Katılımcı Sayısı`/sum(`Katılımcı Sayısı`)),"%"))


yas_gruplari_bilgi

#Tiramisu grafiği
yas_gruplari_pasta<- ggplot(data=yas_gruplari_bilgi, aes(x=factor(1),y=`Katılımcı Sayısı`,fill=factor(`Yaş Aralığı`))) + geom_bar(width=1,stat="identity",color="black") 
yas_gruplari_pasta

#Düzgün pasta grafiği
yas_gruplari_pasta <- yas_gruplari_pasta + coord_polar(theta="y")
yas_gruplari_pasta 

#Yüzdesel bilgileri ekleyelim
yas_gruplari_pasta <- yas_gruplari_pasta + geom_text(aes(y=`Katılımcı Sayısı`/2 + c(0,cumsum(`Katılımcı Sayısı`)[-length(`Katılımcı Sayısı`)]),label=yuzde))
yas_gruplari_pasta 

#Elini yüzünü düzeltelim
yas_gruplari_pasta <- yas_gruplari_pasta + scale_fill_discrete("Yaş Grupları") +
		labs(title="21 Ağustos 2015 BES Katılımcıları Yaş Dağılımı") + 
        guides(fill=guide_legend(override.aes=list(colour=NA))) +
	    theme(axis.ticks=element_blank(),  # the axis ticks
		        axis.title=element_blank(),  # the axis labels
		        axis.text.x=element_blank(),
		        axis.text.y=element_blank(),
				panel.background = element_rect(fill = "transparent",colour = NA),
				panel.grid.minor = element_blank(), 
				panel.grid.major = element_blank(),
				plot.background = element_rect(fill = "transparent",colour = NA),
				legend.text=element_text(size=18),
				legend.title=element_text(size=18),
				legend.position="left")

yas_gruplari_pasta

#25-34 yaş aralığının Toplam içerisindeki yüzdesel temsilinin çizgi grafiği
yas_gruplari_bilgi<- yas_gruplari %>%
							transmute(Tarih,`Yüzdesel 25/34`=round(`25-34 yaş`/Toplam,4)) 

#25-34 yaş aralığının Toplam içerisindeki yüzdesel temsilinin çizgi grafiği
yas_gruplari_cizgi <- ggplot(data=yas_gruplari_bilgi,aes(x=Tarih)) + geom_line(aes(y=`Yüzdesel 25/34`),color="purple")
yas_gruplari_cizgi 

#Elini yüzünü düzeltelim

yas_gruplari_cizgi <- yas_gruplari_cizgi + 
							scale_y_continuous(labels = percent) +  #değerleri yüzde haline getir
							labs(title="25-34 Yaş Aralığının Katılımcı Sayısındaki Oranı", x="",y="Yüzde") #x,y düzlemi başlıkları ve grafik başlığı

yas_gruplari_cizgi

yas_gruplari_bilgi<- yas_gruplari %>%
						melt(id.vars=c("Tarih","Toplam"),variable.name="Yaş Aralığı",value.name="Katılımcı Sayısı") %>%
						mutate(yuzde=(`Katılımcı Sayısı`/Toplam))

yas_gruplari_cizgi2 <- ggplot(data=yas_gruplari_bilgi,aes(x=Tarih)) + 
						geom_line(aes(y=yuzde,color=`Yaş Aralığı`),size=1.2) + 
						scale_y_continuous(labels = percent) +
						labs(title="Yaş Gruplarına Göre Katılımcı Oranı", x="",y="Katılımcı Oranı")

yas_gruplari_cizgi2


#Nokta grafiği yapalım

emeklilik_bilgi <- emeklilik_sirketler %>% 
						filter(Tarih == "2015-08-21" & !grepl("Sektör Toplamı",`Emeklilik Şirketi`))

#Katılımcı Sayısı ile Toplam Fon Tutarı
sirketler_nokta<-ggplot(data=emeklilik_bilgi,aes(x=`Katılımcı Sayısı`/1000,y=`Katılımcıların Fon Tutarı (TL)`/10^6)) + geom_point(aes(color=`Emeklilik Şirketi`))
sirketler_nokta


#Biraz elini yüzünü düzeltelim ve emeklilik fonlarının isimlerini noktaların üzerine yazalım
sirketler_nokta<-sirketler_nokta + 
					geom_text(aes(label=`Emeklilik Şirketi`),size=2.5,angle=15,vjust=-1,position=position_jitter(width=3, height=4)) + 
					labs(x="Katılımcı Sayısı (x1000)",y="Katılımcıların Fon Tutarı (milyon TL)") +
					theme(legend.position="none") 
sirketler_nokta





