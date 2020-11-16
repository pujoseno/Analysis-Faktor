data=read.delim("clipboard")
head(data)


#missing data 
databaru=na.omit(data)
summary(databaru)

#outlier data
library(MVN)
hasil=mvOutlier(databaru, qqplot=T, method="quan")
newdata=hasil$newData
summary(newdata)

#fungsi
kmo <- function(x)
{
  x <- subset(x, complete.cases(x))       # menghilangkan data kosong (NA)
  r <- cor(x)                             # Membuat matrix korelasi
  r2 <- r^2                               # nilai koefisien untuk r squared
  i <- solve(r)                           # Inverse matrix dari matrix korelasi
  d <- diag(i)                            # element diagonal dari inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2          # koefisien korelasi Parsial kuadrat
  diag(r2) <- diag(p2) <- 0               # menghapus element diagonal 
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

uji_bart <- function(x)
{
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) 
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "Khi-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}

cor(newdata)
kmo(newdata)
uji_bart(newdata)

#Analisis faktor (factanal)
#analisis_faktor <- factanal(newdata, factor=2, rotated="none") #lihat nilai eigenvalue, berapa factor yang memenuhi
#analisis_faktor
#analisis_faktor_rotasi <- factanal(newdata,factor=2, rotated="varimax") #dari analisis faktornya didapat 2 faktor, kemudian dirotasi dengan metode varimax

#analisis faktor dengan "psych"
#install.packages("psych")
library(psych)

#metode none (tanpa rotasi)
pcadata<- principal(newdata,nfactor=2,rotate="none")
pcadata
plot(pcadata$values, type="b",ylab="Nilai Eigenvalues",xlab="Komponen yang terbentuk", lab=c(5,5,5))

#Solusi terakhir dengan rotasi (Varimax)
pcadata.r <- principal(newdata,nfactor=2,rotate="varimax",scores=T)
pcadata.r

#melihat plot komponen yang terbentuk
plot(pcadata$values,type="b",ylab="Nilai Eigenvalues",xlab="Komponen yang terbentuk",lab=c(5,5,5))
plot



#untuk PCA pada regresi PCA ketika asumsi multikolinearitas tidak terpenuhi
#sw.scores <-pcadata.r$scores # mendapatkan skor untuk masing-masing faktor (hasil linier dari variabel-variabel) 
#sw.scores

#skor dari faktor bisa digunakan untuk analisis lanjut seperti Regresi PCA (untuk mengatasi permasalahan multikolinieritas)
#sw.score <- data.frame(pcadata.r$scores) #hasil faktor(komponen) dirubah kedalam data.frame
#(summary(lm(newdata$output~sw.score$RC1+sw.score$RC2))) # regresi --> output(berat sapi) = B0+ B1.Faktor_1 +B2.Faktor_2




