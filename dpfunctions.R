# Fonksiyon 9.1: Tepe deðeri hesaplama fonksiyonu
tepe <- function(x) { 
    xuniques <- sort(unique(x))
    xuniques[which.max(tabulate(match(x, xuniques)))] 
}

# Fonksiyon 10.1: Aykýrý deðer saptama fonksiyonu
adsapta <- function(x){
   adx <- c()
    cx <- x
    repeat{
      bpx <- boxplot(cx, col="gray", plot=FALSE)
      if(length(bpx$out)>0){
        adx <- c(adx, bpx$out)
        cx <- cx[-which(cx %in% adx)]
      }else
        break
    }
    return(adx)
}

# Fonksiyon 10.2: Aykýrý deðer saptama fonksiyonu 2
adsapta2 <- function(x){
  adx <- c()
  nd <- ncol(x)
  for(i in 1:nd){
    adx <- c(adx, which(x[,i] %in% adsapta(x[,i])))
  }
  return(adx)
}

# Fonksiyon 10.3: LOF ile aykýrý deðer bulma
adlof <- function(x){
  p <- ncol(x)
  for(i in 1:p){
    q3 <- summary(x[,i])[5]
    cutoff <- q3 +IQR(x[,i])
    ad <- which(x[,i] > cutoff)
    print(ad)
  }
}


# Fonksiyon 12.1: Sistematik örnekleme fonksiyonu
ssample <- function(N, n){
   k <- ceiling(N/n)
   fidx <- sample(1:k, 1)
   sampled <- seq(fidx, fidx + k*(n-1), k)
   return(sampled)
}

# Fonksiyon 12.2: Dosyadan okuyarak örnekleme fonksiyonu
read.samples <- function(file, xs){
    xsamp <- data.frame()
    for(i in xs){
     rx <- read.table(file, sep="\t", header=FALSE, skip = i,
     nrows = 1)
     xsamp <- rbind(xsamp, rx)
   }
   return(xsamp)
} 

# Fonksiyon 14.1: Bellek tüketimi
bellektuketim <- function(nlist){
  if(missing(nlist)) nlist=ls()
  tbellek <- 0
  for(nesne in nlist){
    tbellek <- tbellek + object.size(nesne)
  }
  return(tbellek)
}

# Fonksiyon 14.2: Varyans hesaplama fonksiyonu
varx <- function(x){
  n <- length(x)
  ssqd <- 0
  for(i in 1:n)
    ssqd <- ssqd +(x[i]-mean(x))^2
  varyans <- ssqd/(n-1)
  varyans
}

# Program Kodu 1
kayitno <- 1
tks <- nrow(uyetablo)	#tks: toplam kayýt sayýsý
while(kayitno <= tks){
if(uyetablo[kayitno,3]!="E" & uyetablo[kayitno,3]!="K"){
     ik <- substr(uyetablo[kayitno,3], 1, 1)
      if(ik == "E" | ik == "e")
        uyetablo[kayitno,3] <- "E"
      else if(ik == "K" | ik == "k")
        uyetablo[kayitno,3] <- "K"
     else 
       uyetablo[kayitno,3] <- NA
   }
   kayitno <- kayitno + 1
}


# Program Kodu 2
# Tutarsýz cinsiyet kayýtlarý alt kümesini oluþtur
ttrszcins <- which(uyetablo[,3]!="E"&uyetablo[,3]!="K")
# Tutarsýz cinsiyet kayýtlarýný düzelt 
for(kayitno in ttrszcins){
 ik <- substr(uyetablo[kayitno,3], 1, 1)
 if(ik == "E" | ik == "e")
   uyetablo[kayitno,3] <- "E"
 else if (ik == "K" | ik == "k")
   uyetablo[kayitno,3] <- "K"
 else 
   uyetablo[kayitno,3] <- NA
}

# Program Kodu 23
kayitno <- 1
tks <- nrow(uyetablo)	#tks: toplam kayýt sayýsý
while(kayitno <= tks){
   ik <- substr(uyetablo[kayitno, 2], 1, 1)
   if(!(ik %in% LETTERS)){
	   if(ik %in% letters){
        ik <- toupper(ik)
        dk <- tolower(substr(uyetablo[kayitno, 2], 2, 
            nchar(uyetablo[kayitno, 2])))
        uyetablo[kayitno, 2] <- paste0(ik, dk)
     }else{
        uyetablo[kayitno, 2] <- NA
     }
   }
   kayitno <- kayitno + 1
}

# Program Kodu 4
# Tutarsýz ad kayýtlarý alt kümesini oluþtur
ttrszad <- which(!(substr(uyetablo[,2], 1, 1) %in%
 LETTERS))
# Tutarsýz ad kayýtlarýný düzelt 
for(kayitno in ttrszad){
   if(substr(uyetablo[kayitno,2], 1, 1) %in% letters){
     ik <- toupper(substr(uyetablo[kayitno, 2], 1, 1))
     dk <- tolower(substr(uyetablo[kayitno, 2], 2,
        nchar(uyetablo[kayitno, 2])))
     uyetablo[kayitno, 2] <- paste0(ik, dk)
   }else{
     uyetablo[kayitno, 2] <- NA
   }
}





