#####################################################################################################
# I detta projekt �r vi ute efter att ta reda p� vilka av Sveriges 21 l�n som �r lika varandra samt
# skiljer sig �t i avseende p� fyra variabler. Variablerna i fr�ga �r antalet anm�lda fall av 
# misshandel, skadeg�relse samt narkotika-innehav ett givet l�n hade per 50 000 inv�nare �r 2014. 
# Varf�r antalet presenteras per 50 000 inv�nare �r f�r att man d� har m�jligheten att j�mf�ra l�n 
# med m�nga inv�nare, d�r det �r st�rre sannolikhet att dessa siffror �r h�gre, med l�n med l�gre 
# inv�narantal. Den fj�rde variabel �r eftergymnasial utbildning p� minst tre �r, som helt enkelt
# �r andelen inv�nare i varje l�n �r 2014 som uppfyllde detta kriterium. 
# Resultatet �ver hur l�nen skiljer sig �t presenteras f�rdelaktligen i en graf efter att en 
# Principal Component Analysis (PCA) genomf�rts.

# PCA g�r kortfattat ut p� att skapa en l�g-dimensionell representation av h�g-dimensionella data 
# som inkluderar en s� stor del av variationen i datamaterialet som m�jligt.
# F�r att utf�ra PCA beh�ver vi f�rst importera samt tv�tta data inneh�llandes information om de fyra 
# variablerna som ovan n�mnts. 
# Data ang�ende misshandel, skadeg�relse samt narkotika-innehav h�mtas fr�n myndigheten 
# "brottsf�rebyggande r�dets" databas (http://statistik.bra.se/solwebb/action/index). 
# F�r att f� fram antal personer i varje l�n med en eftergymnasial utbildning p� minst tre �r 
# h�mtas data fr�n "statistiska centralbyr�ns" databas 
# (http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/?rxid=fcf5115f-47a0-4f7f-8538-70be022ae865). 
# F�r att r�kna ut antal anm�lda brott per 50 000 inv�nare samt andelen med eftergymnasial 
# utbildning p� minst tre �r beh�vs information om antal inv�nare i varje l�n �r 2014. 
# Denna information kan �ven den h�mtas fr�n "statistiska centralbyr�ns" databas.

# Projektet �r uppdelat i tv� delar, d�r den f�rsta delen �r till�gnad �t importering samt tv�ttning 
# av datamaterialet som beh�vs f�r att utf�ra PCA. 
# Den andra delen handlar om implementeringen av v�r principal component analysis och l�ngst ner
# finner man en liten summering av resultatet.
#####################################################################################################


#####################################################
###############F�rbered datamaterialet###############
#####################################################

# Importera xls-filen med namn "Brott 2014" inneh�llandes information om  brott i Sverige f�r �r 2014. 
install.packages("XLConnect")
library(XLConnect)

Brott <- loadWorkbook(file.choose(), create = T)
Brott <- readWorksheet(Brott,1, header = F)

# Ta bort de fem f�rsta samt hela kolumn tre. Kolumn tv� konventeras sedan till typen numeric
Brott <- Brott[-c(1:5),]
Brott$Col3 <- NULL
Brott[,2] <- as.numeric(Brott[,2])

# Med hj�lp av en loop skapas en vektor inneh�llandes namnen p� alla de 21 l�n vi har tillg�ng till.
i <-1
L�n <- rep("",21)

for(d in 1:21){
  name <- Brott[i,1]
  
  L�n[d] <- name
  i = i + 8
}

# Upprepa varje namn i vektorn "L�n" 3 g�nger. Detta f�r att vi sedan i vektorerna "Brott" samt
# "Pop" enkelt ska kunna koppla raderna till r�tt l�n.
L�nx3 <- rep(L�n, each = 3)


# Innan vi kan slutf�ra vektorn "Brott" beh�ver vi skapa en vektor inneh�llande information om 
# Sveriges population �r 2014.

# Importera csv-filen med namn "Befolkning Sverige 2014" inneh�llandes populationen f�r Sverige 2014.
Pop <- read.csv(file.choose(), header=TRUE, sep=";")
Pop <- as.data.frame(tapply(Pop$X2014, Pop$region, sum)) 

# Ta bort de tv� siffrorna samt mellanrummet som st�r innan namnet p� varje l�n s� att vi kan 
# sortera varje l�ns folkm�ngd efter l�nsnamn i bokstavsordning.
# "Pop" blir en dataframe inneh�llandes varje l�ns folkm�ngd, d�r varje inv�narantal
# repeteras tre g�nger f�r att underl�tta n�r denna dataframe senare ska multipliceras med "Brott".
row.names(Pop) <- substring(rownames(Pop),4)
Pop <- Pop[order(row.names(Pop)),]
Popx1 <- as.vector(Pop)
Pop <- as.data.frame(rep(Pop, each = 3))

# Dividera folkm�ngden i varje l�n med 50 000
PopDiv50K <- Pop[,1] / 50000
Pop <- cbind(Pop, PopDiv50K, L�nx3)
colnames(Pop) <- c("Befolkning 2014", "Befolkning 2014 dividerat p� 50K", "L�n")


# Vi �terg�r nu till dataframen "Brott" f�r att g�ra klart den.

# Beh�ll endast de rader i dataframen "Brott" som inneh�ller v�rde i b�da kolumnerna.
# Med hj�lp av dataframen "Pop" ber�knas sedan andelen brott per 50 000 inv�nare i det specifika l�net.
Brott <- Brott[complete.cases(Brott),]
BrottPer50KInv <- round(Brott[,2] / Pop[,2], 0)
Brott <- cbind(Brott, BrottPer50KInv, L�nx3)
colnames(Brott) <- c("Brott", "Antal","Brott per 50K inv�nare", "L�n")


# Importera csv-filen med namn "Eftergym 2014" inneh�llandes information om antalet personer i
# Sverige uppdelat efter l�n som har en eftergymnasial utbildning p� minst tre �r.
Utbildning <- read.csv(file.choose(), header=TRUE, sep=";")
Utbildning <- as.data.frame(tapply(Utbildning$X2014, Utbildning$region, sum)) 

# Ta bort de tv� siffrorna samt mellanrummet som st�r innan namnet p� varje l�n s� att vi efter 
# l�nsnamn kan sortera antal personer i varje l�n med minst tre �rs eftergymnasial utbildning.
# Andelen personer i varje l�n med eftergymnasial utbildning p� minst tre �r r�knas sedan ut.
row.names(Utbildning) <- substring(rownames(Utbildning),4)
Utbildning <- as.vector(Utbildning[order(row.names(Utbildning)),])
Utbildning <- round(Utbildning / Popx1 * 100, 1)


# Vi har nu allt vi beh�ver f�r att skapa dataframen som beh�vs f�r v�r principal component analysis.
Misshandel <- subset(Brott$`Brott per 50K inv�nare`, Brott$Brott == "Misshandel inkl. grov (5, 6 �)")
Skadeg�relse <- subset(Brott$`Brott per 50K inv�nare`, Brott$Brott == "12 kap. Skadeg�relsebrott")
Narkotika_Innehav <- subset(Brott$`Brott per 50K inv�nare`, Brott$Brott == "Innehav (1-3 a �)")

FullData <- as.data.frame(cbind(Misshandel, Skadeg�relse, Narkotika_Innehav, Utbildning))
rownames(FullData) <- L�n


#####################################################
############Principal Component Analysis#############
#####################################################

# Vid principal component analysis �r det viktigt att skala de variabler som ska anv�ndas i analysen.
# Nedan ser vi t.ex. att variablen "skadeg�relse" har �verl�gset h�gst varians. Hade vi inte skalat
# v�ra variabler hade denna variabel bidraget �verl�gset mest till vart v�ra l�n placerade sig i den
# slutgiltliga plotten som illustruerar v�r principal component analysis.
apply(FullData, 2, var)

# Utf�r Principal Component Analysis
pca <- prcomp(FullData, scale = TRUE)

# Varje kolumn av pca$rotation inneh�ller den motsvarande principal component loading vectorn.
# Vi ser nedan att den f�rsta "principal componenten" beror mest p� variablerna "Misshandel", 
# "Skadeg�relse" samt "Narkotika_Innehav". 
# Den variabel som �verl�gset har st�rst p�verkan p� "principal component" nummer tv� �r "Utbildning".
pca$rotation

# Skapa en plot som redovisar den kumulativa andelen varians som varje principal component f�rklarar
pca.var <- pca$sdev^2
pca.varexplained <- pca.var / sum(pca.var)

plot(cumsum(pca.varexplained), xlab = "Principal Component", ylab = "Andel varians f�rklarad", 
     ylim = c(0,1), type = "b")

# Ca 85% av all varians f�rklars av de f�rsta tv� principal components. Nedan plottas dessa.
biplot(pca, scale = 0)


# Nedan redovisas en plot som b�ttre visualiserar l�nen med avseende p� kriminalitet samt utbildning. 
# De fyra pilarna som syntes i f�reg�ende plot kan dock inte tas fram h�r.

install.packages('ggplot2')
library(ggplot2)
pca.data <- data.frame(Sample = rownames(pca$x), X = pca$x[,1], Y = pca$x[,2])

ggplot(data = pca.data, aes(x=X, y=Y, label = Sample)) + geom_text() +
  xlab(paste("PC1 - ", round(pca.varexplained[1]*100,2), "% av den totala variansen f�rklarad", sep = "")) +
  ylab(paste("PC2 - ", round(pca.varexplained[2]*100,2), "% av den totala variansen f�rklarad", sep = "")) +
  theme_bw() + ggtitle("Principal Component Analysis med tv� principal components")


#####################################################################################################
# Plotten som resulterat efter v�r PCA kan tolkas som att l�n som placerar sig l�ngt till v�nster p� 
# x-axeln �r l�n med l�gt antal anm�lda fall av misshandel, skadeg�relse samt narkotika-innehav per 
# 50 000 inv�nare. Detta eftersom dessa tre variabler bidrar mest till principal component nummer 1. 
# Vi kan allts� sammanfatta det som att dessa l�n har l�g kriminalitet inom kategorierna misshandel, 
# skadeg�relse samt narkotika-innehav. I kontrast till detta s� har l�n som placerar sig l�ngre 
# h�gerut p� a-axeln h�gre kriminalitet inom dessa tre omr�den.

# N�r det g�ller vart ett l�n placerar sig p� y-axeln i plotten har variabeln utbildning, som allts� 
# �r andelen personer i l�net som har en eftergymnasial utbildning p� minst tre �r, st�rst p�verkan 
# och bidrar s�ledes mest till principal component nummer 2. Ju h�gre andel h�gutbildade inv�nare 
# ett l�n har, desto h�gre upp p� y-axeln placerar sig l�net. Vi ser dock att framf�rallt variabeln 
# misshandel ocks� p�verkar var p� y-axeln ett l�n placerar sig d�r effekten �r motsatt, ju h�gre 
# antal anm�lda fall av misshandel per 50 000 inv�nare, desto l�ngre ner hamnar man i plotten.

# Ett av tv� l�n som verkligen sticker ut i resultatet �r S�dermanlands l�n som placerar sig v�ldigt 
# l�ngt ner samt relativt l�ngt till h�ger. Vi kan s�ledes tolka detta l�ns placering som att detta 
# �r ett l�n som �r 2014 hade h�g kriminalitet, speciellt vad g�ller antal anm�lda fall av misshandel, 
# samt var ett l�n som hade en l�g andel h�gutbildade inv�nare om man j�mf�r med de andra l�nen. 
# Det andra l�net som sticker ut �r Stockholms l�n som placerar sig extremt l�ngt till h�ger i plotten,
# vilket inneb�r att de �r 2014 l�g i topp bland alla l�n vad g�ller antal anm�lda fall av misshandel,
# skadeg�relse samt narkotika-innehav. Eftersom de placeras s� l�ngt till h�ger p� x-axeln, men �nd� 
# ovanf�r mer �n h�lften av l�nen p� y-axeln, m�ste s�ledes Stockholms l�n ha en av den h�gsta andelen
# h�gutbildade inv�nare i landet.

# Andra intressanta slutsatser vi kan dra fr�n v�r PCA �r att l�nen Halland, Kronoberg, Norbotten, 
# Blekinge, V�rmland, J�nk�ping samt J�mtland �r v�ldigt lika varandra med avseende p� kriminalitet 
# inom de tre tidigare n�mnda omr�dena samt utbildningsniv�. D� dessa l�n ligger l�ngt till v�nster 
# p� x-axeln �r det utbildningsniv�n som framf�rallt skiljer t.ex. Hallands l�n fr�n J�nk�pings l�n, 
# d�r Hallands l�n har st�rre andel inv�nare med eftergymnasial utbildning p� minst tre �r. 
# Om vi forts�tter att studera Hallands l�n och j�mf�r dem med �sterg�tlands l�n ser vi att Halland 
# placerar sig h�gre upp p� y-axeln. Betyder det att de har en h�gre andel inv�nare med en 
# eftergymnasial utbildning p� minst tre �r? Svaret �r nej. Vi kan tolka det som att Halland har l�gre
# kriminalitet, men eftersom de tre variablerna, speciellt misshandel, bidrar till att ett l�n 
# placerar sig l�ngre n�r p� y-axeln, betyder det att �sterg�tlands l�n har en h�gre andel 
# v�lutbildade inv�nare. Detsamma g�ller vid j�mf�relse av t.ex. Uppsala l�n och Stockholms l�n.
#####################################################################################################