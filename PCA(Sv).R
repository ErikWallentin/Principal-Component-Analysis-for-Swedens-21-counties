#####################################################################################################
# I detta projekt är vi ute efter att ta reda på vilka av Sveriges 21 län som är lika varandra samt
# skiljer sig åt i avseende på fyra variabler. Variablerna i fråga är antalet anmälda fall av 
# misshandel, skadegörelse samt narkotika-innehav ett givet län hade per 50 000 invånare år 2014. 
# Varför antalet presenteras per 50 000 invånare är för att man då har möjligheten att jämföra län 
# med många invånare, där det är större sannolikhet att dessa siffror är högre, med län med lägre 
# invånarantal. Den fjärde variabel är eftergymnasial utbildning på minst tre år, som helt enkelt
# är andelen invånare i varje län år 2014 som uppfyllde detta kriterium. 
# Resultatet över hur länen skiljer sig åt presenteras fördelaktligen i en graf efter att en 
# Principal Component Analysis (PCA) genomförts.

# PCA går kortfattat ut på att skapa en låg-dimensionell representation av hög-dimensionella data 
# som inkluderar en så stor del av variationen i datamaterialet som möjligt.
# För att utföra PCA behöver vi först importera samt tvätta data innehållandes information om de fyra 
# variablerna som ovan nämnts. 
# Data angående misshandel, skadegörelse samt narkotika-innehav hämtas från myndigheten 
# "brottsförebyggande rådets" databas (http://statistik.bra.se/solwebb/action/index). 
# För att få fram antal personer i varje län med en eftergymnasial utbildning på minst tre år 
# hämtas data från "statistiska centralbyråns" databas 
# (http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/?rxid=fcf5115f-47a0-4f7f-8538-70be022ae865). 
# För att räkna ut antal anmälda brott per 50 000 invånare samt andelen med eftergymnasial 
# utbildning på minst tre år behövs information om antal invånare i varje län år 2014. 
# Denna information kan även den hämtas från "statistiska centralbyråns" databas.

# Projektet är uppdelat i två delar, där den första delen är tillägnad åt importering samt tvättning 
# av datamaterialet som behövs för att utföra PCA. 
# Den andra delen handlar om implementeringen av vår principal component analysis och längst ner
# finner man en liten summering av resultatet.
#####################################################################################################


#####################################################
###############Förbered datamaterialet###############
#####################################################

# Importera xls-filen med namn "Brott 2014" innehållandes information om  brott i Sverige för år 2014. 
install.packages("XLConnect")
library(XLConnect)

Brott <- loadWorkbook(file.choose(), create = T)
Brott <- readWorksheet(Brott,1, header = F)

# Ta bort de fem första samt hela kolumn tre. Kolumn två konventeras sedan till typen numeric
Brott <- Brott[-c(1:5),]
Brott$Col3 <- NULL
Brott[,2] <- as.numeric(Brott[,2])

# Med hjälp av en loop skapas en vektor innehållandes namnen på alla de 21 län vi har tillgång till.
i <-1
Län <- rep("",21)

for(d in 1:21){
  name <- Brott[i,1]
  
  Län[d] <- name
  i = i + 8
}

# Upprepa varje namn i vektorn "Län" 3 gånger. Detta för att vi sedan i vektorerna "Brott" samt
# "Pop" enkelt ska kunna koppla raderna till rätt län.
Länx3 <- rep(Län, each = 3)


# Innan vi kan slutföra vektorn "Brott" behöver vi skapa en vektor innehållande information om 
# Sveriges population år 2014.

# Importera csv-filen med namn "Befolkning Sverige 2014" innehållandes populationen för Sverige 2014.
Pop <- read.csv(file.choose(), header=TRUE, sep=";")
Pop <- as.data.frame(tapply(Pop$X2014, Pop$region, sum)) 

# Ta bort de två siffrorna samt mellanrummet som står innan namnet på varje län så att vi kan 
# sortera varje läns folkmängd efter länsnamn i bokstavsordning.
# "Pop" blir en dataframe innehållandes varje läns folkmängd, där varje invånarantal
# repeteras tre gånger för att underlätta när denna dataframe senare ska multipliceras med "Brott".
row.names(Pop) <- substring(rownames(Pop),4)
Pop <- Pop[order(row.names(Pop)),]
Popx1 <- as.vector(Pop)
Pop <- as.data.frame(rep(Pop, each = 3))

# Dividera folkmängden i varje län med 50 000
PopDiv50K <- Pop[,1] / 50000
Pop <- cbind(Pop, PopDiv50K, Länx3)
colnames(Pop) <- c("Befolkning 2014", "Befolkning 2014 dividerat på 50K", "Län")


# Vi återgår nu till dataframen "Brott" för att göra klart den.

# Behåll endast de rader i dataframen "Brott" som innehåller värde i båda kolumnerna.
# Med hjälp av dataframen "Pop" beräknas sedan andelen brott per 50 000 invånare i det specifika länet.
Brott <- Brott[complete.cases(Brott),]
BrottPer50KInv <- round(Brott[,2] / Pop[,2], 0)
Brott <- cbind(Brott, BrottPer50KInv, Länx3)
colnames(Brott) <- c("Brott", "Antal","Brott per 50K invånare", "Län")


# Importera csv-filen med namn "Eftergym 2014" innehållandes information om antalet personer i
# Sverige uppdelat efter län som har en eftergymnasial utbildning på minst tre år.
Utbildning <- read.csv(file.choose(), header=TRUE, sep=";")
Utbildning <- as.data.frame(tapply(Utbildning$X2014, Utbildning$region, sum)) 

# Ta bort de två siffrorna samt mellanrummet som står innan namnet på varje län så att vi efter 
# länsnamn kan sortera antal personer i varje län med minst tre års eftergymnasial utbildning.
# Andelen personer i varje län med eftergymnasial utbildning på minst tre år räknas sedan ut.
row.names(Utbildning) <- substring(rownames(Utbildning),4)
Utbildning <- as.vector(Utbildning[order(row.names(Utbildning)),])
Utbildning <- round(Utbildning / Popx1 * 100, 1)


# Vi har nu allt vi behöver för att skapa dataframen som behövs för vår principal component analysis.
Misshandel <- subset(Brott$`Brott per 50K invånare`, Brott$Brott == "Misshandel inkl. grov (5, 6 §)")
Skadegörelse <- subset(Brott$`Brott per 50K invånare`, Brott$Brott == "12 kap. Skadegörelsebrott")
Narkotika_Innehav <- subset(Brott$`Brott per 50K invånare`, Brott$Brott == "Innehav (1-3 a §)")

FullData <- as.data.frame(cbind(Misshandel, Skadegörelse, Narkotika_Innehav, Utbildning))
rownames(FullData) <- Län


#####################################################
############Principal Component Analysis#############
#####################################################

# Vid principal component analysis är det viktigt att skala de variabler som ska användas i analysen.
# Nedan ser vi t.ex. att variablen "skadegörelse" har överlägset högst varians. Hade vi inte skalat
# våra variabler hade denna variabel bidraget överlägset mest till vart våra län placerade sig i den
# slutgiltliga plotten som illustruerar vår principal component analysis.
apply(FullData, 2, var)

# Utför Principal Component Analysis
pca <- prcomp(FullData, scale = TRUE)

# Varje kolumn av pca$rotation innehåller den motsvarande principal component loading vectorn.
# Vi ser nedan att den första "principal componenten" beror mest på variablerna "Misshandel", 
# "Skadegörelse" samt "Narkotika_Innehav". 
# Den variabel som överlägset har störst påverkan på "principal component" nummer två är "Utbildning".
pca$rotation

# Skapa en plot som redovisar den kumulativa andelen varians som varje principal component förklarar
pca.var <- pca$sdev^2
pca.varexplained <- pca.var / sum(pca.var)

plot(cumsum(pca.varexplained), xlab = "Principal Component", ylab = "Andel varians förklarad", 
     ylim = c(0,1), type = "b")

# Ca 85% av all varians förklars av de första två principal components. Nedan plottas dessa.
biplot(pca, scale = 0)


# Nedan redovisas en plot som bättre visualiserar länen med avseende på kriminalitet samt utbildning. 
# De fyra pilarna som syntes i föregående plot kan dock inte tas fram här.

install.packages('ggplot2')
library(ggplot2)
pca.data <- data.frame(Sample = rownames(pca$x), X = pca$x[,1], Y = pca$x[,2])

ggplot(data = pca.data, aes(x=X, y=Y, label = Sample)) + geom_text() +
  xlab(paste("PC1 - ", round(pca.varexplained[1]*100,2), "% av den totala variansen förklarad", sep = "")) +
  ylab(paste("PC2 - ", round(pca.varexplained[2]*100,2), "% av den totala variansen förklarad", sep = "")) +
  theme_bw() + ggtitle("Principal Component Analysis med två principal components")


#####################################################################################################
# Plotten som resulterat efter vår PCA kan tolkas som att län som placerar sig långt till vänster på 
# x-axeln är län med lågt antal anmälda fall av misshandel, skadegörelse samt narkotika-innehav per 
# 50 000 invånare. Detta eftersom dessa tre variabler bidrar mest till principal component nummer 1. 
# Vi kan alltså sammanfatta det som att dessa län har låg kriminalitet inom kategorierna misshandel, 
# skadegörelse samt narkotika-innehav. I kontrast till detta så har län som placerar sig längre 
# högerut på a-axeln högre kriminalitet inom dessa tre områden.

# När det gäller vart ett län placerar sig på y-axeln i plotten har variabeln utbildning, som alltså 
# är andelen personer i länet som har en eftergymnasial utbildning på minst tre år, störst påverkan 
# och bidrar således mest till principal component nummer 2. Ju högre andel högutbildade invånare 
# ett län har, desto högre upp på y-axeln placerar sig länet. Vi ser dock att framförallt variabeln 
# misshandel också påverkar var på y-axeln ett län placerar sig där effekten är motsatt, ju högre 
# antal anmälda fall av misshandel per 50 000 invånare, desto längre ner hamnar man i plotten.

# Ett av två län som verkligen sticker ut i resultatet är Södermanlands län som placerar sig väldigt 
# långt ner samt relativt långt till höger. Vi kan således tolka detta läns placering som att detta 
# är ett län som år 2014 hade hög kriminalitet, speciellt vad gäller antal anmälda fall av misshandel, 
# samt var ett län som hade en låg andel högutbildade invånare om man jämför med de andra länen. 
# Det andra länet som sticker ut är Stockholms län som placerar sig extremt långt till höger i plotten,
# vilket innebär att de år 2014 låg i topp bland alla län vad gäller antal anmälda fall av misshandel,
# skadegörelse samt narkotika-innehav. Eftersom de placeras så långt till höger på x-axeln, men ändå 
# ovanför mer än hälften av länen på y-axeln, måste således Stockholms län ha en av den högsta andelen
# högutbildade invånare i landet.

# Andra intressanta slutsatser vi kan dra från vår PCA är att länen Halland, Kronoberg, Norbotten, 
# Blekinge, Värmland, Jönköping samt Jämtland är väldigt lika varandra med avseende på kriminalitet 
# inom de tre tidigare nämnda områdena samt utbildningsnivå. Då dessa län ligger långt till vänster 
# på x-axeln är det utbildningsnivån som framförallt skiljer t.ex. Hallands län från Jönköpings län, 
# där Hallands län har större andel invånare med eftergymnasial utbildning på minst tre år. 
# Om vi fortsätter att studera Hallands län och jämför dem med Östergötlands län ser vi att Halland 
# placerar sig högre upp på y-axeln. Betyder det att de har en högre andel invånare med en 
# eftergymnasial utbildning på minst tre år? Svaret är nej. Vi kan tolka det som att Halland har lägre
# kriminalitet, men eftersom de tre variablerna, speciellt misshandel, bidrar till att ett län 
# placerar sig längre när på y-axeln, betyder det att Östergötlands län har en högre andel 
# välutbildade invånare. Detsamma gäller vid jämförelse av t.ex. Uppsala län och Stockholms län.
#####################################################################################################