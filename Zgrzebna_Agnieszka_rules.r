# AGNIESZKA ZGRZEBNA

############################################################################################

# CEL EKSPERYMENTU:
# 
# Odkrycie zale¿no¶ci miêdzy wy¿sz± klas± dochodow±(income=large w nastêpniku), a innymi zmiennymi.  
# 
# Regu³y interesuj±ce: wsparcie 15%, zaufanie 80%, 4-5 cech po lewej stronie.
# 
# Najlepsz± regu³± jest ta, która spe³nia powy¿sze za³o¿enia i ma najwy¿sze wsparcie. 

############################################################################################

# EKSPERYMENT:

# ³adowanie bibliotek
library(arules)
library(arulesViz)

# Zbiór danych:
data("AdultUCI") 

# wstêpne zapoznanie siê z danymi
dim(AdultUCI)
View(AdultUCI)
summary(AdultUCI)
str(AdultUCI)

# Przetwarzanie danych:

# atrybuty tylko z warto¶ci± unikaln±
which(sapply(AdultUCI, function(x) {length(unique(x))}) == 1) #nie wystêpuj±

# atrybuty z warto¶ciami tylko unikalnymi
which(sapply(AdultUCI, function(x) {length(unique(x))}) == nrow(AdultUCI)) #nie wystêpuj±

# zduplikowane wiersze
length(which(duplicated(AdultUCI) == TRUE))

# usuniecie zduplikowanych wierszy
AdultUCI <- unique(AdultUCI)
length(which(duplicated(AdultUCI) == TRUE))
dim(AdultUCI)

# znalezienie w zbiorze AdultUCI atrybutów z brakuj±cymi danymi i uzupe³nienie danych
# ¶redni± (dla warto¶ci ci±g³ych) lub warto¶ci± dominuj±c± (dla warto¶ci dyskretnych)
res <- sapply(AdultUCI,function(x) {anyNA(x)})
idx = which(res == TRUE)

for (i in 1:length(idx))
{
  if(is.factor(AdultUCI[[idx[i]]]))
  {
    tab = table(AdultUCI[idx[i]])
    value = names(tab[which(tab == max(tab))[1]])
    AdultUCI[which(is.na(AdultUCI[idx[i]])==TRUE),idx[i]] <- value
  }
  else if(is.numeric(AdultUCI[[idx[i]]])==TRUE)
  {
    AdultUCI[which(is.na(AdultUCI[idx[i]])==TRUE),idx[i]] <- mean(na.omit(AdultUCI[[idx[i]]]))  
  }
}
summary(AdultUCI)
anyNA(AdultUCI)
dim(AdultUCI)

# usuniêcie niepotrzebnych atrybutów
AdultUCI[["fnlwgt"]] <- NULL 
AdultUCI[["education-num"]] <-NULL

# dyskretyzacja atrybutów ci±g³ych
hist(AdultUCI$age)
hist(AdultUCI$`hours-per-week`)
hist(AdultUCI$`capital-gain`)
hist(AdultUCI$`capital-loss`)

AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(16,30,50,70,91)), labels = c("Young", "Middle-aged", "Senior", "Old"))

AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]],c(0,25,40,60,168)),
                                         labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))

AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)), labels = c("None", "Low", "High"))

AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
                                           c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)),
                                       labels = c("none", "low", "high"))

# Wykrywanie regu³ asocjacyjnych:

# przekszta³enie postaci relacyjnej na transakcyjn±
adultTR <- as(AdultUCI, "transactions")
inspect(head(adultTR))
summary(adultTR)

# ustalenie adekwatnego wsparcia wzglêdnego

# czêsto¶æ wystêpowania elementów z min 15% wsparcia wzglêdnego
freqTbl  = itemFrequency(adultTR, type = "relative")
freqTbl = sort(freqTbl, decreasing= TRUE)
print(freqTbl[freqTbl>0.15])
itemFrequencyPlot(adultTR, type ="relative", support= 0.15)

# wizualizacja income
pie(table(AdultUCI$"income"))
barplot(table(AdultUCI$"income"), cex.names=0.7)

# liczba czêstych elementów od 1% do 100% z skokiem 5pp dla ca³ej bazy transakcji 
sup = seq(0.01, 1, 0.05)
nbFSet<-vector(mode = 'integer', length = length(sup))

for( i in 1:length(sup))
{
  nbFSet[i] <- length(freqTbl[freqTbl>=sup[i]])
}

res <- data.frame(sup, nbFSet)
View(res)

library(ggplot2)
qplot(sup,nbFSet,
      geom=c("point","line"), xlab="Wsparcie", ylab="Liczba czêstych elementów")+ theme_bw()

# Postanowi³am zmieniæ definicjê regu³y interesuj±cej dla wsparcia na 5% z uwagi na to, ¿e wy¿sza 
# klasa dochodowa stanowi zaledwie oko³o 16% w ca³ej bazie danych. Dodatkowo na tle ca³ej bazy transakcji
# mniejsze wsparcie wydaje siê bardziej adekwatne z uwagi na spor± liczbê rekordów i ich atrybutów. 

# tworzenie parametru dla metody apriori zgodnie z regu³ami interesuj±cymi
aParam  = new("APparameter", "confidence" = 0.8, "support" =0.05, minlen = 5L, maxlen = 6L, 
              maxtime = 20, "target" = "rules")
aParam

# tworzenie regu³ asocjacyjnych za pomoc± funkcji apriori bez zak³adania nastêpnika - wstêpna analiza
aRules <-apriori(adultTR,aParam)
summary(aRules)
inspect(head(sort(aRules, by="support"),10))
inspectDT(head(aRules,10000))
# Dla za³o¿onych regu³ wy¿szy próg dochodowy nie wystêpuje jako nastêpnik, ale pojawia siê w poprzedniku.
# Oznacza to, ¿e nie znaleziono elementów wp³ywaj±cych na sukces ekonomiczny, ale (na chwilê obecn±) za
# jego pomoc± mo¿na przewidywaæ wystêpowanie innych elementów z nim powi±zanych. 

# korekta regu³ interesuj±cych
aParam@confidence = 0.6
aParam@support = 0.01
aParam
aRules <-apriori(adultTR,aParam)
summary(aRules)
inspectDT(head(aRules,10000))
# Dla zmienionych warto¶ci (wsparcie wzglêdne 1%, zaufanie 60%) uda³o siê znale¼æ 37 interesuj±cych regu³,
# po przefiltrowaniu regu³ za pomoc± inspectDT dla wy¿szego progu dochodowego w nastêpniku. Najlepsza regu³a : 

# ma w poprzedniku {race=White,capital-gain=High,capital-loss=none,native-country=United-States} 
# wsparcie: 2%, 
# dla którego zaufanie wynosi oko³o 67%, 
# a lift 4.215

# co oznacza, ¿e 2% transakcji zawiera wszystkie elementy z bazy, a w 67% z nich wy¿szy próg dochodowy
# determinowany jest pozytywnie przez wyliczone elementy: rasa-bia³a, zysk kapita³owy wysoki, brak strat
# kapita³owych, pochodzenie z USA. 

# regu³y w których wystêpuje element dochodowy wy¿szy - wstêpna analiza
setsIncome <- subset(aRules, subset = items %in% "income=large")
summary(setsIncome)
inspect(head(sort(setsIncome, by="support"),10))

rule_data <- DATAFRAME(setsIncome, 
                       separate = TRUE, 
                       setStart = '', 
                       itemSep = ',', 
                       setEnd = '')

View(rule_data)
# Regu³, w których wystêpuje wy¿szy próg dochodowy bez wskazania jego pozycji jest 10453, wsparcie
# maksymalnie osi±ga próg oko³o 11%, a bior±c pod uwagê lift mozna stwierdziæ, ¿e czê¶æ regu³ wp³ywa 
# pozytywnie a cze¶æ negatywnie na pojawienie siê nastêpnika (nieokre¶lonego). Analiza jest bardzo ogólnikowa.

# regu³y z zadanym nastêpnikiem - analiza w³a¶ciwa
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
inspect(head(sort(rulesInGivenConseq, by="support"),10))
summary(rulesInGivenConseq)
plot(rulesInGivenConseq, method = "graph", engine = "htmlwidget")
plot(rulesInGivenConseq, method = "paracoord", engine = "default")
# Znaleziono wiêcej regu³ (59 regu³ asocjacyjnych) ni¿ we wstêpnej analizie dla okre¶lonego nastêpnika.
# Najlepsza regu³a jest taka sama, a serwer nie jest tak obci±¿aony jak podczas wyszukiwania wszystkich regu³
# z transakcji w celu manualnego filtrowania. W podsumowaniu mo¿na wyczytaæ, ¿e wsparcie maksymalne
# wynosi 2%, zaufanie oko³o 68%, a wszytskie regu³y przedstawiaj± pozytywny wp³yw na wyszukiwanie wy¿szego
# progu dochodowego. Za pomoc± wykresu mo¿na miêdzy innymi podejrzeæ jakie elementy pojawiaj± siê przed
# income=large w ró¿nych regu³ach. S± to: bia³a rasa, zysk kapita³owy wysoki, przepracowywanie nadgodzin, pochodzenie z USA,
# brak straty kapita³owej, status po ¶lubie, status mê¿a, p³eæ meska, wiek ¶redni, praca w sektorze prywatnym.

# regu³y oparte o czêste zbiory maksymalne
maxRul <- rulesInGivenConseq[is.maximal(rulesInGivenConseq) == TRUE]
summary(maxRul)
inspect(head(sort(maxRul, by="support"),10))
plot(maxRul, method = "graph", engine = "htmlwidget")
plot(maxRul, method = "paracoord", engine = "default")
plot(maxRul, shading="order", control=list(main = "Two-key plot" ))
# Zmiejszy³a siê liczba regu³ do 24. Najistotniejsza regu³a ma teraz wsparcie oko³o 1,6%, zaufanie oko³o 68%.
# W porównaniu do poprzedniej regu³y intersuj±cej dodany zosta³ element p³ci mêskiej. 

#usuniêcie regu³ nadmiarowych (dla których istnieje prostsza regu³a, która ma wsparcie takie samo dla
# nastêpnika i niemniejsze zaufanie)
notRedun <- rulesInGivenConseq[is.redundant(rulesInGivenConseq) == FALSE]
summary(notRedun)
inspect(head(sort(notRedun, by="support"),10))
# nie wystêpowa³y redundantne regu³y

# Pog³êbiona analiza wzglêdem interesuj±cego maksymalnego zbioru, dla kilku elementów nie wystêpuj±cych
# z wymuszeniem ich wystêpowania ze zmienionymi proporcjonalnie warto¶ciami parametów dla wsparcia (przyk³adowo zbiory
# 2 razy mniejsze otrzymaj± 2 razy mniejsz± warto¶æ wsparcia) oraz dodatkowo obni¿onym zaufaniem,
# z uwzglêdnienim mniej licznych zbiorów

# p³eæ ¿eñska
summary(AdultUCI$sex)
# p³eæ ¿eñska wystêpuje 2 razy rzadziej w zbiorze
aParam@confidence = 0.5
aParam@support = 0.005
aParam@minlen = 2L
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsSex <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","sex=Female"))
summary(setsSex)
inspect(head(sort(setsSex, by="support"),10))
# nie ma regu³
# korekta wsparcia
aParam@support = 0.001
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsSex <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","sex=Female"))
summary(setsSex)
inspect(head(sort(setsSex, by="support"),10))

# p³eæ z regu³y interesuj±cej (mêska)
aParam@support = 0.01
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsSex <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","sex=Male"))
summary(setsSex)
inspect(head(sort(setsSex, by="support"),10))

# P³eæ ¿eñska wystêpuje do¶æ czêsto w ca³ym zbiorze. Mo¿na wnioskowaæ na podsatwie tego elementu, ¿e p³eæ
# wykazuje wp³yw na wyniki. Dla wsparcia maksymalnego zbiór jest 5 razy mniejszy, a uwzglêdniaj±c, ¿e
# liczno¶æ grupy ¿eñskiej jest 2 razy mniejsza mo¿na by siê spodziewaæ 2.5 razy wiêkszego zbioru ni¿
# otrzyma³am gdyby p³eæ nie mia³a wp³ywu - aby zachowaæ proporcje (mniej-wiecej).


# rasa druga pod wzglêdem liczno¶ci (black)
summary(AdultUCI$race)
# rasa czarna wystêpuje oko³o 9 razy rzadziej, co zaookr±glê do 10 dla w³asnej wygody
aParam@support = 0.001
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsRace <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","race=Black"))
summary(setsRace)
inspect(head(sort(setsRace, by="support"),10))

# rasa z regu³y intersuj±cej (white)
aParam@support = 0.01
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsRace <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","race=White"))
summary(setsRace)
inspect(head(sort(setsRace, by="support"),10))

# Dla wsparcia maksymalnego zbiór jest oko³o 15 razy mniejszy, a uwzglêdniaj±c, ¿e liczno¶æ grupy dla rasy
# czarnej jest 10 razy mniejsza mozna by siê spodziewaæ 1.5 razy wiêkszego zbioru ni¿ otrzyma³am gdyby rasa nie mia³a wp³ywu - aby
# zachowaæ proporcje (mniej-wiecej). Wed³ug rasy te¿ mo¿na wzglêdnie wnioskowaæ na podsatwie tych wyników,
# choæ zbiory s± na tyle od siebie ró¿ne, ¿e mo¿na mówiæ o pewnych wstêpnych tendencjach, wartych 
# pog³êbienia dla wiêkszych i równiejszych grup. 


# kraj pochodzenia drugi pod wzglêdem liczno¶ci (Meksyk)
summary(AdultUCI$`native-country`)
# kraj pochodzenia Meksyk wystêpuje oko³o 44 razy rzadziej ni¿ USA
aParam@support = 0.0002
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsCountry <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","native-country=Mexico"))
summary(setsCountry)
inspect(head(sort(setsCountry, by="support"),10))
# nie ma regu³

# korekta wsparcia
aParam@support = 0.00002
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsCountry <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","native-country=Mexico"))
summary(setsCountry)
inspect(head(sort(setsCountry, by="support"),10))

# kraj pochodzenia z regu³y intersuj±cej(USA)
summary(AdultUCI)
aParam@confidence = 0.5
aParam@support = 0.01
aParam@minlen = 2L
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsCountry <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","native-country=United-States"))
summary(setsCountry)
inspect(head(sort(setsCountry, by="support"),10))

# Dla wsparcia maksymalnego zbiór jest oko³o 212 razy mniejszy, a uwzglêdniaj±c, ¿e liczno¶æ grupy dla kraju
# Meksyk jest 44 razy mniejsza mo¿na by siê spodziewaæ 4-5 razy wiêkszego zbioru ni¿ otrzyma³am - aby
# zachowaæ proporcje (mniej-wiecej). Zbiory s± tak ró¿noliczne, ¿e nie bêdê ich porównywaæ. 

# Dla wszystkich dodatkowych analiz wykryto regu³y tylko pozytywnie skorelowane z wyst±pieniem wy¿szego progu 
# dochodowego.

############################################################################################

# WNIOSKI:

# W eksperymencie odkryto ragu³y asocjacyjne. Analizowanie ich, wyobra¿am sobie, mo¿na wykorzystaæ w ró¿nych 
# 
# dziedzinach np. nauki, biznesu czy publicystyki, mniej lub bardziej stronniczo. Na podsatwie regu³ mo¿na 
# 
# stworzyæ pewien profil wp³ywaj±cy na pozytywne okre¶lanie jednostek o wy¿szym progu dochodowym. Nauka np. 
# 
# socjologia mog³aby wykorzystaæ dane do analiz spo³eczeñstwa, biznes np. banki do profilowania potencjalnych 
# 
# klientów do lokat czy oceniania kredytobiorców, a publicystyka do np. chwytliwych artyku³ów. Moim zdaniem
# 
# obiektywne interpretowanie regu³ powinno uwzglêdniæ fakt znalezienia regu³ pozytywnie skorelowanych przy
# 
# równoczesnym braku znalezienia regu³ negatywnie skorelowanych. Oznacza to, ¿e wyst±pienie okre¶lonych 
# 
# elementów nie determinuje automatycznie wystêpowania te¿ ich przeciwstawieñst. Przyk³adowo w regu³ach znajduje
# 
# siê element dotycz±cy p³ci, wi±¿±cy p³eæ mêsk± z regu³ami czêstymi, ale równocze¶nie nie wystêpuj± regu³y
# 
# wi±¿±ce p³eæ ¿eñsk± z negatywnym wp³ywem na wystepowanie wy¿szego progu dochodowego. Przy wnioskowaniu warto
# 
# wzi±c pod uwagê te¿ liczno¶ci grup. Atrybut "rasa" mierzy siê z takim problemem. Podobnie atrybut "kraj
# 
# pochodzenia" pojawiaj±cy siê w regu³ach mo¿e byæ problematyczny dla wniskowania, gdy¿ a¿ 44665 rekordów 
# 
# z 48812 jest przypisanych do USA, przez co ca³y zbiór jest automatycznie dostosowany bardziej do wnioskowania
# 
# na temat pochodzenia z tego jednego kraju.
