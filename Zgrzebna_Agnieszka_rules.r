# AGNIESZKA ZGRZEBNA

############################################################################################

# CEL EKSPERYMENTU:
# 
# Odkrycie zale�no�ci mi�dzy wy�sz� klas� dochodow�(income=large w nast�pniku), a innymi zmiennymi.  
# 
# Regu�y interesuj�ce: wsparcie 15%, zaufanie 80%, 4-5 cech po lewej stronie.
# 
# Najlepsz� regu�� jest ta, kt�ra spe�nia powy�sze za�o�enia i ma najwy�sze wsparcie. 

############################################################################################

# EKSPERYMENT:

# �adowanie bibliotek
library(arules)
library(arulesViz)

# Zbi�r danych:
data("AdultUCI") 

# wst�pne zapoznanie si� z danymi
dim(AdultUCI)
View(AdultUCI)
summary(AdultUCI)
str(AdultUCI)

# Przetwarzanie danych:

# atrybuty tylko z warto�ci� unikaln�
which(sapply(AdultUCI, function(x) {length(unique(x))}) == 1) #nie wyst�puj�

# atrybuty z warto�ciami tylko unikalnymi
which(sapply(AdultUCI, function(x) {length(unique(x))}) == nrow(AdultUCI)) #nie wyst�puj�

# zduplikowane wiersze
length(which(duplicated(AdultUCI) == TRUE))

# usuniecie zduplikowanych wierszy
AdultUCI <- unique(AdultUCI)
length(which(duplicated(AdultUCI) == TRUE))
dim(AdultUCI)

# znalezienie w zbiorze AdultUCI atrybut�w z brakuj�cymi danymi i uzupe�nienie danych
# �redni� (dla warto�ci ci�g�ych) lub warto�ci� dominuj�c� (dla warto�ci dyskretnych)
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

# usuni�cie niepotrzebnych atrybut�w
AdultUCI[["fnlwgt"]] <- NULL 
AdultUCI[["education-num"]] <-NULL

# dyskretyzacja atrybut�w ci�g�ych
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

# Wykrywanie regu� asocjacyjnych:

# przekszta�enie postaci relacyjnej na transakcyjn�
adultTR <- as(AdultUCI, "transactions")
inspect(head(adultTR))
summary(adultTR)

# ustalenie adekwatnego wsparcia wzgl�dnego

# cz�sto�� wyst�powania element�w z min 15% wsparcia wzgl�dnego
freqTbl  = itemFrequency(adultTR, type = "relative")
freqTbl = sort(freqTbl, decreasing= TRUE)
print(freqTbl[freqTbl>0.15])
itemFrequencyPlot(adultTR, type ="relative", support= 0.15)

# wizualizacja income
pie(table(AdultUCI$"income"))
barplot(table(AdultUCI$"income"), cex.names=0.7)

# liczba cz�stych element�w od 1% do 100% z skokiem 5pp dla ca�ej bazy transakcji 
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
      geom=c("point","line"), xlab="Wsparcie", ylab="Liczba cz�stych element�w")+ theme_bw()

# Postanowi�am zmieni� definicj� regu�y interesuj�cej dla wsparcia na 5% z uwagi na to, �e wy�sza 
# klasa dochodowa stanowi zaledwie oko�o 16% w ca�ej bazie danych. Dodatkowo na tle ca�ej bazy transakcji
# mniejsze wsparcie wydaje si� bardziej adekwatne z uwagi na spor� liczb� rekord�w i ich atrybut�w. 

# tworzenie parametru dla metody apriori zgodnie z regu�ami interesuj�cymi
aParam  = new("APparameter", "confidence" = 0.8, "support" =0.05, minlen = 5L, maxlen = 6L, 
              maxtime = 20, "target" = "rules")
aParam

# tworzenie regu� asocjacyjnych za pomoc� funkcji apriori bez zak�adania nast�pnika - wst�pna analiza
aRules <-apriori(adultTR,aParam)
summary(aRules)
inspect(head(sort(aRules, by="support"),10))
inspectDT(head(aRules,10000))
# Dla za�o�onych regu� wy�szy pr�g dochodowy nie wyst�puje jako nast�pnik, ale pojawia si� w poprzedniku.
# Oznacza to, �e nie znaleziono element�w wp�ywaj�cych na sukces ekonomiczny, ale (na chwil� obecn�) za
# jego pomoc� mo�na przewidywa� wyst�powanie innych element�w z nim powi�zanych. 

# korekta regu� interesuj�cych
aParam@confidence = 0.6
aParam@support = 0.01
aParam
aRules <-apriori(adultTR,aParam)
summary(aRules)
inspectDT(head(aRules,10000))
# Dla zmienionych warto�ci (wsparcie wzgl�dne 1%, zaufanie 60%) uda�o si� znale�� 37 interesuj�cych regu�,
# po przefiltrowaniu regu� za pomoc� inspectDT dla wy�szego progu dochodowego w nast�pniku. Najlepsza regu�a : 

# ma w poprzedniku {race=White,capital-gain=High,capital-loss=none,native-country=United-States} 
# wsparcie: 2%, 
# dla kt�rego zaufanie wynosi oko�o 67%, 
# a lift 4.215

# co oznacza, �e 2% transakcji zawiera wszystkie elementy z bazy, a w 67% z nich wy�szy pr�g dochodowy
# determinowany jest pozytywnie przez wyliczone elementy: rasa-bia�a, zysk kapita�owy wysoki, brak strat
# kapita�owych, pochodzenie z USA. 

# regu�y w kt�rych wyst�puje element dochodowy wy�szy - wst�pna analiza
setsIncome <- subset(aRules, subset = items %in% "income=large")
summary(setsIncome)
inspect(head(sort(setsIncome, by="support"),10))

rule_data <- DATAFRAME(setsIncome, 
                       separate = TRUE, 
                       setStart = '', 
                       itemSep = ',', 
                       setEnd = '')

View(rule_data)
# Regu�, w kt�rych wyst�puje wy�szy pr�g dochodowy bez wskazania jego pozycji jest 10453, wsparcie
# maksymalnie osi�ga pr�g oko�o 11%, a bior�c pod uwag� lift mozna stwierdzi�, �e cz�� regu� wp�ywa 
# pozytywnie a cze�� negatywnie na pojawienie si� nast�pnika (nieokre�lonego). Analiza jest bardzo og�lnikowa.

# regu�y z zadanym nast�pnikiem - analiza w�a�ciwa
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
inspect(head(sort(rulesInGivenConseq, by="support"),10))
summary(rulesInGivenConseq)
plot(rulesInGivenConseq, method = "graph", engine = "htmlwidget")
plot(rulesInGivenConseq, method = "paracoord", engine = "default")
# Znaleziono wi�cej regu� (59 regu� asocjacyjnych) ni� we wst�pnej analizie dla okre�lonego nast�pnika.
# Najlepsza regu�a jest taka sama, a serwer nie jest tak obci��aony jak podczas wyszukiwania wszystkich regu�
# z transakcji w celu manualnego filtrowania. W podsumowaniu mo�na wyczyta�, �e wsparcie maksymalne
# wynosi 2%, zaufanie oko�o 68%, a wszytskie regu�y przedstawiaj� pozytywny wp�yw na wyszukiwanie wy�szego
# progu dochodowego. Za pomoc� wykresu mo�na mi�dzy innymi podejrze� jakie elementy pojawiaj� si� przed
# income=large w r�nych regu�ach. S� to: bia�a rasa, zysk kapita�owy wysoki, przepracowywanie nadgodzin, pochodzenie z USA,
# brak straty kapita�owej, status po �lubie, status m�a, p�e� meska, wiek �redni, praca w sektorze prywatnym.

# regu�y oparte o cz�ste zbiory maksymalne
maxRul <- rulesInGivenConseq[is.maximal(rulesInGivenConseq) == TRUE]
summary(maxRul)
inspect(head(sort(maxRul, by="support"),10))
plot(maxRul, method = "graph", engine = "htmlwidget")
plot(maxRul, method = "paracoord", engine = "default")
plot(maxRul, shading="order", control=list(main = "Two-key plot" ))
# Zmiejszy�a si� liczba regu� do 24. Najistotniejsza regu�a ma teraz wsparcie oko�o 1,6%, zaufanie oko�o 68%.
# W por�wnaniu do poprzedniej regu�y intersuj�cej dodany zosta� element p�ci m�skiej. 

#usuni�cie regu� nadmiarowych (dla kt�rych istnieje prostsza regu�a, kt�ra ma wsparcie takie samo dla
# nast�pnika i niemniejsze zaufanie)
notRedun <- rulesInGivenConseq[is.redundant(rulesInGivenConseq) == FALSE]
summary(notRedun)
inspect(head(sort(notRedun, by="support"),10))
# nie wyst�powa�y redundantne regu�y

# Pog��biona analiza wzgl�dem interesuj�cego maksymalnego zbioru, dla kilku element�w nie wyst�puj�cych
# z wymuszeniem ich wyst�powania ze zmienionymi proporcjonalnie warto�ciami paramet�w dla wsparcia (przyk�adowo zbiory
# 2 razy mniejsze otrzymaj� 2 razy mniejsz� warto�� wsparcia) oraz dodatkowo obni�onym zaufaniem,
# z uwzgl�dnienim mniej licznych zbior�w

# p�e� �e�ska
summary(AdultUCI$sex)
# p�e� �e�ska wyst�puje 2 razy rzadziej w zbiorze
aParam@confidence = 0.5
aParam@support = 0.005
aParam@minlen = 2L
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsSex <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","sex=Female"))
summary(setsSex)
inspect(head(sort(setsSex, by="support"),10))
# nie ma regu�
# korekta wsparcia
aParam@support = 0.001
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsSex <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","sex=Female"))
summary(setsSex)
inspect(head(sort(setsSex, by="support"),10))

# p�e� z regu�y interesuj�cej (m�ska)
aParam@support = 0.01
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsSex <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","sex=Male"))
summary(setsSex)
inspect(head(sort(setsSex, by="support"),10))

# P�e� �e�ska wyst�puje do�� cz�sto w ca�ym zbiorze. Mo�na wnioskowa� na podsatwie tego elementu, �e p�e�
# wykazuje wp�yw na wyniki. Dla wsparcia maksymalnego zbi�r jest 5 razy mniejszy, a uwzgl�dniaj�c, �e
# liczno�� grupy �e�skiej jest 2 razy mniejsza mo�na by si� spodziewa� 2.5 razy wi�kszego zbioru ni�
# otrzyma�am gdyby p�e� nie mia�a wp�ywu - aby zachowa� proporcje (mniej-wiecej).


# rasa druga pod wzgl�dem liczno�ci (black)
summary(AdultUCI$race)
# rasa czarna wyst�puje oko�o 9 razy rzadziej, co zaookr�gl� do 10 dla w�asnej wygody
aParam@support = 0.001
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsRace <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","race=Black"))
summary(setsRace)
inspect(head(sort(setsRace, by="support"),10))

# rasa z regu�y intersuj�cej (white)
aParam@support = 0.01
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsRace <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","race=White"))
summary(setsRace)
inspect(head(sort(setsRace, by="support"),10))

# Dla wsparcia maksymalnego zbi�r jest oko�o 15 razy mniejszy, a uwzgl�dniaj�c, �e liczno�� grupy dla rasy
# czarnej jest 10 razy mniejsza mozna by si� spodziewa� 1.5 razy wi�kszego zbioru ni� otrzyma�am gdyby rasa nie mia�a wp�ywu - aby
# zachowa� proporcje (mniej-wiecej). Wed�ug rasy te� mo�na wzgl�dnie wnioskowa� na podsatwie tych wynik�w,
# cho� zbiory s� na tyle od siebie r�ne, �e mo�na m�wi� o pewnych wst�pnych tendencjach, wartych 
# pog��bienia dla wi�kszych i r�wniejszych grup. 


# kraj pochodzenia drugi pod wzgl�dem liczno�ci (Meksyk)
summary(AdultUCI$`native-country`)
# kraj pochodzenia Meksyk wyst�puje oko�o 44 razy rzadziej ni� USA
aParam@support = 0.0002
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsCountry <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","native-country=Mexico"))
summary(setsCountry)
inspect(head(sort(setsCountry, by="support"),10))
# nie ma regu�

# korekta wsparcia
aParam@support = 0.00002
aParam
aRules <-apriori(adultTR,aParam)
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "income=large")
setsCountry <- subset(rulesInGivenConseq, subset = items %ain% c("income=large","native-country=Mexico"))
summary(setsCountry)
inspect(head(sort(setsCountry, by="support"),10))

# kraj pochodzenia z regu�y intersuj�cej(USA)
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

# Dla wsparcia maksymalnego zbi�r jest oko�o 212 razy mniejszy, a uwzgl�dniaj�c, �e liczno�� grupy dla kraju
# Meksyk jest 44 razy mniejsza mo�na by si� spodziewa� 4-5 razy wi�kszego zbioru ni� otrzyma�am - aby
# zachowa� proporcje (mniej-wiecej). Zbiory s� tak r�noliczne, �e nie b�d� ich por�wnywa�. 

# Dla wszystkich dodatkowych analiz wykryto regu�y tylko pozytywnie skorelowane z wyst�pieniem wy�szego progu 
# dochodowego.

############################################################################################

# WNIOSKI:

# W eksperymencie odkryto ragu�y asocjacyjne. Analizowanie ich, wyobra�am sobie, mo�na wykorzysta� w r�nych 
# 
# dziedzinach np. nauki, biznesu czy publicystyki, mniej lub bardziej stronniczo. Na podsatwie regu� mo�na 
# 
# stworzy� pewien profil wp�ywaj�cy na pozytywne okre�lanie jednostek o wy�szym progu dochodowym. Nauka np. 
# 
# socjologia mog�aby wykorzysta� dane do analiz spo�ecze�stwa, biznes np. banki do profilowania potencjalnych 
# 
# klient�w do lokat czy oceniania kredytobiorc�w, a publicystyka do np. chwytliwych artyku��w. Moim zdaniem
# 
# obiektywne interpretowanie regu� powinno uwzgl�dni� fakt znalezienia regu� pozytywnie skorelowanych przy
# 
# r�wnoczesnym braku znalezienia regu� negatywnie skorelowanych. Oznacza to, �e wyst�pienie okre�lonych 
# 
# element�w nie determinuje automatycznie wyst�powania te� ich przeciwstawie�st. Przyk�adowo w regu�ach znajduje
# 
# si� element dotycz�cy p�ci, wi���cy p�e� m�sk� z regu�ami cz�stymi, ale r�wnocze�nie nie wyst�puj� regu�y
# 
# wi���ce p�e� �e�sk� z negatywnym wp�ywem na wystepowanie wy�szego progu dochodowego. Przy wnioskowaniu warto
# 
# wzi�c pod uwag� te� liczno�ci grup. Atrybut "rasa" mierzy si� z takim problemem. Podobnie atrybut "kraj
# 
# pochodzenia" pojawiaj�cy si� w regu�ach mo�e by� problematyczny dla wniskowania, gdy� a� 44665 rekord�w 
# 
# z 48812 jest przypisanych do USA, przez co ca�y zbi�r jest automatycznie dostosowany bardziej do wnioskowania
# 
# na temat pochodzenia z tego jednego kraju.
