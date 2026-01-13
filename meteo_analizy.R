# Instalowanie paczek
# wybór paczek (uzasadnienie):
# tidyverse-,
# readxd-dane do projektu są w formacie .xlsx, dlatego aby je odtworzyć potrzebna nam będzie ta biblioteka 
install.packages(c("tidyverse", "readxl")) 

# Plik z danymi
# ctrl+alt+b <- otwieranie okna do podglądu danych
'''plik z danymi "temperatura18-20i23.xlsx" zawieta dane meteorologiczne dotyczące
temperatury powietrza na stacjach na obszarze aglomeracji trójmiejskiej z lat 2018-2023 pozyskane z różnych źródeł:
-IMGW,
-Fundacja ARMAG,
-Urząd Miasta Gdańska.
Wybór danych podyktowała znajomość ich struktury- dane z analiz własnych do pracy inżynierskiej.'''

#Wczytywanie danych
meteo <- "temperatura18-20i23.xlsx" 
readxl::read_excel("temperatura18-20i23.xlsx")
readxl::excel_sheets("temperatura18-20i23.xlsx") #sprawdzanie nazw arkuszy

#Wczytywanie danych z arkuszy za pomocą lib readxl
miejsce <- readxl::read_excel(meteo,sheet = "gdzie stacja")
analizy <- readxl::read_excel(meteo,sheet = "analizy 18-21 i 23")
r2023 <- readxl::read_excel(meteo, sheet = "2023")
r2021 <- readxl::read_excel(meteo, sheet = "2021")
r2020 <- readxl::read_excel(meteo, sheet = "2020")
r2019 <- readxl::read_excel(meteo, sheet = "2019")
r2018 <- readxl::read_excel(meteo, sheet = "2018")
sandomierska<- readxl::read_excel(meteo, sheet = "sprawdzenie_czujnik27EMP_O")

#Porządkowanie danych
'''Wyfiltrowanie danych dotyczących tylko pomiarów ze stacji ARMAG i stworzenie osobnego arkusza tylko z nimi.
Usunięcie z nowego arkusza kolumn, które nie będą nas interesować w analizach.
Dodanie do nowego arkusza pustą kolumnę "2022" i umieszczenie jej w dobrym miejscu według chronologii.
Zmiana wielkości liter nagłówków w pliku analizy. 
Stworzenie full_analizy20, przez left_join wyników z sandomierskiej.'''

#odnalezienie w arkuszu "analizy 18-21 i 23" w kolumnie "właściciel" stacji należących do Fundacji ARMAG, przez Filter
dplyr::filter(analizy,Właściciel=="ARMAG") # wiersze które zawierają ARMAG w kolumnie Właściciel wyświetlają się teraz w Consoli (jest ich 35)- wstępna analiza

#Nowa ramka danych na podstawie wystelekcjonowanych stacji należących do Fundacji ARMAG
ANALIZY_ARMAG <- dplyr::filter(analizy,Właściciel=="ARMAG") #tworzy się nowa ramka danych o nazwie "ANALIZY_ARMAG"
ANALIZY_ARMAG[,1:7] #Wybór kolumn i sprawdzenie w consoli, czy są to na pewno te kolumny

#Zachowanie z "ANALIZY_ARMAG" tylko kolumn "Właściciel","ŚREDNIE TEMPERATUR ROCZNE [*C]","2018","2019","2020","2021","2023", które znajdują się w kolumnach [,1:7]
ANALIZY_ARMAG <- dplyr::filter(ANALIZY_ARMAG[,1:7])

#Dodanie pustej kolumny o nazwie "2022" z racji na brak danych dostępnych dla tego roku
ANALIZY_ARMAG$"2022" <- NA

#zmiana kolejności kolumn, tak żeby 2022, był przed 2023
dplyr::select(ANALIZY_ARMAG,"Właściciel","ŚREDNIE TEMPERATUR ROCZNE [*C]","2018","2019","2020","2021","2022","2023") #sprawdzenie działania w consoli
ANALIZY_ARMAG <- dplyr::select(ANALIZY_ARMAG,"Właściciel","ŚREDNIE TEMPERATUR ROCZNE [*C]","2018","2019","2020","2021","2022","2023")#zastosowanie na danych

#Zmiana wszystkich kolumn na pisane wielką literą     
analizy <- dplyr::rename_with(analizy,\(x)toupper(x)) 

full_analiza20 <- dplyr::left_join(r2020, sandomierska, by= dplyr::join_by(...1 == "2020"))

#Analiza danych

'''Wyliczenie średniej rocznej dla wszystkich stacji na dany rok.
Sprawdzenie trendu fluktuacji średnich rocznych temperatur na danych stacjach
wyliczenie trendu przez porównanie średnich ze sobą:
     -jeżeli śr t 2018-śr t 2019= 0 <-stała,
     - jeżeli śr t 2018-śr t 2019< 0 <- trend malejący,
      jeżeli śr t 2018-śr t 2019> 0 <- trend rosnący.
    Najniższa wartość trendu będzie oznaką największej zmiany ujemnej więdzy dwomalatami, a największa- największej dodatniej zmiany wartości. 
W przypadku braku danych dla roku "2022", przypisywana mu będzie na potrzeby zadania średnia temperatura dla tej stacji ze wszystkich lat z pominięciem pól pustych przez użycie: rowMeans(x,na.rm=TRUE).
Jeżeli jakieś pole nie należące do do kolumny "2022" będzie puste, w obliczeniach trendu zostanie pominięte, co będzie skutkować brakiem różnicy dla temperatur dla lat które uwzględniają ten rok. 
Stworzenie nowej kolumny(przez mutate), która mówić nam będzie o ile dana temperatura średnia na danej stacji w danym roku odbiegała od średniej dla danego roku wyliczonej ze wszystkich stacji.'''

summary(ANALIZY_ARMAG) #podsumowanie nowej ramki danych z danymi tylko dla ARMAGu widoczne w consoli

#Obliczanie wartości średniej dla "2022" i przypisanie jej do właściwej kolumny
'''Wykorzystanie samodzielnie napisanej funkcji, której nie wykorzystywaliśmy na zajęciach
     nazwa: rowMeans
     opis: funkcjafunkcja służy do obliczania średniej w wierszu dla ramki danych (lub macierza)
     link do dokumentacji: https://www.rdocumentation.org/packages/fame/versions/1.03/topics/rowMeans?.com'''

ANALIZY_ARMAG$"2022" <- rowMeans(ANALIZY_ARMAG[, c("2018", "2019", "2020", "2021", "2023")], na.rm = TRUE)

#Obliczanie wartości trendu
#Tworzenie nowych kolumn
ANALIZY_ARMAG$delta18_19 <- ANALIZY_ARMAG$"2019"/ANALIZY_ARMAG$"2018"
ANALIZY_ARMAG$delta19_20 <- ANALIZY_ARMAG$"2020"/ANALIZY_ARMAG$"2019"
ANALIZY_ARMAG$delta20_21 <- ANALIZY_ARMAG$"2021"/ANALIZY_ARMAG$"2020"
ANALIZY_ARMAG$delta21_22 <- ANALIZY_ARMAG$"2022"/ANALIZY_ARMAG$"2021"
ANALIZY_ARMAG$delta22_23 <- ANALIZY_ARMAG$"2023"/ANALIZY_ARMAG$"2022"

#Wyliczane wartości średniej dla wszystkic stacji na dany rok ((AM1+AM2+..../35)
summary(ANALIZY_ARMAG[,c("2018", "2019", "2020", "2021","2022", "2023")])

#Sprawdzenie w jakim formacie są dane
str(ANALIZY_ARMAG) #wszystkie kolumny w ANALIZY_ARMAG są numeryczne oprócz dwóch wierwszych

#Wykorzystanie średnich rocznych temperatur do sprawdzenia ile dana wartość od niej odbiega 
ANALIZY_ARMAG <- dplyr::mutate(ANALIZY_ARMAG,
     roznica_2018=ANALIZY_ARMAG$"2018"-11.7,
     roznica_2019=ANALIZY_ARMAG$"2019"-10.6,
     roznica_2020=ANALIZY_ARMAG$"2020"-10.5,
     roznica_2021=ANALIZY_ARMAG$"2021"-9.6,
     roznica_2022=ANALIZY_ARMAG$"2022"-10.2,
     roznica_2023=ANALIZY_ARMAG$"2023"-9.4)

#Opis rezultatów istatystyk
#Wszystkie zamierzone powyżej zadania udało się zrealizować
#wyniki końcowe poniżej są na podstawie statystyk wyświetlających się w Consoli przez funkcję summary
summary(ANALIZY_ARMAG[, c("delta18_19","delta19_20","delta20_21","delta21_22","delta22_23")]) 
dplyr::filter(ANALIZY_ARMAG, delta20_21 < 0.44) 
dplyr::filter(ANALIZY_ARMAG, delta22_23 < 0.44)
dplyr::filter(ANALIZY_ARMAG, delta21_22 > 1.920)

'''Rezultaty analiz dla różnicy temperatur:
Różnice temperatur średnich pomiędzy wszystkimi latami mają tendencję rosnącą.
Najwyższa tendencja rosnąca została odnotowana między rokiem  2021, a 2022r na stacji AM10 i wynosiła różnicę: 1,9*C,
natomiast najmniejsza tendencja wzrostowa rórnież została osiągnięta między 2022, a 2023r na stacji AM10 i na czujnikach GA17 oraz GA21 wynosiła 0,4*C.
(osiągnięcie najniższej różnicy temperatury na dwóch stacjach wynika z zaokrąglenia temperatury do jednego miejsca po przecinku)'''

'''Rezultaty dla średniej temperatury rocznej-pochodzi z funkcji:summary(ANALIZY_ARMAG[,c("2018", "2019", "2020", "2021","2022", "2023")])):
   W roku 2018, średnia temperatura roczna wyliczona ze wszystkich stacji wyniosła:11,7*C.
   W roku 2019, średnia temperatura roczna wyliczona ze wszystkich stacji wyniosła:1o,6*C.
   W roku 2020, średnia temperatura roczna wyliczona ze wszystkich stacji wyniosła:10,5*C.
   W roku 2021, średnia temperatura roczna wyliczona ze wszystkich stacji wyniosła:9,6*C.
   W roku 2022, średnia temperatura roczna wyliczona ze wszystkich stacji wyniosła:10,2*C.
   W roku 2023, średnia temperatura roczna wyliczona ze wszystkich stacji wyniosła:9,4*C. '''
