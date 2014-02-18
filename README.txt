Wymagania
=========

Projekt został przetestowany w systemie Linux, dystrybucja Ubuntu 12.04 LTS. Do
skompilowania projektu potrzebne są:
 * program make
 * aktualna platforma GHC, tzn. kompilator GHC oraz narzędzie cabal.

Kompilacja
==========

Wystarczy wydać polecenie make będąc połączonym z Internetem.  Narzędzie cabal
powinno automatycznie pobrać wszystkie wymagane biblioteki, a następnie
skompilować projekt. W efekcie w podkatalogu bin/ pojawią się trzy pliki
wykonywalne:
* CloudAtlas
* Client
* CATool

Wszystkie pliki wykonywalne wyświetlają krótkie komunikaty pomocy

Generowanie kluczy i certyfikatów
=================================

Przykład:

# Utwórz bazę danych dla CA w katalogu db:
$ CATool db --init

# Utwórz certyfikaty dla stref poziomu 1:
$ CATool db --create-zone /uw
$ CATool db --create-zone /pjwstk

# Utwórz certyfikaty dla podstref strefy /uw:
$ CATool db --create-zone /uw/violet07
$ CATool db --create-zone /uw/khaki13
$ CATool db --create-zone /uw/khaki31

# Utwórz bazę danych dla kolejnego CA odpowiadającego strefie /pjwstk:
$ CATool db/pjwstk --init /pjwstk

# Utwórz certyfikaty dla podstref strefy /pjwstk:
$ CATool db/pjwstk --create-zone /whatever01
$ CATool db/pjwstk --create-zone /whatever02

# Utwórz certyfikat dla klienta, który może modyfikować tylko:
#  Strefy /uw/khaki13 i /uw/khaki31
#  Atrybuty num_processes i max_processes
$ CATool db/pjwstk --client-cert "/uw/khaki13,/uw/khaki31" "num_processes,max_processes"

Program CATool tworzy pliki:
 * ca.pub i ca.priv w głównym katalogu bazy danych
   - klucze CA
 * zone.cert i zone.priv w podkatalogach bazy danych
   - certyfikat i klucz prywatny strefy
 * client_*.cert i client_*.priv w katalogu wywołania
   - certyfikat i klucz klienta

Zawartość plików ca.pub, zone.cert i zone.priv należy
skopiować do odpowiednich pól pliku konfiguracyjnego serwera.

Sposób użycia
=============

Aby uruchomić agenta, należy wydać polecenie:

$ CloudAtlas <plik konfiguracyjny>

Przykładowy plik konfiguracyjny (server.ini) znajduje się w głównym katalogu
projektu.

Aby uruchomić klienta, należy wydać jedno z poleceń:

$ Client <client.cert> <client.priv> <plik konfiguracyjny>

Gdzie <client.cert> i <client.priv> to ścieżki do
odpowiednich plików wygenerowanych przez CA.

Przykładowy plik konfiguracyjny to client.ini. Po tym poleceniu klient będzie
działał w trybie daemon - tzn. będzie w określonych odstępach przesyłał
atrybuty do ustalonego w pliku konfiguracyjnym agenta.


Aby używać klienta interaktywnie, należy wydać polecenie:

$ Client <client.cert> <client.priv> <serwer>:<port>

W tym trybie klient będzie przyjmował polecenia ze standardowego wyjścia i
komunikował się z agentem nasłuchującym na <serwer>:<port>. Lista dostępnych
poleceń zostanie wyświetlona po podaniu nieprawidłowego polecenia, np. 'help'.

Dodatkowo w podkatalogu bin/ znajduje się skrypt forall.sh. Sposób użycia jest
następujący:

$ forall.sh <plik z poleceniami> <client.cert> <client.priv> <host1>:<port1> [ <host2>:<port2> [ ... ] ]

Skrypt usiłuje wykonać wszystkie polecenia z <plik z poleceniami>, łącząc się
najpierw z <host1>:<port1>, następnie z <host2>:<port2> itd.

Testowanie
==========

W podkatalogu test/ znajdują się pliki instalujące i usuwające zapytanie, które
generuje atrybut num_processes.

Aby przeprowadzić test z użyciem tego zapytania, należy:
* Uruchomić 5 instancji programu CloudAtlas
* Uruchomić 5 instancji programu Client działających w trybie daemon
* Ustawić zapasowe kontakty przynajmniej niektórym z uruchomionych agentów:
    $ Client <host1>:<port1>
    > set_contacts <host2>:<port2> [ <host3>:<port3> ... ]
    OK
* Wygenerować odpowiedni certyfikat (np. bez ograniczeń):
    $ CATool db --client-cert "*" "*"
* Zainstalować zapytanie w dowolnej strefie 
    bin/Client client_*.cert client_*.priv <host1>:<port1> < test/num_processes
* Sprawdzić atrybuty wybranej strefy:
    $ bin/Client client_*.cert client_*.priv <host1>:<port1>
    > zone /
    ...
    num_processes : integer = 624
    ...

Ograniczenia
============
W niniejszym rozwiązaniu nie zaimplementowano:
* mechanizmu ACC.
* mechanizmu przeterminowania certyfikatów.
