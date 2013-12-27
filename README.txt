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
skompilować projekt. W efekcie w podkatalogu bin/ pojawią się dwa pliki
wykonywalne:
* CloudAtlas
* Client

Sposób użycia
=============

Aby uruchomić agenta, należy wydać polecenie:

$ CloudAtlas <plik konfiguracyjny>

Przykładowy plik konfiguracyjny (server.ini) znajduje się w głównym katalogu
projektu.

Aby uruchomić klienta, należy wydać jedno z poleceń:

$ Client <plik konfiguracyjny>

Przykładowy plik konfiguracyjny to client.ini. Po tym poleceniu klient będzie
działał w trybie daemon - tzn. będzie w określonych odstępach przesyłał
atrybuty do ustalonego w pliku konfiguracyjnym agenta.


Aby używać klienta interaktywnie, należy wydać polecenie:

$ Client <serwer>:<port>

W tym trybie klient będzie przyjmował polecenia ze standardowego wyjścia i
komunikował się z agentem nasłuchującym na <serwer>:<port>. Lista dostępnych
poleceń zostanie wyświetlona po podaniu nieprawidłowego polecenia, np. 'help'.

Dodatkowo w podkatalogu bin/ znajduje się skrypt forall.sh. Sposób użycia jest
następujący:

$ forall.sh <plik z poleceniami> <host1>:<port1> [ <host2>:<port2> [ ... ] ]

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
* Zainstalować zapytanie we wszystkich strefach wszystkich agentów:
    bin/forall.sh test/install_num_processes <host1>:<port1> ... <host5>:<port5>
* Sprawdzić atrybuty wybranej strefy:
    $ Client <host1>:<port1>
    > zone /
    ...
    num_processes : integer = 624
    ...
