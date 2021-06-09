# STM-Bank in Haskell und Elm
Projekt für das Modul Functional Program Design (fprod) der FHNW Hochschule für Technik.

![image](https://user-images.githubusercontent.com/36758233/121358206-b5c3fc00-c932-11eb-9ff6-01449ccb4b49.png)

# Beschreibung
In diesem Projekt wurde eine beispielhafte Bankapplikation entwickelt, wie sie auch aus anderen Modulen (vesys, conpr) bekannt ist. Dabei wurden die Software Transactional Memory-Abstraktionen von GHC verwendet, um bei potenziell nebenläufigen Vorgängen deren Atomizität und somit insgesamt die Datenkonsistenz zu garantieren.
Via REST API ist es möglich, neue Bankkonti zu erstellen, Geld in ein Konto einzuzahlen oder abzuheben, Geld zwischen Konti zu überweisen und leere Konti zu schliessen.

Ein Frontend wurde in Elm entwickelt, um mittels GUI mit der Bank interagieren zu können.

Sowohl im Frontend als auch im Backend werden die Eingaben jeweils auf ihr Format und ihre Richtigkeit geprüft oder im Vorfeld begrenzt. Im Frontend bedeutet das z. B. dass in bestimmten Feldern nur Zahlen verwendet werden können, und dass alle relevanten Felder ausgefüllt sein müssen, bevor ein Request abgesendet werden kann.
Im Backend werden die Eingaben noch auf ihre Sinnhaftigkeit geprüft, z. B. wird bei Zahlenangaben unter 0 eine Fehlermeldung an das Frontend gesendet, welches diese dann anzeigt.

# Anleitung
## Voraussetzungen
GHC Version: 8.10.4

cabal-version: 3.0

Nachdem das Repository gecloned wurde können die einzelnen Teile folgend installiert und gestartet werden:
## Backend
### Installation Backend
```
cd Backend
```
```
cabal update
```
### Start Backend-Applikation
```
cabal run stmbank
```
### Start Testsuite
```
cabal run stmbank-test
```
Nach Start ist das Backend unter http://localhost:4000/ erreichbar und kann auch mit curl oder Postman, etc. verwendet werden.

## Frontend
### Installation Frontend
```
cd Frontend
```
```
npm install
```
### Start Frontend-Applikation
```
npm run dev
```
Nach Start ist das Frontend unter http://localhost:8000/ erreichbar.
