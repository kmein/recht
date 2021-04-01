# Der `recht` [ʀɛçt] Gesetzbrowser
[Gesetze](https://www.gesetze-im-internet.de/) finden und lesen auf der Kommandozeile.

## Bedienung
Um alle Gesetze aufzulisten:

    $ recht list | head -n 5
    [A/KAE] Ausführungsanordnung zur Konzessionsabgabenanordnung
    [AABG] Gesetz zur Begrenzung der Arzneimittelausgaben der gesetzlichen Krankenversicherung
    [AAG] Gesetz über den Ausgleich der Arbeitgeberaufwendungen für Entgeltfortzahlung
    [AAppO] Approbationsordnung für Apotheker
    [AarhusÜbk] Übereinkommen über den Zugang zu Informationen, die Öffentlichkeitsbeteiligung an Entscheidungsverfahren und den Zugang zu Gerichten in Umweltangelegenheiten

Mithilfe der üblichen Verdächtigen können Gesetze zu bestimmten Themen gefunden oder die Ausgabe anderweitig modifiziert werden:

    $ recht list | grep -i berufsausbildung | head -n 5
    [ÄndSchnAusbV] Verordnung über die Berufsausbildung zum Änderungsschneider/zur Änderungsschneiderin
    [AgrarAusbStEignV] Verordnung über die Eignung der Ausbildungsstätte für die Berufsausbildung zur Fachkraft Agrarservice
    [AgrarAusbV] Verordnung über die Berufsausbildung zur Fachkraft Agrarservice
    [AMAusbV 2004] Verordnung über die Berufsausbildung zum Aufbereitungsmechaniker/zur Aufbereitungsmechanikerin
    [ArbMDFAngAusbV] Verordnung über die Berufsausbildung zum Fachangestellten für Arbeitsmarktdienstleistungen und zur Fachangestellten für Arbeitsmarktdienstleistungen

Das Inhaltsverzeichnis eines Gesetzes findet sich via:

    $ recht list UrhG | head -n 5
    Inhaltsübersicht
    § 1 – Allgemeines
    § 2 – Geschützte Werke
    § 3 – Bearbeitungen
    § 4 – Sammelwerke und Datenbankwerke

Um einen einzelnen Paragraphen zu lesen, lässt sich das Programm wie folgt aufrufen:

    $ recht get GG 1
    Art 1

    (1) Die Würde des Menschen ist unantastbar. Sie zu achten und zu schützen ist Verpflichtung aller staatlichen Gewalt.

    (2) Das Deutsche Volk bekennt sich darum zu unverletzlichen und unveräußerlichen Menschenrechten als Grundlage jeder menschlichen Gemeinschaft, des Friedens und der Gerechtigkeit in der Welt.

    (3) Die nachfolgenden Grundrechte binden Gesetzgebung, vollziehende Gewalt und Rechtsprechung als unmittelbar geltendes Recht.

Um ein Gesetz vollständig zu lesen, kann die Paragraphennummer weggelassen werden:

    $ recht get BlauzungenV
    Verordnung zum Schutz gegen die Blauzungenkrankheit (BlauzungenV)
    Ausfertigungsdatum: 22.03.2002

    § 1 – Begriffsbestimmungen

    (1) Im Sinne dieser Verordnung liegen vor:

    1.
    Blauzungenkrankheit ...

Für einen zufälligen Paragraphen aus diesem faszinierenden Gesetz:

    $ recht random BlauzungenV
    BlauzungenV
    Verordnung zum Schutz gegen die Blauzungenkrankheit
    § 6 – Vorschriften für Sperrgebiet und Beobachtungsgebiet

    Wer in einem Sperrgebiet oder einem Beobachtungsgebiet empfängliche Tiere hält, hat dies und den Standort der Tiere unverzüglich nach Bekanntgabe der Festsetzung nach § 5 Absatz 4 der zuständigen Behörde anzuzeigen.

Die Schranken dieses einen Gesetzes hinter sich lassend, können auch zufällige Paragraphen aus der gesamten Gesetzeslandschaft angezeigt werden:


    $ recht random
    BSG 2000
    Gesetz zur Bestimmung der Beitragssätze und zur Bestimmung der Umrechnungsfaktoren für den Versorgungsausgleich in der gesetzlichen Rentenversicherung für 2000  (Beitragssatzgesetz 2000 - BSG 2000)
    § 1 – Beitragssätze in der Rentenversicherung

    Der Beitragssatz für das Jahr 2000 beträgt in der Rentenversicherung der Arbeiter und der Angestellten 19,3 vom Hundert und in der knappschaftlichen Rentenversicherung 25,6 vom Hundert.

## Cache

Die Daten von gesetze-im-internet.de werden nach dem Herunterladen für einen Tag im Cache bereitgehalten, sodass folgende Aufrufe von `recht` sowohl schneller sind als auch keinen Internetzugang benötigen.

## Entwicklung
- Kompilation: `nix-build`
- GHCI Demon: `nix-shell --attr env default.nix --run ghcid`
- REPL: `nix-shell --attr env default.nix --run 'cabal repl'`
