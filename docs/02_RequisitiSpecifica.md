# Requisiti e specifica

## Business
- Si vuole realizzare un game engine in Scala per poter sperimentare come le funzionalità avanzate di questo linguaggio possono influenzare l'esperienza di sviluppo.

- Il game engine deve essere abbastanza general purpose.
- Deve essere anche semplice da utilizzare.
- Il software deve essere progettato e sviluppato per poter essere evoluto nel tempo.
- Per verificare la buona riuscita del progetto deve essere possibile realizzare almeno due giochi con caratteristiche differenti.

## Modello del dominio
Si è presa ispirazione da altri game engine esistenti e di successo.

Il framework avrà le seguenti caratteristiche:
- Game loop
- Più scene (*scene*)
- Più game object
- Ogni game object ha uno o più comportamenti (*behaviour*) componibili tra di loro
- Devono essere presenti dei behaviour built-in ad esempio per gestire l'input o il rendering grafico
- L'editor sarà testuale
- Un meccanismo per passare informazioni tra una scena e l'altra

![Diagramma delle classi - modello del dominio](./img/Modello%20del%20dominio.png)

## Funzionali

### L'Utente ...
- realizzando i propri behaviour può implementare o meno i seguenti metodi che fungono da punto di aggancio all'engine.
    - Inizializzazione del behaviour
    - Abilitazione del behaviour
    - Avvio del behaviour
    - Chiamata di aggiornamento anticipata per ogni frame
    - Chiamata di aggiornamento
    - Chiamata di aggiornamento posticipata per ogni frame
    - Disabilitazione del behaviour
    - Distruzione del behaviour
- deve aver a disposizione dall'engine dei metodi per:
    - avviarlo e fermarlo
    - cercare specifi oggetti nella scena. (In base ad un identificativo o al behaviour che questi possiedono).
    - creare e distruggere oggetti.
    - abilitare e disabilitare un oggetto.
    - passare da una scena all'altra.
    - memorizzare informazioni che persistano anche al cambio di scena
- avrà a disposizione i seguenti comportamenti built-in:
    - Definizione di un identificativo
    - Definizione della posizione 2D dell'oggetto.
    - Definizione della dimensione 2D dell'oggetto.
    - Definizione di un renderer 2D con sprite e dimensione di visualizzazione
    - Definizione di un collider 2D con dimensione.
    - Possibilità di leggere l'input (tastiera e mouse)
    - Fisica semplificata (velocità e accelerazione)
- avrà a disposizione un insieme di API che semplifichino il testing di dell'engine o degli elementi di gioco

### Sistema
- L'engine chiama dei metodi sui game object in momenti specifici del game loop.
- L'engine mantiene attiva solo una scena alla volta.
- L'engine mette a disposizione un concetto condiviso di framerate/tempo trascorso dall'ultimo frame
- L'engine mette a disposizione la possibilità di salvare in memoria volatile delle informazioni che persistano durante l'esecuzione del gioco.

## Non funzionali
- La flessibilità ed estensibilità del sistema sono le cose più importanti. L'obiettivo è quello di permettere potenzialmente all'utente di allargare le funzionalità del framework.
(Ad esempio si potrebbe introdurre la possibilità di realizzare giochi 3D semplicemente creando i propri behaviour specifici)
- La semplicità e intuitività di utilizzo dell'engine da parte dell'utente (essendo gli sviluppatori anche stakeholders la realizzazione di questo requisito verrà controllata in base alla loro soddisfazione)

## Implementazione
- Dato lo scopo del progetto (dimostrare le competenze aquisite durante l'insegnamento) si utilizzerà Scala.
- Gli svilupattori intendono utilizzare i mixin per realizzare quello che normalmente viene implementato con il pattern Component, in quanto sembra essere un buon campo di applicazione di questo concetto.
- Per ottenere come risultato una codebase solida e di qualità si è deciso di implementare la tecnica del Test Driven Development (TDD).
- Per facilitare il testing dei componenti di gioco e dell'engine si svilupperà un DSL per il suddetto scopo.
