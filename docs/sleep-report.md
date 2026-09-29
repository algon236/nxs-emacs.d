# Søvnrapport, version 18

Blodtryk vises i to grafer på samme side: sys og dia, hver med morgen,
middag og aften. Alle numeriske målepunkter forbindes, også punkter markeret
med kryds. Manglende og ikke-numeriske felter indgår ikke; ret dem i Org-kilden.

For hver af de seks serier beregnes først gennemsnittet af alle numeriske
målinger. Punkter mere end 7,5 % fra dette gennemsnit for sys eller 15 % for
dia markeres med kryds. Frasortering foretages kun én gang. Punkter præcis
på grænsen medtages. Der er ingen fast grænse ved sys 160 eller dia 60.

Kun de medtagne punkter bruges til tabellens gennemsnit, stikprøvens
standardafvigelse (n−1) og lineære tendens. Tendensen tegnes mellem første
og sidste medtagne måledag. Tabellen viser antal medtagne og udeladte punkter.
SD kræver mindst to målinger; ellers vises en streg. En tendens kræver mindst
to forskellige måledage. Grafernes y-akser tilpasses uafhængigt, så punkter
ikke skjules af faste aksegrænser. Reglen udpeger ikke dokumenterede målefejl;
spredningen gælder kun de medtagne data og er ikke apparatets måleusikkerhed.

## Eksisterende LaTeX-skabeloner

Erstat den gamle tikzpicture-blok for blodtryk med:

```tex
\input{generated/blood-pressure-page.tex}
```

Behold sideskiftet før og efter blodtrykssiden. Den nye side kræver de
samme PGFPlots-pakker og `reportaxis`-stil som hidtil samt `\SleepPeriod`.
Den gamle `generated/plot-blood-pressure.tex` genereres fortsat af hensyn
til ældre skabeloner, men den nye side kræver ovenstående ændring.

Generér rapporten igen med F8 fra Org-filen. Historiske rapporter og
arkiverede skabeloner ændres ikke automatisk.

Version 18 bruger tabularray med mørkegrønt tabelhoved, grønne rækker og
gitter som rapportens øvrige tabeller. Skabelonen skal indlæse tabularray og
xcolor med svgnames. I Medicin Status er købsdatokolonnen udvidet ved at
reducere pladsen i præparat- og antal-kolonnerne.
