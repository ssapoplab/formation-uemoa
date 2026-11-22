# Labo du lundi 23 novembre — Calculer les indicateurs démographiques de son pays

- `jour1-labo-indicateurs.Rmd` : le labo en R (tidyverse). Changez `mon_pays` au début, puis cliquez sur **Knit**.
- `jour1-labo-indicateurs.xlsx` : le même calcul dans Excel. Choisissez le pays dans la cellule jaune B2 de l'onglet « Calculs ».
- `donnees/jour1_tableaux_uemoa_2023.csv` : population, naissances et décès par groupe d'âge, 8 pays, 2023.
- `donnees/wpp2024_uemoa_indicateurs.csv` : valeurs officielles de l'ONU (ISF, espérance de vie), pour vérifier les calculs.

Source : Nations Unies, *World Population Prospects 2024* (paquet R `wpp2024`). Les tableaux par âge sont reconstitués en appliquant les taux de fécondité et de mortalité par âge du WPP aux populations au 1er juillet 2023, puis regroupés en groupes quinquennaux. Données agrégées et publiques.

Seul le paquet `tidyverse` est nécessaire.
