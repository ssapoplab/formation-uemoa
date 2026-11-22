# Projet R autonome — Projections démographiques (jour 4)

Formation régionale UEMOA en analyse démographique pour la décision publique ·
Ouagadougou, 26 novembre 2026 · Vissého Adjiwanou, Département de sociologie, UQAM · SSAPOPLAB

Ce dossier fonctionne **sans connexion internet**. Il suffit de R (4.2 ou plus récent)
et du `tidyverse`.

## Démarrer

1. Copier tout le dossier `projet-projection/` sur son ordinateur.
2. Ouvrir `projet-projection.Rproj` dans RStudio.
3. Ouvrir `tp1_projection.R`, remplacer `"Togo"` par le nom de son pays, puis tout exécuter.
4. Les graphiques et tableaux s'écrivent dans `resultats/`.

Si un paquet manque : `install.packages("tidyverse")` (à faire avant l'atelier,
ou depuis la clé USB qui contient les paquets).

## Contenu

| Élément | Rôle |
|:--|:--|
| `tp1_projection.R` | Script du TP : une seule ligne à modifier, le nom du pays |
| `R/fonctions_projection.R` | Méthode des composantes : table de mortalité, pas de projection, scénarios, indicateurs, besoins, contrôle par rapport à l'ONU |
| `donnees/` | Huit fichiers CSV du WPP 2024 pour les huit pays de l'UEMOA |
| `simulations/bayespop_uemoa.rds` | 1 000 trajectoires probabilistes par pays, 2025-2100 |
| `excel/TP1_projection_UEMOA.xlsx` | La même projection en formules Excel, pour qui préfère Excel |
| `modeles/modele_population_recensement.csv` | Modèle pour utiliser la population de son recensement |
| `preparation/` | Scripts du formateur : extraction des données, simulations, figures du cours, intrants Excel |

## Vérifications faites

- Le scénario moyen reproduit la variante moyenne de l'ONU à moins de 0,3 % près
  en 2050, pour les huit pays.
- Le classeur Excel donne exactement les mêmes résultats que le script R
  (vérifié pour le Togo et le Mali, avec et sans migration, pour trois scénarios).

## Sources

Nations Unies, Division de la population (2024), *World Population Prospects 2024*,
via le paquet R [`wpp2024`](https://github.com/PPgp/wpp2024). Simulations probabilistes :
paquets `bayesTFR`, `bayesLife` et `bayesPop` (Ševčíková, Raftery et coll.).
