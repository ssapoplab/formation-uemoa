# Site de l'atelier UEMOA — Ouagadougou, 23-28 novembre 2026

Site Quarto publié avec GitHub Pages : https://ssapoplab.github.io/formation-uemoa

## Mise en ligne (une seule fois)

1. Dans GitHub Desktop : *File > Add local repository*, choisir ce dossier, puis *Publish repository* sous le nom `formation-uemoa` (décocher « Keep this code private » si le site doit être public).
2. Sur github.com, dans le dépôt : *Settings > Pages > Build and deployment*, Source = *Deploy from a branch*, Branch = `main`, dossier = `/docs`. Enregistrer.

## Mettre le site à jour

1. Modifier les fichiers `.qmd` dans RStudio.
2. Dans le terminal RStudio : `quarto render` (régénère le dossier `docs/`).
3. Dans GitHub Desktop : commit, puis *Push origin*. Le site se met à jour en une à deux minutes.

## Où mettre quoi

| Fichier ou dossier | Contenu |
|---|---|
| `index.qmd` | Accueil : présentation, public, lieu |
| `formateurs.qmd` | Une fiche par formateur ; photos dans `images/` |
| `programme.qmd` | Programme jour par jour avec liens vers le matériel |
| `participants.qmd` | Liste des participants ayant consenti à la publication |
| `materiel/jour-N/` | Diapositives, code R et données d'exercice du jour N |

## Règles pour les formateurs

- Diapositives en Quarto revealjs (`.qmd`) pour les modules en R ; un PDF est accepté sinon.
- Nommer les fichiers `jourN-module-nom.qmd` (ex. `jour2-fecondite-adjiwanou.qmd`).
- Données d'exercice : jamais de microdonnées nominatives ; uniquement des extraits anonymisés autorisés par l'INS concerné.
- `docs/` est généré automatiquement : ne jamais le modifier à la main.
