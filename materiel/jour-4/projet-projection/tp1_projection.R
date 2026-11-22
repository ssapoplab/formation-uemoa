# =============================================================================
# TP 1 — Projeter la population de son pays à 2050
# Formation UEMOA · Jour 4 · Projections démographiques
#
# Mode d'emploi :
#   1. Ouvrir projet-projection.Rproj dans RStudio.
#   2. Changer le nom du pays à la ligne ci-dessous, et rien d'autre.
#   3. Tout exécuter (Ctrl + Maj + Entrée, ou Cmd + Maj + Entrée sur Mac).
#   Les résultats s'écrivent dans le dossier resultats/.
#
# Pays possibles : "Bénin", "Burkina Faso", "Côte d'Ivoire", "Guinée-Bissau",
#                  "Mali", "Niger", "Sénégal", "Togo"
# =============================================================================

pays <- "Togo"

# -----------------------------------------------------------------------------
# Rien à modifier en dessous de cette ligne
# -----------------------------------------------------------------------------
source("R/fonctions_projection.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(ggplot2))
dir.create("resultats", showWarnings = FALSE)
nom_court <- gsub("[^A-Za-z]+", "_", iconv(pays, "UTF-8", "ASCII//TRANSLIT"))
fichier <- function(x) file.path("resultats", paste0(nom_court, "_", x))

# 1. Données et hypothèses du pays
d    <- lire_donnees("donnees")
intr <- intrants_pays(d, pays)

# 2. Quatre scénarios de fécondité, migration de l'hypothèse moyenne de l'ONU
proj <- projeter_scenarios(intr, migration = TRUE)

# 3. Contrôle : notre scénario « moyenne » doit reproduire la variante moyenne de l'ONU
controle <- comparer_onu(proj, d) %>% filter(scenario == "moyenne")
print(controle %>% select(annee, notre_projection = pop_totale, onu = onu_variante_moyenne, ecart_pct))

# 4. Indicateurs de synthèse
ind <- indicateurs(proj)
print(ind %>% filter(annee %in% c(2025, 2050)) %>%
        select(scenario, annee, pop_totale, pct_moins_15, rapport_dependance, naissances_an),
      n = Inf)

# 5. Besoins pour l'éducation primaire et l'emploi
bes <- besoins(proj)
print(bes %>% filter(annee %in% c(2025, 2035, 2050)) %>%
        select(scenario, annee, eleves_primaire, enseignants, salles_classe, nouveaux_actifs_an),
      n = Inf)

# 6. Graphiques
ordre <- c("constante", "haute", "moyenne", "basse")
g1 <- ggplot(ind, aes(annee, pop_totale / 1000, colour = factor(scenario, ordre))) +
  geom_line(linewidth = 1) + geom_point() +
  scale_colour_manual(values = COULEURS_SCENARIOS) +
  labs(title = paste(pays, ": population selon quatre hypothèses de fécondité"),
       x = NULL, y = "Population (millions)", colour = "Fécondité",
       caption = "Source : calculs à partir du WPP 2024 (Nations Unies).") +
  theme_minimal(base_size = 13)
ggsave(fichier("population_scenarios.png"), g1, width = 9, height = 5, dpi = 150)

pyr <- proj %>%
  filter(scenario == "moyenne", annee %in% c(2025, 2050)) %>%
  mutate(age = forcats::fct_collapse(age, "85+" = GROUPES[18:21]),
         pop = if_else(sexe == "hommes", -pop, pop)) %>%
  group_by(annee, sexe, age) %>% summarise(pop = sum(pop), .groups = "drop")
g2 <- ggplot(pyr, aes(pop, age, fill = sexe)) +
  geom_col(width = 0.9) + facet_wrap(~annee) +
  scale_x_continuous(labels = function(x) format(abs(x), big.mark = " ")) +
  scale_fill_manual(values = c(femmes = "#a51c24", hommes = "#5b6670")) +
  labs(title = paste(pays, ": pyramide des âges, scénario moyen"),
       x = "Effectif (milliers)", y = NULL, fill = NULL) +
  theme_minimal(base_size = 13)
ggsave(fichier("pyramides.png"), g2, width = 10, height = 6, dpi = 150)

g3 <- ggplot(bes, aes(annee, eleves_primaire / 1000, colour = factor(scenario, ordre))) +
  geom_line(linewidth = 1) + geom_point() +
  scale_colour_manual(values = COULEURS_SCENARIOS) +
  labs(title = paste(pays, ": élèves du primaire (6-11 ans) à scolariser"),
       subtitle = "Taux de scolarisation de 90 %",
       x = NULL, y = "Élèves (millions)", colour = "Fécondité") +
  theme_minimal(base_size = 13)
ggsave(fichier("eleves_primaire.png"), g3, width = 9, height = 5, dpi = 150)

# 7. Tableaux à reprendre dans la note pays
write_csv(ind, fichier("indicateurs.csv"))
write_csv(bes, fichier("besoins.csv"))
write_csv(proj, fichier("projection_age_sexe.csv"))
cat("\nTerminé. Résultats dans le dossier resultats/ :\n"); print(list.files("resultats", pattern = nom_court))
