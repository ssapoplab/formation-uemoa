# =============================================================================
# Figures du cours du jour 4 (à exécuter depuis le dossier projet-projection/)
# Sorties : ../figures/*.png
# =============================================================================
source("R/fonctions_projection.R", encoding = "UTF-8")
suppressPackageStartupMessages({ library(ggplot2); library(forcats) })

d <- lire_donnees("donnees")
dossier_fig <- "../figures"
dir.create(dossier_fig, showWarnings = FALSE)
rouge <- "#a51c24"; rouge_fonce <- "#7f1d1d"; gris <- "#5b6670"
theme_cours <- theme_minimal(base_size = 15) +
  theme(plot.title = element_text(face = "bold", colour = rouge_fonce),
        plot.title.position = "plot", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))
sauver <- function(g, f, w = 11, h = 5.8) ggsave(file.path(dossier_fig, f), g, width = w, height = h, dpi = 150, bg = "white")
src <- "Source : Nations Unies, World Population Prospects 2024."

# 1. Population 1990-2100 : médiane et intervalles à 80 % et 95 % -----------
pt <- d$onu_pop %>% mutate(pays = fct_reorder(pays, -pop_mediane, .fun = max))
g <- ggplot(pt, aes(annee, pop_mediane / 1000)) +
  geom_ribbon(aes(ymin = pop_95_inf / 1000, ymax = pop_95_sup / 1000), fill = rouge, alpha = 0.15) +
  geom_ribbon(aes(ymin = pop_80_inf / 1000, ymax = pop_80_sup / 1000), fill = rouge, alpha = 0.25) +
  geom_line(colour = rouge_fonce, linewidth = 0.9) +
  geom_vline(xintercept = 2025, linetype = 2, colour = gris) +
  facet_wrap(~pays, nrow = 2, scales = "free_y") +
  labs(title = "La population des huit pays continuera de croître jusqu'en 2100",
       subtitle = "Médiane, intervalles de prédiction à 80 % et 95 % (millions)",
       x = NULL, y = NULL, caption = src) + theme_cours
sauver(g, "fig1_population_intervalles.png", h = 6.2)

# 2. ISF : estimations, variante moyenne et variantes ±0,5 -------------------
isf <- d$isf %>% mutate(annee = as.integer(substr(periode, 1, 4)) + 2.5)
g <- ggplot(isf, aes(annee, isf_moyenne)) +
  geom_ribbon(aes(ymin = isf_95_inf, ymax = isf_95_sup), fill = rouge, alpha = 0.15) +
  geom_line(aes(y = isf_basse), linetype = 2, colour = gris) +
  geom_line(aes(y = isf_haute), linetype = 2, colour = gris) +
  geom_line(colour = rouge_fonce, linewidth = 0.9) +
  geom_hline(yintercept = 2.1, colour = gris, linewidth = 0.3) +
  facet_wrap(~pays, nrow = 2) +
  labs(title = "Partout, l'ONU prévoit une baisse lente de la fécondité",
       subtitle = "ISF (enfants par femme) : variante moyenne, variantes haute et basse (tirets), intervalle à 95 %",
       x = NULL, y = NULL, caption = src) + theme_cours
sauver(g, "fig2_isf_hypotheses.png", h = 6.2)

# 3. Projections des huit pays (scénario moyen et autres) --------------------
proj <- map_dfr(PAYS_UEMOA, ~ projeter_scenarios(intrants_pays(d, .x)))
ind  <- indicateurs(proj)

# 3a. Inertie : part de la population de 2050 déjà née en 2025
inertie <- proj %>% filter(scenario == "moyenne", annee == 2050) %>%
  mutate(debut = as.integer(sub("[-+].*", "", age)),
         statut = if_else(debut >= 25, "Déjà nés en 2025", "Nés après 2025")) %>%
  group_by(pays, statut) %>% summarise(pop = sum(pop), .groups = "drop") %>%
  group_by(pays) %>% mutate(part = 100 * pop / sum(pop)) %>% ungroup()
ordre <- inertie %>% filter(statut == "Déjà nés en 2025") %>% arrange(part) %>% pull(pays)
inertie <- inertie %>% mutate(statut = factor(statut, c("Nés après 2025", "Déjà nés en 2025")))
g <- ggplot(inertie, aes(part, factor(pays, ordre), fill = statut)) +
  geom_col(width = 0.7) +
  geom_text(data = filter(inertie, statut == "Déjà nés en 2025"),
            aes(x = part / 2, label = paste0(round(part), " %")), colour = "white", fontface = "bold") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("Déjà nés en 2025" = rouge_fonce, "Nés après 2025" = "#d9c3c3")) +
  labs(title = "Environ la moitié des habitants de 2050 sont déjà nés",
       subtitle = "Population de 2050 selon l'année de naissance, scénario moyen (%)",
       x = NULL, y = NULL, fill = NULL, caption = "Source : projection par composantes à partir du WPP 2024.") +
  theme_cours + theme(legend.position = "top")
sauver(g, "fig3_inertie.png", h = 5.4)

# 3b. UEMOA : population totale selon les quatre scénarios
uem <- ind %>% group_by(scenario, annee) %>% summarise(pop = sum(pop_totale) / 1000, .groups = "drop")
fin <- uem %>% filter(annee == 2050)
g <- ggplot(uem, aes(annee, pop, colour = scenario)) +
  geom_line(linewidth = 1.1) + geom_point() +
  geom_text(data = fin, aes(label = paste0(scenario, " : ", round(pop), " M")), hjust = -0.08, show.legend = FALSE) +
  scale_colour_manual(values = COULEURS_SCENARIOS, guide = "none") +
  scale_x_continuous(limits = c(2025, 2056), breaks = seq(2025, 2050, 5)) +
  labs(title = paste0("Selon la fécondité, l'UEMOA compterait de ", round(min(fin$pop)), " à ", round(max(fin$pop)), " millions d'habitants en 2050"),
       subtitle = "Population des huit pays (millions), quatre hypothèses de fécondité, mortalité et migration de l'ONU",
       x = NULL, y = NULL, caption = "Source : projection par composantes à partir du WPP 2024.") + theme_cours
sauver(g, "fig4_uemoa_scenarios.png")

# 3c. Besoins : élèves du primaire par scénario, UEMOA
bes <- besoins(proj) %>% group_by(scenario, annee) %>%
  summarise(eleves = sum(eleves_primaire) / 1000, enseignants = sum(enseignants), .groups = "drop")
g <- ggplot(bes, aes(annee, eleves, colour = scenario)) +
  geom_line(linewidth = 1.1) + geom_point() +
  geom_text(data = filter(bes, annee == 2050), aes(label = paste0(scenario, " : ", round(eleves, 1), " M")),
            hjust = -0.08, show.legend = FALSE) +
  scale_colour_manual(values = COULEURS_SCENARIOS, guide = "none") +
  scale_x_continuous(limits = c(2025, 2056), breaks = seq(2025, 2050, 5)) +
  labs(title = "Le nombre d'élèves dépend de la fécondité dès 2035",
       subtitle = "Élèves du primaire (6-11 ans) avec 90 % de scolarisation, UEMOA (millions)",
       x = NULL, y = NULL, caption = "Source : projection par composantes à partir du WPP 2024.") + theme_cours
sauver(g, "fig5_uemoa_eleves.png")

# 4. Rapport de dépendance 1990-2100 : la fenêtre du dividende ---------------
dep <- d$pop %>% mutate(debut = as.integer(sub("[-+].*", "", age))) %>%
  group_by(pays, annee) %>%
  summarise(dep = 100 * sum(pop_milliers[debut < 15 | debut >= 65]) / sum(pop_milliers[debut >= 15 & debut < 65]),
            .groups = "drop")
g <- ggplot(dep, aes(annee, dep, colour = pays)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 60, linetype = 2, colour = gris) +
  annotate("text", x = 1992, y = 57, label = "Seuil indicatif de 60", hjust = 0, colour = gris) +
  labs(title = "Le rapport de dépendance baisse : la fenêtre du dividende s'ouvre",
       subtitle = "Personnes de moins de 15 ans et de 65 ans et plus pour 100 personnes de 15-64 ans, variante moyenne",
       x = NULL, y = NULL, colour = NULL, caption = src) + theme_cours
sauver(g, "fig6_dependance.png")

# 5. Pyramide UEMOA 2025 et 2050, scénario moyen ----------------------------
pyr <- proj %>% filter(scenario == "moyenne", annee %in% c(2025, 2050)) %>%
  mutate(age = fct_collapse(age, "85+" = GROUPES[18:21])) %>%
  group_by(annee, sexe, age) %>% summarise(pop = sum(pop) / 1000, .groups = "drop") %>%
  mutate(pop = if_else(sexe == "hommes", -pop, pop))
g <- ggplot(pyr, aes(pop, age, fill = sexe)) + geom_col(width = 0.9) + facet_wrap(~annee) +
  scale_x_continuous(labels = function(x) abs(x)) +
  scale_fill_manual(values = c(femmes = rouge, hommes = gris)) +
  labs(title = "En 2050, la pyramide de l'UEMOA reste jeune mais s'élargit au milieu",
       subtitle = "Population par âge et sexe, huit pays, scénario moyen (millions)",
       x = NULL, y = NULL, fill = NULL, caption = "Source : projection par composantes à partir du WPP 2024.") +
  theme_cours + theme(legend.position = "top")
sauver(g, "fig7_pyramide_uemoa.png", h = 6.4)

# 6. Contrôle : notre projection reproduit l'ONU ------------------------------
ctrl <- map_dfr(PAYS_UEMOA, ~ comparer_onu(projeter(intrants_pays(d, .x)), d)) %>% filter(annee == 2050)
print(ctrl %>% select(pays, pop_totale, onu_variante_moyenne, ecart_pct))
print(uem %>% filter(annee == 2050)); print(bes %>% filter(annee %in% c(2025, 2050)))
print(inertie %>% filter(statut == "Déjà nés en 2025"))

# 7. Diagramme de Lexis : deux lectures de 5Lx --------------------------------
library(patchwork)
vert <- "#1D9E75"; orange <- "#D85A30"; bande <- "#9FE1CB"
base_lexis <- function() {
  ggplot() + coord_equal(xlim = c(0, 20), ylim = c(0, 20), expand = FALSE) +
    theme_cours + theme(panel.grid = element_blank(), axis.ticks = element_line(colour = gris),
                        plot.title = element_text(size = 14), plot.subtitle = element_text(size = 11.5))
}
# A. Suivi de cohorte : 5Lx = années vécues entre les âges x et x+5
nais <- seq(0, 1, by = 0.25)
gA <- base_lexis() +
  annotate("rect", xmin = 0, xmax = 20, ymin = 10, ymax = 15, fill = "grey92") +
  annotate("polygon", x = c(10, 11, 16, 15), y = c(10, 10, 15, 15), fill = bande, colour = vert, linewidth = 0.8) +
  annotate("segment", x = nais, y = 0, xend = nais + 19, yend = 19, colour = gris, linetype = 2, linewidth = 0.4) +
  annotate("segment", x = 0, xend = 1, y = 0, yend = 0, colour = vert, linewidth = 2.5) +
  annotate("text", x = 1.5, y = 1.2, label = "l[0]~naissances", parse = TRUE, hjust = 0, colour = rouge_fonce, size = 4.2) +
  annotate("text", x = 1.5, y = 13.1, label = "{}[5]*L[x]", parse = TRUE, hjust = 0, colour = rouge_fonce, size = 5) +
  annotate("text", x = 1.5, y = 11.5, label = "années vécues", hjust = 0, colour = gris, size = 3.6) +
  scale_x_continuous(breaks = 0.5, labels = "naissance") +
  scale_y_continuous(breaks = c(10, 15), labels = c("x", "x+5")) +
  labs(title = "Suivi de cohorte : une durée",
       subtitle = "Années vécues par une génération entre les âges x et x+5",
       x = "Temps", y = "Âge")
# B. Population stationnaire : 5Lx = personnes âgées de x à x+5 à une date
gB <- base_lexis() +
  annotate("segment", x = seq(-20, 19, 1), y = 0, xend = seq(-20, 19, 1) + 20, yend = 20,
           colour = "grey80", linewidth = 0.3) +
  annotate("polygon", x = c(10, 10, 15, 15), y = c(5, 10, 15, 10), fill = bande, alpha = 0.5) +
  annotate("segment", x = 10, xend = 10, y = 5, yend = 10, colour = vert, linewidth = 2.5) +
  annotate("segment", x = 15, xend = 15, y = 10, yend = 15, colour = orange, linewidth = 2.5) +
  annotate("text", x = 9.4, y = 7.5, label = "{}[5]*L[x-5]", parse = TRUE, hjust = 1, colour = vert, size = 5) +
  annotate("text", x = 15.6, y = 12.5, label = "{}[5]*L[x]", parse = TRUE, hjust = 0, colour = orange, size = 5) +
  annotate("text", x = 15.6, y = 10.9, label = "personnes", hjust = 0, colour = gris, size = 3.6) +
  annotate("text", x = 0.5, y = 19, label = "l[0]~naissances~chaque~année", parse = TRUE, hjust = 0, colour = rouge_fonce, size = 4) +
  scale_x_continuous(breaks = c(10, 15), labels = c("t", "t+5")) +
  scale_y_continuous(breaks = c(5, 10, 15), labels = c("x-5", "x", "x+5")) +
  labs(title = "Population stationnaire : un effectif",
       subtitle = "Personnes âgées de x à x+5 à une date",
       x = "Temps", y = NULL)
g <- gA + gB + plot_annotation(
  caption = "En population stationnaire, la bande diagonale et le segment vertical contiennent le même nombre.\nLa part du segment vert qui atteint le segment orange est donc 5Lx / 5Lx-5.",
  theme = theme(plot.caption = element_text(colour = rouge_fonce, size = 14, hjust = 0.5, face = "bold")))
sauver(g, "fig8_lexis_5Lx.png", w = 10, h = 5.9)
