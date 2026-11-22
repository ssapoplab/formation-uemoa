# =============================================================================
# Simulations probabilistes pour les huit pays de l'UEMOA
# Formation UEMOA · Jour 4 · Projections démographiques
#
# À exécuter UNE FOIS par le formateur (environ 1 heure sur 2 cœurs).
# Paquets : bayesTFR, bayesPop, snowFT (calcul parallèle), wpp2024.
#
# Seule la FÉCONDITÉ est probabiliste ici (bayesTFR). La mortalité (espérance
# de vie médiane) et la migration suivent les hypothèses moyennes de l'ONU :
# bayesLife ne projette pas, sans le module VIH (bayesLifeHIV), la Côte d'Ivoire,
# la Guinée-Bissau et le Togo. À l'horizon 2050, la fécondité est de loin la
# principale source d'incertitude.
#
# Chaînes volontairement COURTES (2 chaînes de 6 000 itérations) : résultats
# à usage pédagogique. Pour un usage officiel, utiliser les intervalles publiés
# par l'ONU (donnees/onu_population_totale.csv) ou des chaînes bien plus longues.
#
# Sortie : simulations/bayespop_uemoa.rds, une liste à deux éléments :
#   trajectoires : pays, annee, trajectoire, pop_totale, pop_0_14, pop_15_64,
#                  pop_65_plus, age_primaire (6-11 ans), en milliers
#   resume       : quantiles 2,5 %, 10 %, 50 %, 90 %, 97,5 % par pays et année
# =============================================================================

library(bayesTFR); library(bayesPop)
library(dplyr); library(tidyr); library(purrr)

dossier_sim <- "simulations/calcul"          # gros fichiers intermédiaires, hors du projet distribué
dir.create(dossier_sim, recursive = TRUE, showWarnings = FALSE)
it <- 6000; bi <- 2000; ntraj <- 1000

# 1. Fécondité : phases II et III, puis projection --------------------------
# Les jeux quinquennaux de wpp2024 sont observés jusqu'en 2015-2020 :
# la projection commence donc à la période 2020-2025 (start.year = 2023).
run.tfr.mcmc(output.dir = file.path(dossier_sim, "tfr"), iter = it, nr.chains = 2,
             wpp.year = 2024, present.year = 2023, parallel = TRUE, replace.output = TRUE)
run.tfr3.mcmc(sim.dir = file.path(dossier_sim, "tfr"), iter = it, nr.chains = 2, thin = 5,
              parallel = TRUE, replace.output = TRUE)
tfr.predict(sim.dir = file.path(dossier_sim, "tfr"), end.year = 2100, start.year = 2023,
            burnin = bi, burnin3 = bi, nr.traj = ntraj, replace.output = TRUE)

# 2. Population : méthode des composantes sur chaque trajectoire de l'ISF ---
uemoa <- c(`Bénin` = 204, `Burkina Faso` = 854, `Côte d'Ivoire` = 384, `Guinée-Bissau` = 624,
           Mali = 466, Niger = 562, `Sénégal` = 686, Togo = 768)
pop.predict(end.year = 2100, start.year = 1950, present.year = 2020, wpp.year = 2024,
            countries = uemoa, nr.traj = ntraj,
            inputs = list(tfr.sim.dir = file.path(dossier_sim, "tfr"),
                          e0F.file = "median_", e0M.file = "median_"),
            output.dir = file.path(dossier_sim, "pop"), replace.output = TRUE)

# 3. Extraction des trajectoires utiles au labo -----------------------------
pred <- get.pop.prediction(file.path(dossier_sim, "pop"))
traj_expr <- function(expr, nom) {
  m <- get.pop.ex(expr, pred)                     # matrice : années x trajectoires
  m <- matrix(m, nrow = dim(m)[1], dimnames = list(dimnames(m)[[1]], NULL))
  tibble(annee = as.integer(rownames(m))) %>%
    bind_cols(as_tibble(m, .name_repair = ~ paste0("t", seq_along(.x)))) %>%
    pivot_longer(-annee, names_to = "trajectoire", values_to = nom) %>%
    mutate(trajectoire = as.integer(sub("t", "", trajectoire)))
}
trajectoires <- imap_dfr(uemoa, function(code, nom) {
  # Indices d'âge de bayesPop : 1 = 0-4, 2 = 5-9, ..., 27 = 130+
  c <- code
  list(traj_expr(paste0("P", c), "pop_totale"),
       traj_expr(paste0("P", c, "[1:3]"), "pop_0_14"),
       traj_expr(paste0("P", c, "[4:13]"), "pop_15_64"),
       traj_expr(paste0("P", c, "[14:27]"), "pop_65_plus"),
       traj_expr(paste0("P", c, "[2]"), "p5_9"),
       traj_expr(paste0("P", c, "[3]"), "p10_14")) %>%
    reduce(left_join, by = c("annee", "trajectoire")) %>%
    mutate(pays = nom, .before = 1)
}) %>%
  filter(annee >= 2025) %>%
  mutate(age_primaire = 0.8 * p5_9 + 0.4 * p10_14) %>%
  select(-p5_9, -p10_14)

resume <- trajectoires %>%
  pivot_longer(pop_totale:age_primaire, names_to = "indicateur", values_to = "valeur") %>%
  group_by(pays, annee, indicateur) %>%
  summarise(q025 = quantile(valeur, 0.025), q10 = quantile(valeur, 0.10),
            mediane = median(valeur), q90 = quantile(valeur, 0.90),
            q975 = quantile(valeur, 0.975), .groups = "drop")

saveRDS(list(trajectoires = trajectoires, resume = resume,
             note = paste("Simulations bayesTFR + bayesPop, WPP 2024 : fécondité probabiliste (2 chaînes de", it,
                          "itérations, 1 000 trajectoires), mortalité et migration selon les hypothèses moyennes de l'ONU.",
                          "Usage pédagogique.")),
        "simulations/bayespop_uemoa.rds", compress = "xz")
