# =============================================================================
# Fonctions de projection par la méthode des composantes
# Formation UEMOA · Jour 4 · Projections démographiques
#
# Toutes les fonctions lisent les CSV du dossier donnees/ (WPP 2024).
# Groupes d'âge quinquennaux de 0-4 à 95-99, puis 100+ (21 groupes).
# Pas de projection : 5 ans, du 1er juillet 2025 au 1er juillet 2050.
# Effectifs en milliers.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
})

GROUPES  <- c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+")
PERIODES <- c("2025-2030", "2030-2035", "2035-2040", "2040-2045", "2045-2050")
PAYS_UEMOA <- c("Bénin", "Burkina Faso", "Côte d'Ivoire", "Guinée-Bissau",
                "Mali", "Niger", "Sénégal", "Togo")

# -----------------------------------------------------------------------------
# 1. Lecture des données
# -----------------------------------------------------------------------------
lire_donnees <- function(dossier = "donnees") {
  lire <- function(f) read_csv(file.path(dossier, f), show_col_types = FALSE)
  list(
    pop      = lire("pop_age_sexe.csv"),
    mx       = lire("mortalite_mx.csv"),
    isf      = lire("fecondite_isf.csv"),
    calend   = lire("fecondite_calendrier.csv"),
    rmn      = lire("rapport_masculinite_naissance.csv"),
    mig      = lire("migration_age_sexe.csv"),
    onu_pop  = lire("onu_population_totale.csv"),
    onu_e0   = lire("onu_esperance_vie.csv")
  )
}

# -----------------------------------------------------------------------------
# 2. Table de mortalité abrégée (âges 0, 1, 5, 10, ..., 100+)
#    à partir des taux de mortalité mx
# -----------------------------------------------------------------------------
table_mortalite <- function(mx, sexe = c("femmes", "hommes")) {
  sexe <- match.arg(sexe)
  n  <- c(1, 4, rep(5, 19), NA)            # amplitude des intervalles
  m0 <- mx[1]
  # Années vécues par les décédés dans les deux premiers intervalles
  # (formules de Coale et Demeny, modèle Ouest)
  if (sexe == "femmes") {
    a0 <- if (m0 >= 0.107) 0.350 else 0.053 + 2.800 * m0
    a1 <- if (m0 >= 0.107) 1.361 else 1.522 - 1.518 * m0
  } else {
    a0 <- if (m0 >= 0.107) 0.330 else 0.045 + 2.684 * m0
    a1 <- if (m0 >= 0.107) 1.352 else 1.651 - 2.816 * m0
  }
  # Âge 0 et 1-4 : formule classique avec ax ;
  # âges 5 et plus : force de mortalité constante dans l'intervalle
  qx <- numeric(22); Lx <- numeric(22)
  qx[1:2] <- n[1:2] * mx[1:2] / (1 + (n[1:2] - c(a0, a1)) * mx[1:2])
  qx[3:21] <- 1 - exp(-5 * mx[3:21])
  qx[22] <- 1
  lx <- cumprod(c(1, 1 - qx[-22]))
  dx <- lx * qx
  Lx[1:2]  <- n[1:2] * lx[2:3] + c(a0, a1) * dx[1:2]
  Lx[3:21] <- dx[3:21] / mx[3:21]
  Lx[22]   <- lx[22] / mx[22]                # groupe ouvert 100+
  Tx <- rev(cumsum(rev(Lx)))
  tibble(age_debut = c(0, 1, seq(5, 100, 5)), mx, qx, lx, Lx, Tx, ex = Tx / lx)
}

# Passage aux groupes quinquennaux : rapports de survie S(x) et survie
# des naissances jusqu'à 0-4 ans
survies_quinquennales <- function(tm) {
  L5 <- c(tm$Lx[1] + tm$Lx[2], tm$Lx[3:21])          # 0-4, 5-9, ..., 95-99
  T5 <- c(tm$Tx[1], tm$Tx[3:22])                     # T(0), T(5), ..., T(100)
  list(
    S        = L5[-1] / L5[-20],                     # 0-4 -> 5-9, ..., 90-94 -> 95-99
    S_ouvert = T5[21] / T5[20],                      # (95-99 et 100+) -> 100+
    s0       = L5[1] / 5,                            # naissances -> 0-4 ans  (5L0 / 5 l0)
    e0       = tm$ex[1]
  )
}

# -----------------------------------------------------------------------------
# 3. Intrants d'un pays : population de départ et hypothèses par période
# -----------------------------------------------------------------------------
intrants_pays <- function(d, pays_choisi, annee_depart = 2025) {
  stopifnot(pays_choisi %in% PAYS_UEMOA)

  P0 <- d$pop %>%
    filter(pays == pays_choisi, annee == annee_depart) %>%
    mutate(age = factor(age, levels = GROUPES)) %>%
    arrange(sexe, age)
  P0 <- list(femmes = P0$pop_milliers[P0$sexe == "femmes"],
             hommes = P0$pop_milliers[P0$sexe == "hommes"])

  survie <- map(PERIODES, function(p) {
    m <- d$mx %>% filter(pays == pays_choisi, periode == p) %>% arrange(sexe, age_debut)
    list(femmes = survies_quinquennales(table_mortalite(m$mx[m$sexe == "femmes"], "femmes")),
         hommes = survies_quinquennales(table_mortalite(m$mx[m$sexe == "hommes"], "hommes")))
  }) %>% set_names(PERIODES)

  calendrier <- map(PERIODES, function(p) {
    d$calend %>% filter(pays == pays_choisi, periode == p) %>% arrange(age) %>% pull(pct_naissances) / 100
  }) %>% set_names(PERIODES)

  migration <- map(PERIODES, function(p) {
    m <- d$mig %>% filter(pays == pays_choisi, periode == p) %>%
      mutate(age = factor(age, levels = GROUPES)) %>% arrange(sexe, age)
    list(femmes = m$migrants_nets_milliers[m$sexe == "femmes"],
         hommes = m$migrants_nets_milliers[m$sexe == "hommes"])
  }) %>% set_names(PERIODES)

  isf <- d$isf %>% filter(pays == pays_choisi)
  isf_depart <- isf$isf_moyenne[isf$periode == "2020-2025"]
  isf <- isf %>% filter(periode %in% PERIODES) %>% arrange(periode)

  rmn <- d$rmn %>% filter(pays == pays_choisi, periode %in% PERIODES) %>% arrange(periode) %>% pull(rmn)

  list(pays = pays_choisi, P0 = P0, survie = survie, calendrier = calendrier,
       migration = migration, rmn = rmn,
       isf = list(moyenne = isf$isf_moyenne, basse = isf$isf_basse,
                  haute = isf$isf_haute, constante = rep(isf_depart, length(PERIODES))))
}

# Remplacer la population de départ de l'ONU par celle d'un recensement.
# Le fichier CSV doit contenir les colonnes sexe (femmes / hommes), age
# (0-4, 5-9, ..., 80-84, 85+) et pop_milliers, au 1er juillet 2025.
# Le groupe 85+ est réparti entre 85-89 et 100+ selon la structure de l'ONU.
remplacer_population_depart <- function(intr, fichier) {
  rec <- read_csv(fichier, show_col_types = FALSE)
  for (s in c("femmes", "hommes")) {
    r <- rec %>% filter(sexe == s)
    v <- r$pop_milliers[match(GROUPES[1:17], r$age)]
    if (anyNA(v)) stop("Groupes d'âge manquants pour les ", s, " : vérifier la colonne age.")
    vieux <- r$pop_milliers[r$age == "85+"]
    parts <- intr$P0[[s]][18:21] / sum(intr$P0[[s]][18:21])
    intr$P0[[s]] <- c(v, vieux * parts)
  }
  intr
}

# -----------------------------------------------------------------------------
# 4. Un pas de projection de 5 ans
# -----------------------------------------------------------------------------
# P   : liste(femmes, hommes) de 21 effectifs (0-4 ... 100+) au début de la période
# sv  : liste(femmes, hommes) des survies (sortie de survies_quinquennales)
# fx  : 7 taux de fécondité annuels (15-19 ... 45-49)
# rmn : rapport de masculinité à la naissance
# M   : liste(femmes, hommes) de 21 soldes migratoires nets de la période,
#       par groupe d'âge en fin de période (moitié en début, moitié en fin)
pas_projection <- function(P, sv, fx, rmn, M) {
  Pn <- list()
  for (s in c("femmes", "hommes")) {
    p <- P[[s]]; m <- M[[s]]; S <- sv[[s]]$S
    out <- numeric(21)
    # Étape 1 : survie des cohortes, groupes 5-9 à 95-99
    out[2:20] <- (p[1:19] + m[2:20] / 2) * S + m[2:20] / 2
    # Étape 2 : groupe ouvert 100+
    out[21] <- (p[20] + p[21] + m[21] / 2) * sv[[s]]$S_ouvert + m[21] / 2
    Pn[[s]] <- pmax(out, 0)
  }
  # Étape 3 : naissances de la période (effectif féminin moyen, 15-49 ans)
  w <- 4:10
  naissances <- 5 * sum(fx * (P$femmes[w] + Pn$femmes[w]) / 2)
  # Étapes 4 et 5 : répartition par sexe, survie jusqu'à 0-4 ans, migrants de 0-4 ans
  Pn$femmes[1] <- naissances / (1 + rmn) * sv$femmes$s0 + M$femmes[1]
  Pn$hommes[1] <- naissances * rmn / (1 + rmn) * sv$hommes$s0 + M$hommes[1]
  attr(Pn, "naissances") <- naissances
  Pn
}

# -----------------------------------------------------------------------------
# 5. Projection complète 2025-2050 pour un scénario
# -----------------------------------------------------------------------------
# scenario  : "moyenne", "basse", "haute" (variantes de l'ONU) ou "constante"
# migration : TRUE (hypothèse moyenne de l'ONU) ou FALSE (solde nul)
# isf_perso : facultatif, vecteur de 5 ISF (un par période 2025-2030 à 2045-2050)
#             pour un scénario national ; remplace alors `scenario`
projeter <- function(intr, scenario = "moyenne", migration = TRUE, isf_perso = NULL) {
  if (!is.null(isf_perso)) {
    stopifnot(length(isf_perso) == length(PERIODES))
    intr$isf$personnalise <- isf_perso
    scenario <- "personnalise"
  } else {
    scenario <- match.arg(scenario, c("moyenne", "basse", "haute", "constante"))
  }
  zero <- list(femmes = rep(0, 21), hommes = rep(0, 21))
  annees <- seq(2025, 2050, 5)

  en_tableau <- function(P, an, B = NA_real_) {
    tibble(annee = an, sexe = rep(c("femmes", "hommes"), each = 21),
           age = factor(rep(GROUPES, 2), levels = GROUPES),
           pop = c(P$femmes, P$hommes), naissances_periode = B)
  }

  P <- intr$P0
  res <- list(en_tableau(P, annees[1]))
  for (k in seq_along(PERIODES)) {
    p  <- PERIODES[k]
    fx <- intr$isf[[scenario]][k] * intr$calendrier[[p]] / 5
    M  <- if (migration) intr$migration[[p]] else zero
    P  <- pas_projection(P, intr$survie[[p]], fx, intr$rmn[k], M)
    res[[k + 1]] <- en_tableau(P, annees[k + 1], attr(P, "naissances"))
  }
  bind_rows(res) %>%
    mutate(pays = intr$pays,
           scenario = scenario,
           migration = if (migration) "moyenne ONU" else "nulle",
           .before = 1)
}

# Trajectoire logistique de l'ISF, évaluée au milieu de chaque période
# depart : ISF de 2020-2025 ; final : niveau vers lequel l'ISF tend ;
# annee_inflexion : année de la baisse la plus rapide ; vitesse : k
isf_logistique <- function(depart, final, annee_inflexion = 2030, vitesse = 0.08) {
  t <- seq(2027.5, 2047.5, 5)
  haut <- final + (depart - final) * (1 + exp(vitesse * (2022.5 - annee_inflexion)))
  final + (haut - final) / (1 + exp(vitesse * (t - annee_inflexion)))
}

# Les quatre scénarios de fécondité d'un coup
projeter_scenarios <- function(intr, migration = TRUE) {
  map_dfr(c("constante", "haute", "moyenne", "basse"),
          ~ projeter(intr, .x, migration))
}

# -----------------------------------------------------------------------------
# 6. Indicateurs de synthèse
# -----------------------------------------------------------------------------
indicateurs <- function(proj) {
  proj %>%
    mutate(debut = as.integer(sub("[-+].*", "", age))) %>%
    group_by(pays, scenario, migration, annee) %>%
    summarise(
      pop_totale    = sum(pop),
      pop_0_14      = sum(pop[debut < 15]),
      pop_15_64     = sum(pop[debut >= 15 & debut < 65]),
      pop_65_plus   = sum(pop[debut >= 65]),
      naissances_an = first(naissances_periode[sexe == "femmes"]) / 5,
      .groups = "drop") %>%
    mutate(rapport_dependance = 100 * (pop_0_14 + pop_65_plus) / pop_15_64,
           pct_moins_15 = 100 * pop_0_14 / pop_totale)
}

# -----------------------------------------------------------------------------
# 7. Besoins sectoriels dérivés de la projection
# -----------------------------------------------------------------------------
# Âges du primaire 6-11 ans approchés à partir des groupes quinquennaux :
# 6-9 = 4/5 du groupe 5-9 ; 10-11 = 2/5 du groupe 10-14.
besoins <- function(proj,
                    taux_scolarisation = 0.90,   # part des 6-11 ans scolarisés visée
                    eleves_par_maitre  = 45,     # ratio élèves/maître visé
                    eleves_par_classe  = 50,
                    taux_activite      = 0.65) { # part des 15-64 ans actifs
  proj %>%
    group_by(pays, scenario, migration, annee) %>%
    summarise(
      age_primaire = 0.8 * sum(pop[age == "5-9"]) + 0.4 * sum(pop[age == "10-14"]),
      pop_15_64    = sum(pop[age %in% GROUPES[4:13]]),
      naissances_an = first(naissances_periode[sexe == "femmes"]) / 5,
      .groups = "drop") %>%
    arrange(pays, scenario, migration, annee) %>%
    group_by(pays, scenario, migration) %>%
    mutate(
      eleves_primaire = age_primaire * taux_scolarisation,
      enseignants     = eleves_primaire / eleves_par_maitre,
      salles_classe   = eleves_primaire / eleves_par_classe,
      actifs          = pop_15_64 * taux_activite,
      nouveaux_actifs_an = (actifs - lag(actifs)) / 5) %>%
    ungroup()
}

# -----------------------------------------------------------------------------
# 8. Comparaison avec la projection officielle de l'ONU
# -----------------------------------------------------------------------------
comparer_onu <- function(proj, d) {
  ind <- indicateurs(proj) %>% filter(migration == "moyenne ONU")
  # Variante moyenne (déterministe) : somme des effectifs par âge
  moyenne <- d$pop %>% filter(pays %in% ind$pays, annee %in% ind$annee) %>%
    group_by(pays, annee) %>% summarise(onu_variante_moyenne = sum(pop_milliers), .groups = "drop")
  # Projection probabiliste : médiane et intervalles à 95 %
  proba <- d$onu_pop %>% filter(pays %in% ind$pays, annee %in% ind$annee) %>%
    select(pays, annee, onu_mediane = pop_mediane, onu_95_inf = pop_95_inf, onu_95_sup = pop_95_sup)
  ind %>% select(pays, scenario, annee, pop_totale) %>%
    left_join(moyenne, by = c("pays", "annee")) %>%
    left_join(proba, by = c("pays", "annee")) %>%
    mutate(ecart_pct = 100 * (pop_totale / onu_variante_moyenne - 1))
}

# Couleurs des scénarios, identiques dans tous les graphiques de la journée
COULEURS_SCENARIOS <- c(constante = "#5b6670", haute = "#E69F00",
                        moyenne = "#a51c24", basse = "#0072B2")
