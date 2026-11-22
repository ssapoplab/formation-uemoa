# =============================================================================
# Extraction des données WPP 2024 pour les huit pays de l'UEMOA
# Formation UEMOA · Jour 4 · Projections démographiques
#
# À exécuter UNE FOIS par le formateur, avant l'atelier, avec le paquet
# wpp2024 installé depuis GitHub (remotes::install_github("PPgp/wpp2024")).
# Les participants n'ont besoin que des CSV produits dans donnees/.
#
# Source : Nations Unies, World Population Prospects 2024, jeux de données
# en périodes quinquennales (« 5dt ») du paquet wpp2024. Population au
# 1er juillet ; taux et flux d'une période du 1er juillet t au 30 juin t+5.
# =============================================================================

library(wpp2024)
library(data.table)

pays <- data.table(
  code_pays = c(204, 854, 384, 624, 466, 562, 686, 768),
  pays      = c("Bénin", "Burkina Faso", "Côte d'Ivoire", "Guinée-Bissau",
                "Mali", "Niger", "Sénégal", "Togo")
)
codes <- pays$code_pays
dossier <- "donnees"
dir.create(dossier, showWarnings = FALSE)
ecrire <- function(d, f) fwrite(d, file.path(dossier, f), bom = TRUE)  # BOM : accents lisibles dans Excel

charge <- function(x) { e <- new.env(); data(list = x, package = "wpp2024", envir = e); as.data.table(get(x, e)) }
groupes <- c(paste0(seq(0, 95, 5), "-", seq(4, 99, 5)), "100+")

# 1. Population par âge quinquennal et sexe, 1990-2100 (variante moyenne) ----
est  <- charge("popAge5dt")[country_code %in% codes & year >= 1990, .(code_pays = country_code, annee = year, age, hommes = popM, femmes = popF)]
proj <- charge("popprojAge5dt")[country_code %in% codes & year > 2020, .(code_pays = country_code, annee = year, age, hommes = popM, femmes = popF)]
pop <- rbind(est, proj)
pop <- melt(pop, id.vars = c("code_pays", "annee", "age"), variable.name = "sexe", value.name = "pop_milliers")
pop[, age := factor(age, levels = groupes)]
pop <- merge(pays, pop, by = "code_pays")
pop[, statut := fifelse(annee <= 2020, "estimation", "projection")]
setorder(pop, code_pays, annee, sexe, age)
ecrire(pop[, .(code_pays, pays, annee, statut, sexe, age, pop_milliers = round(pop_milliers, 3))], "pop_age_sexe.csv")

# 2. Quotients de mortalité : taux mx par âge (table abrégée) et sexe -------
mx <- charge("mx5dt")[country_code %in% codes & year >= 2018, .(code_pays = country_code, periode = period, age, hommes = mxM, femmes = mxF)]
mx <- melt(mx, id.vars = c("code_pays", "periode", "age"), variable.name = "sexe", value.name = "mx")
mx[, age_debut := as.integer(sub("\\+", "", age))]
mx <- merge(pays, mx, by = "code_pays")
setorder(mx, code_pays, periode, sexe, age_debut)
ecrire(mx[, .(code_pays, pays, periode, sexe, age_debut, mx = signif(mx, 6))], "mortalite_mx.csv")

# 3. Fécondité : ISF par période et variantes, calendrier par âge ------------
tfr <- charge("tfrproj5dt")[country_code %in% codes, .(code_pays = country_code, periode = period,
        isf_moyenne = tfr, isf_basse = tfr_low, isf_haute = tfr_high,
        isf_80_inf = tfr_80l, isf_80_sup = tfr_80u, isf_95_inf = tfr_95l, isf_95_sup = tfr_95u)]
tfr_est <- charge("tfr5dt")[country_code %in% codes & year >= 1988,
        .(code_pays = country_code, periode = period, isf_moyenne = tfr)]
tfr <- rbind(tfr_est[!periode %in% tfr$periode], tfr, fill = TRUE)
tfr <- merge(pays, tfr, by = "code_pays")
setorder(tfr, code_pays, periode)
ecrire(tfr[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 4) else v)], "fecondite_isf.csv")

pasfr <- charge("percentASFR5dt")[country_code %in% codes & year >= 2018, .(code_pays = country_code, periode = period, age, pct_naissances = pasfr)]
pasfr <- merge(pays, pasfr, by = "code_pays")
setorder(pasfr, code_pays, periode, age)
ecrire(pasfr[, .(code_pays, pays, periode, age, pct_naissances = round(pct_naissances, 4))], "fecondite_calendrier.csv")

# 4. Rapport de masculinité à la naissance ---------------------------------
srb <- charge("sexRatio5dt")
if (!"period" %in% names(srb)) srb[, period := paste0(year - 3, "-", year + 2)]
srb <- srb[country_code %in% codes & year >= 2018, .(code_pays = country_code, periode = period, rmn = srb)]
srb <- merge(pays, srb, by = "code_pays")
ecrire(srb[, .(code_pays, pays, periode, rmn = round(rmn, 4))], "rapport_masculinite_naissance.csv")

# 5. Migration nette par âge et sexe (variante moyenne) ---------------------
mig <- charge("migprojAge5dt")[country_code %in% codes, .(code_pays = country_code, periode = period, age, hommes = migM, femmes = migF)]
mig <- melt(mig, id.vars = c("code_pays", "periode", "age"), variable.name = "sexe", value.name = "migrants_nets_milliers")
mig[, age := factor(age, levels = groupes)]
mig <- merge(pays, mig, by = "code_pays")
setorder(mig, code_pays, periode, sexe, age)
ecrire(mig[, .(code_pays, pays, periode, sexe, age, migrants_nets_milliers = round(migrants_nets_milliers, 4))], "migration_age_sexe.csv")

# 6. Projections officielles : population totale, variantes et intervalles --
pt <- charge("popproj5dt")[country_code %in% codes, .(code_pays = country_code, annee = year,
        pop_mediane = pop, pop_basse = pop_low, pop_haute = pop_high,
        pop_80_inf = pop_80l, pop_80_sup = pop_80u, pop_95_inf = pop_95l, pop_95_sup = pop_95u)]
pt_est <- charge("pop5dt")[country_code %in% codes & year >= 1990 & year <= 2020, .(code_pays = country_code, annee = year, pop_mediane = pop)]
pt <- rbind(pt_est, pt, fill = TRUE)
pt <- merge(pays, pt, by = "code_pays")
setorder(pt, code_pays, annee)
ecrire(pt[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)], "onu_population_totale.csv")

# 7. Espérance de vie à la naissance (variante moyenne et intervalles) ------
e0 <- charge("e0proj5dt")[country_code %in% codes]
e0 <- e0[, .(code_pays = country_code, periode = period,
             e0_hommes = e0M, e0_femmes = e0F, e0_ensemble = e0B,
             e0_ensemble_80_inf = e0B_80l, e0_ensemble_80_sup = e0B_80u,
             e0_ensemble_95_inf = e0B_95l, e0_ensemble_95_sup = e0B_95u)]
e0 <- merge(pays, e0, by = "code_pays")
setorder(e0, code_pays, periode)
ecrire(e0[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)], "onu_esperance_vie.csv")

cat("Fichiers écrits dans", normalizePath(dossier), "\n"); print(list.files(dossier))
