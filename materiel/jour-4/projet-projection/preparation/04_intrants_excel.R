# Intrants du classeur Excel : une ligne par clé « pays|élément|… », 21 valeurs au plus
source("R/fonctions_projection.R", encoding = "UTF-8")
d <- lire_donnees("donnees")
lignes <- list()
ajout <- function(cle, v) lignes[[length(lignes) + 1]] <<- c(cle = cle, setNames(as.list(c(v, rep(NA, 21 - length(v)))), paste0("v", 1:21)))
for (p in PAYS_UEMOA) {
  i <- intrants_pays(d, p)
  for (s in c("femmes", "hommes")) ajout(paste(p, "P0", s, sep = "|"), i$P0[[s]])
  for (k in seq_along(PERIODES)) {
    per <- PERIODES[k]
    for (s in c("femmes", "hommes")) {
      sv <- i$survie[[per]][[s]]
      ajout(paste(p, "S", s, per, sep = "|"), c(sv$S, sv$S_ouvert, sv$s0))
      ajout(paste(p, "M", s, per, sep = "|"), i$migration[[per]][[s]])
    }
    ajout(paste(p, "F", per, sep = "|"), i$calendrier[[per]])
  }
  for (sc in c("constante", "haute", "moyenne", "basse")) ajout(paste(p, "ISF", sc, sep = "|"), i$isf[[sc]])
  ajout(paste(p, "RMN", sep = "|"), i$rmn)
  onu <- d$pop %>% filter(pays == p, annee %in% seq(2025, 2050, 5)) %>% group_by(annee) %>% summarise(t = sum(pop_milliers)) %>% pull(t)
  ajout(paste(p, "ONU", "moyenne", sep = "|"), onu)
  o <- d$onu_pop %>% filter(pays == p, annee %in% seq(2025, 2050, 5)) %>% arrange(annee)
  ajout(paste(p, "ONU", "95inf", sep = "|"), o$pop_95_inf); ajout(paste(p, "ONU", "95sup", sep = "|"), o$pop_95_sup)
}
out <- bind_rows(lapply(lignes, as_tibble))
write_csv(out, "excel_intrants.csv", na = "")
# Résultats de référence pour vérifier le classeur
ref <- map_dfr(c("Togo", "Mali"), function(p) {
  i <- intrants_pays(d, p)
  bind_rows(projeter(i, "moyenne"), projeter(i, "constante"), projeter(i, "basse", migration = FALSE)) %>% indicateurs()
})
write_csv(ref, "excel_reference.csv")
cat(nrow(out), "lignes\n")
