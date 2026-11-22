import csv
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.worksheet.datavalidation import DataValidation
from openpyxl.chart import LineChart, BarChart, Reference
from openpyxl.comments import Comment
from openpyxl.utils import get_column_letter as L

OUT = "excel/TP1_projection_UEMOA.xlsx"   # à exécuter depuis projet-projection/, après 04_intrants_excel.R
PAYS = ["Bénin", "Burkina Faso", "Côte d'Ivoire", "Guinée-Bissau", "Mali", "Niger", "Sénégal", "Togo"]
GROUPES = [f"{a}-{a+4}" for a in range(0, 100, 5)] + ["100+"]
ANNEES = [2025, 2030, 2035, 2040, 2045, 2050]
PERIODES = ["2025-2030", "2030-2035", "2035-2040", "2040-2045", "2045-2050"]
AGES_F = ["15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"]

ARIAL = "Arial"
f_norm = Font(name=ARIAL, size=10)
f_bold = Font(name=ARIAL, size=10, bold=True)
f_titre = Font(name=ARIAL, size=14, bold=True, color="7F1D1D")
f_sect = Font(name=ARIAL, size=11, bold=True, color="FFFFFF")
f_input = Font(name=ARIAL, size=10, color="0000FF", bold=True)
f_lien = Font(name=ARIAL, size=10, color="008000")
f_note = Font(name=ARIAL, size=9, italic=True, color="5B6670")
fill_input = PatternFill("solid", fgColor="FFFF00")
fill_sect = PatternFill("solid", fgColor="A51C24")
fill_head = PatternFill("solid", fgColor="F6EAEA")
fin = Side(style="thin", color="D9D9D9")
bord = Border(left=fin, right=fin, top=fin, bottom=fin)

wb = Workbook()
lis = wb.active; lis.title = "Lisez-moi"
par = wb.create_sheet("Paramètres")
pro = wb.create_sheet("Projection")
res = wb.create_sheet("Résultats")
don = wb.create_sheet("Données")
P = "'Paramètres'"
PAYS_REF = f"{P}!$C$3"

def section(ws, row, text, ncol):
    for c in range(1, ncol + 1):
        ws.cell(row=row, column=c).fill = fill_sect
    ws.cell(row=row, column=1, value=text).font = f_sect

# ---------------------------------------------------------------- Données
rows = list(csv.reader(open("excel_intrants.csv", encoding="utf-8")))
don["A1"] = "Clé (pays|élément|sexe|période)"; don["B1"] = "Élément"
for j in range(1, 22):
    don.cell(row=1, column=2 + j, value=f"v{j}")
for c in range(1, 24):
    don.cell(row=1, column=c).font = f_bold; don.cell(row=1, column=c).fill = fill_head
desc = {"P0": "Population au 1er juillet 2025, 0-4 à 100+ (milliers)",
        "S": "Survie : S(0-4→5-9) … S(90-94→95-99), groupe ouvert, naissances→0-4",
        "M": "Migrants nets de la période, 0-4 à 100+ (milliers)",
        "F": "Part des naissances par âge de la mère, 15-19 à 45-49",
        "ISF": "ISF par période, 2025-2030 à 2045-2050",
        "RMN": "Rapport de masculinité à la naissance, par période",
        "ONU": "Population totale ONU 2025-2050 (milliers)"}
for i, r in enumerate(rows[1:], start=2):
    don.cell(row=i, column=1, value=r[0]).font = f_norm
    don.cell(row=i, column=2, value=desc[r[0].split("|")[1]]).font = f_note
    for j, v in enumerate(r[1:], start=3):
        if v != "":
            c = don.cell(row=i, column=j, value=float(v)); c.font = f_norm; c.number_format = "0.000000"
don.column_dimensions["A"].width = 38; don.column_dimensions["B"].width = 50
don.freeze_panes = "C2"
NDON = len(rows)
KEYS = f"Données!$A$2:$A${NDON}"
VALS = f"Données!$C$2:$W${NDON}"
def look(key_expr, j):
    return f"INDEX({VALS},MATCH({key_expr},{KEYS},0),{j})"

# ---------------------------------------------------------------- Paramètres
par["A1"] = "Paramètres de la projection"; par["A1"].font = f_titre
par["A2"] = "Les cellules jaunes se modifient ; tout le reste se recalcule."; par["A2"].font = f_note
lab = [(3, "Pays", "Togo"), (4, "Scénario de fécondité", "moyenne"), (5, "Migration (hypothèse moyenne de l'ONU)", "Oui")]
for r, t, v in lab:
    par.cell(row=r, column=1, value=t).font = f_bold
    c = par.cell(row=r, column=3, value=v); c.font = f_input; c.fill = fill_input; c.border = bord
dv1 = DataValidation(type="list", formula1='"' + ",".join(PAYS) + '"', allow_blank=False)
dv2 = DataValidation(type="list", formula1='"constante,haute,moyenne,basse,personnalisé"', allow_blank=False)
dv3 = DataValidation(type="list", formula1='"Oui,Non"', allow_blank=False)
for dv, ref in [(dv1, "C3"), (dv2, "C4"), (dv3, "C5")]:
    par.add_data_validation(dv); dv.add(ref)
par["D4"] = "constante = ISF de 2020-2025 maintenu ; haute, moyenne, basse = variantes de l'ONU ; personnalisé = ligne 8"; par["D4"].font = f_note
par["D5"] = "Non = solde migratoire nul"; par["D5"].font = f_note

par["A7"] = "Période"; par["A7"].font = f_bold
for k, per in enumerate(PERIODES):
    c = par.cell(row=7, column=3 + k, value=per); c.font = f_bold; c.fill = fill_head; c.alignment = Alignment(horizontal="center")
par["A8"] = "ISF personnalisé (enfants par femme)"; par["A8"].font = f_bold
for k, v in enumerate([3.9, 3.6, 3.3, 3.0, 2.8]):
    c = par.cell(row=8, column=3 + k, value=v); c.font = f_input; c.fill = fill_input; c.number_format = "0.00"; c.border = bord
par["A9"] = "Exemple : trajectoire vers 2,8 enfants en 2045-2050. Utilisée seulement si le scénario est « personnalisé »."; par["A9"].font = f_note
par["A11"] = "ISF utilisé dans la projection"; par["A11"].font = f_bold
for k in range(5):
    col = L(3 + k)
    c = par.cell(row=11, column=3 + k,
                 value=f'=IF($C$4="personnalisé",{col}8,{look(f"$C$3&\"|ISF|\"&$C$4", k+1)})')
    c.font = f_norm; c.number_format = "0.00"; c.border = bord

par["A13"] = "Normes pour les besoins"; par["A13"].font = f_bold
normes = [(14, "Taux de scolarisation visé (6-11 ans)", 0.90, "0%"),
          (15, "Élèves par maître", 45, "0"),
          (16, "Élèves par classe", 50, "0"),
          (17, "Taux d'activité des 15-64 ans", 0.65, "0%"),
          (18, "Part des accouchements assistés", 0.80, "0%"),
          (19, "Consultations prénatales par grossesse", 4, "0")]
for r, t, v, fmt in normes:
    par.cell(row=r, column=1, value=t).font = f_norm
    c = par.cell(row=r, column=3, value=v); c.font = f_input; c.fill = fill_input; c.number_format = fmt; c.border = bord
par["D14"] = "Valeurs d'exemple : remplacez-les par celles de votre plan national et citez la source."; par["D14"].font = f_note
par.column_dimensions["A"].width = 42; par.column_dimensions["B"].width = 2
for k in range(5):
    par.column_dimensions[L(3 + k)].width = 13
par.column_dimensions["C"].width = 16

# ---------------------------------------------------------------- Projection
pro["A1"] = "Projection par la méthode des composantes, 2025-2050 (milliers)"; pro["A1"].font = f_titre
pro["A2"] = "Pays :"; pro["A2"].font = f_bold
pro["C2"] = f"={PAYS_REF}"; pro["C2"].font = f_lien
pro["E2"] = "Scénario :"; pro["E2"].font = f_bold
pro["F2"] = f"={P}!$C$4"; pro["F2"].font = f_lien
for k, a in enumerate(ANNEES):
    c = pro.cell(row=3, column=3 + k, value=str(a)); c.font = f_bold; c.fill = fill_head; c.alignment = Alignment(horizontal="center")
for k, per in enumerate(PERIODES):
    c = pro.cell(row=4, column=4 + k, value=per); c.font = f_note; c.alignment = Alignment(horizontal="center")
pro["B3"] = "Année"; pro["B4"] = "Période"
for x in ("B3", "B4"): pro[x].font = f_bold

R_F, R_H = 7, 30            # première ligne (0-4) des blocs femmes / hommes
R_B, R_FX, R_RMN = 53, 56, 64
R_SF, R_SH = 67, 90         # S1 des blocs de survie
R_MF, R_MH = 113, 136       # 0-4 des blocs de migration

section(pro, 6, "Étape 1 et 2 — Population féminine par âge", 8)
section(pro, 29, "Population masculine par âge", 8)
section(pro, 52, "Étape 3 — Naissances de la période (femmes moyennes de 15-49 ans × taux)", 8)
section(pro, 55, "Taux de fécondité annuels par âge f(x) = ISF × part des naissances / 5", 8)
section(pro, 66, "Rapports de survie, femmes (tables de mortalité de l'ONU, variante moyenne)", 8)
section(pro, 89, "Rapports de survie, hommes", 8)
section(pro, 112, "Migrants nets de la période, femmes (0 si migration = Non)", 8)
section(pro, 135, "Migrants nets de la période, hommes", 8)

def cell(ws, r, c, v, fmt="#,##0.0", font=f_norm):
    x = ws.cell(row=r, column=c, value=v); x.number_format = fmt; x.font = font; x.border = bord
    return x

for sexe, R0, RS, RM in [("femmes", R_F, R_SF, R_MF), ("hommes", R_H, R_SH, R_MH)]:
    for i in range(1, 22):
        r = R0 + i - 1
        pro.cell(row=r, column=2, value=GROUPES[i - 1]).font = f_bold
        cell(pro, r, 3, "=" + look(f'{PAYS_REF}&"|P0|{sexe}"', i), font=f_lien)
        for k in range(5):
            X, Y = L(4 + k), L(3 + k)
            if i == 1:
                part = f"1/(1+{X}${R_RMN})" if sexe == "femmes" else f"{X}${R_RMN}/(1+{X}${R_RMN})"
                f = f"=MAX(0,{X}${R_B}*{part}*{X}{RS+20}+{X}{RM})"
            elif i < 21:
                f = f"=MAX(0,({Y}{r-1}+{X}{RM+i-1}/2)*{X}{RS+i-2}+{X}{RM+i-1}/2)"
            else:
                f = f"=MAX(0,({Y}{r-1}+{Y}{r}+{X}{RM+20}/2)*{X}{RS+19}+{X}{RM+20}/2)"
            cell(pro, r, 4 + k, f)
    # survies
    for j in range(1, 22):
        r = RS + j - 1
        if j <= 19:
            t = f"{GROUPES[j-1]} → {GROUPES[j]}"
        elif j == 20:
            t = "95-99 et 100+ → 100+"
        else:
            t = "naissances → 0-4"
        pro.cell(row=r, column=2, value=t).font = f_norm
        for k in range(5):
            X = L(4 + k)
            cell(pro, r, 4 + k, "=" + look(f'{PAYS_REF}&"|S|{sexe}|"&{X}$4', j), fmt="0.0000", font=f_lien)
    # migration
    for j in range(1, 22):
        r = RM + j - 1
        pro.cell(row=r, column=2, value=GROUPES[j - 1]).font = f_norm
        for k in range(5):
            X = L(4 + k)
            cell(pro, r, 4 + k, f'=IF({P}!$C$5="Oui",' + look(f'{PAYS_REF}&"|M|{sexe}|"&{X}$4', j) + ",0)", fmt="#,##0.000", font=f_lien)

pro.cell(row=R_B, column=2, value="Naissances").font = f_bold
for k in range(5):
    X, Y = L(4 + k), L(3 + k)
    cell(pro, R_B, 4 + k, f"=5*SUMPRODUCT({X}{R_FX}:{X}{R_FX+6},({Y}{R_F+3}:{Y}{R_F+9}+{X}{R_F+3}:{X}{R_F+9})/2)")
for j, a in enumerate(AGES_F):
    r = R_FX + j
    pro.cell(row=r, column=2, value=a).font = f_norm
    for k in range(5):
        X = L(4 + k)
        cell(pro, r, 4 + k, f"={P}!{L(3+k)}$11*" + look(f'{PAYS_REF}&"|F|"&{X}$4', j + 1) + "/5", fmt="0.0000")
pro.cell(row=R_RMN, column=2, value="Rapport de masculinité").font = f_bold
for k in range(5):
    cell(pro, R_RMN, 4 + k, "=" + look(f'{PAYS_REF}&"|RMN"', k + 1), fmt="0.000", font=f_lien)
pro.column_dimensions["A"].width = 3; pro.column_dimensions["B"].width = 24
for c in range(3, 9):
    pro.column_dimensions[L(c)].width = 12
pro.freeze_panes = "C5"
pro["A158"] = ("Formules : âge 5-9 et plus, P(x+5, t+5) = [P(x, t) + M/2] × S(x) + M/2 ; groupe ouvert, "
               "[P(95-99) + P(100+) + M/2] × S + M/2 ; 0-4 ans, naissances × part du sexe × survie + migrants. "
               "Les rapports de survie sont calculés dans R (fonction table_mortalite) à partir des taux de mortalité de l'ONU.")
pro["A158"].font = f_note

# ---------------------------------------------------------------- Résultats
res["A1"] = "Résultats : indicateurs et besoins (milliers, sauf mention)"; res["A1"].font = f_titre
res["A2"] = "Pays :"; res["A2"].font = f_bold; res["B2"] = f"={PAYS_REF}"; res["B2"].font = f_lien
res["C2"] = "Scénario :"; res["C2"].font = f_bold; res["D2"] = f"={P}!$C$4"; res["D2"].font = f_lien
res["E2"] = "Migration :"; res["E2"].font = f_bold; res["F2"] = f"={P}!$C$5"; res["F2"].font = f_lien
heads = ["Année", "Population totale", "0-14 ans", "15-64 ans", "65 ans et plus", "Moins de 15 ans (%)",
         "Rapport de dépendance (pour 100)", "Naissances par an", "ONU, variante moyenne", "Écart à l'ONU (%)",
         "ONU, borne 95 % inf.", "ONU, borne 95 % sup.", "Âge primaire (6-11 ans)", "Élèves du primaire",
         "Enseignants nécessaires", "Salles de classe", "Actifs 15-64 ans", "Nouveaux actifs par an",
         "Accouchements assistés par an", "Consultations prénatales par an"]
HR = 4
for j, h in enumerate(heads, start=1):
    c = res.cell(row=HR, column=j, value=h); c.font = f_bold; c.fill = fill_head
    c.alignment = Alignment(wrap_text=True, vertical="center", horizontal="center"); c.border = bord
res.row_dimensions[HR].height = 54
def S(col, r1, r2):
    return f"SUM(Projection!{col}{r1}:{col}{r2})"
for k, a in enumerate(ANNEES):
    r = HR + 1 + k; X = L(3 + k)
    cell(res, r, 1, str(a), fmt="@", font=f_bold)
    cell(res, r, 2, f"={S(X,R_F,R_F+20)}+{S(X,R_H,R_H+20)}")
    cell(res, r, 3, f"={S(X,R_F,R_F+2)}+{S(X,R_H,R_H+2)}")
    cell(res, r, 4, f"={S(X,R_F+3,R_F+12)}+{S(X,R_H+3,R_H+12)}")
    cell(res, r, 5, f"={S(X,R_F+13,R_F+20)}+{S(X,R_H+13,R_H+20)}")
    cell(res, r, 6, f"=C{r}/B{r}", fmt="0.0%")
    cell(res, r, 7, f"=100*(C{r}+E{r})/D{r}", fmt="0.0")
    cell(res, r, 8, "" if k == 0 else f"=Projection!{X}{R_B}/5")
    cell(res, r, 9, "=" + look(f'{PAYS_REF}&"|ONU|moyenne"', k + 1), font=f_lien)
    cell(res, r, 10, f'=IF(AND({P}!$C$5="Oui",{P}!$C$4="moyenne"),B{r}/I{r}-1,"")', fmt="0.00%")
    cell(res, r, 11, "=" + look(f'{PAYS_REF}&"|ONU|95inf"', k + 1), font=f_lien)
    cell(res, r, 12, "=" + look(f'{PAYS_REF}&"|ONU|95sup"', k + 1), font=f_lien)
    cell(res, r, 13, f"=0.8*(Projection!{X}{R_F+1}+Projection!{X}{R_H+1})+0.4*(Projection!{X}{R_F+2}+Projection!{X}{R_H+2})")
    cell(res, r, 14, f"=M{r}*{P}!$C$14")
    cell(res, r, 15, f"=N{r}/{P}!$C$15", fmt="#,##0.00")
    cell(res, r, 16, f"=N{r}/{P}!$C$16", fmt="#,##0.00")
    cell(res, r, 17, f"=D{r}*{P}!$C$17")
    cell(res, r, 18, "" if k == 0 else f"=(Q{r}-Q{r-1})/5")
    cell(res, r, 19, "" if k == 0 else f"=H{r}*{P}!$C$18")
    cell(res, r, 20, "" if k == 0 else f"=H{r}*{P}!$C$19")
NR = HR + len(ANNEES)
notes = ["Effectifs en milliers : 40,1 enseignants = 40 100 enseignants.",
         "Âge primaire 6-11 ans approché par 4/5 du groupe 5-9 et 2/5 du groupe 10-14.",
         "Naissances par an : moyenne de la période quinquennale qui se termine l'année indiquée.",
         "L'écart à l'ONU n'a de sens qu'avec le scénario « moyenne » et la migration « Oui » : il doit rester sous 0,5 %.",
         "Source des données : Nations Unies, World Population Prospects 2024."]
for i, t in enumerate(notes):
    res.cell(row=NR + 2 + i, column=1, value=t).font = f_note
res.column_dimensions["A"].width = 9
for c in range(2, 21):
    res.column_dimensions[L(c)].width = 13
res.freeze_panes = "B5"

ch = LineChart(); ch.title = "Population totale : projection et ONU (milliers)"
ch.add_data(Reference(res, min_col=2, min_row=HR, max_row=NR), titles_from_data=True)
ch.add_data(Reference(res, min_col=9, min_row=HR, max_row=NR), titles_from_data=True)
ch.add_data(Reference(res, min_col=11, min_row=HR, max_row=NR), titles_from_data=True)
ch.add_data(Reference(res, min_col=12, min_row=HR, max_row=NR), titles_from_data=True)
ch.set_categories(Reference(res, min_col=1, min_row=HR + 1, max_row=NR))
ch.height, ch.width = 8, 17
ch.roundedCorners = False; ch.legend.position = "b"; ch.legend.overlay = False
ch.x_axis.delete = False; ch.y_axis.delete = False
ch.series[0].graphicalProperties.line.solidFill = "A51C24"; ch.series[0].graphicalProperties.line.width = 32000
for s, col in zip(ch.series[1:], ["5B6670", "B0B0B0", "B0B0B0"]):
    s.graphicalProperties.line.solidFill = col; s.graphicalProperties.line.dashStyle = "dash"
res.add_chart(ch, f"A{NR + 9}")
bc = BarChart(); bc.title = "Élèves du primaire (milliers)"
bc.add_data(Reference(res, min_col=14, min_row=HR, max_row=NR), titles_from_data=True)
bc.set_categories(Reference(res, min_col=1, min_row=HR + 1, max_row=NR))
bc.height, bc.width = 8, 13; bc.roundedCorners = False; bc.legend = None
bc.x_axis.delete = False; bc.y_axis.delete = False
bc.series[0].graphicalProperties.solidFill = "A51C24"
res.add_chart(bc, f"J{NR + 9}")

# ---------------------------------------------------------------- Lisez-moi
txt = [
    ("TP 1 — Projeter la population de son pays à 2050 (version Excel)", f_titre),
    ("Formation régionale UEMOA · Jour 4 · Ouagadougou, 26 novembre 2026 · Vissého Adjiwanou, UQAM · SSAPOPLAB", f_note),
    ("", f_norm),
    ("Mode d'emploi", f_bold),
    ("1. Feuille « Paramètres » : choisissez votre pays dans la cellule jaune C3 (liste déroulante).", f_norm),
    ("2. Choisissez le scénario de fécondité (C4) et la migration (C5). Pour un scénario national, choisissez « personnalisé » et saisissez l'ISF de chaque période à la ligne 8.", f_norm),
    ("3. Remplacez les normes des lignes 14 à 19 par celles de votre plan national, et notez leur source.", f_norm),
    ("4. Lisez les résultats dans la feuille « Résultats » : indicateurs, besoins et graphiques.", f_norm),
    ("5. La feuille « Projection » montre chaque étape de calcul, période par période. Cliquez sur une cellule pour voir sa formule.", f_norm),
    ("", f_norm),
    ("Contrôle", f_bold),
    ("Avec le scénario « moyenne » et la migration « Oui », la colonne « Écart à l'ONU » doit rester sous 0,5 %. Sinon, ne pas interpréter : appeler un formateur.", f_norm),
    ("", f_norm),
    ("Code couleur", f_bold),
    ("Cellule jaune, texte bleu : à modifier. Texte vert : valeur tirée de la feuille « Données ». Texte noir : formule. Ne modifiez pas les feuilles « Projection » et « Données ».", f_norm),
    ("", f_norm),
    ("Méthode", f_bold),
    ("Méthode des composantes, par sexe et groupe d'âge quinquennal (0-4 à 100+), par pas de 5 ans du 1er juillet 2025 au 1er juillet 2050. Mortalité, migration, calendrier de la fécondité et rapport de masculinité : variante moyenne de l'ONU. Seul le niveau de la fécondité change d'un scénario à l'autre.", f_norm),
    ("Le classeur reproduit le script R tp1_projection.R : les deux donnent les mêmes résultats, à l'arrondi près.", f_norm),
    ("", f_norm),
    ("Source", f_bold),
    ("Nations Unies, Division de la population (2024), World Population Prospects 2024, jeux quinquennaux du paquet R wpp2024. Effectifs en milliers.", f_norm),
]
for i, (t, f) in enumerate(txt, start=1):
    c = lis.cell(row=i, column=1, value=t); c.font = f; c.alignment = Alignment(wrap_text=True, vertical="top")
lis.column_dimensions["A"].width = 120
for i in (6, 12, 15, 18):
    lis.row_dimensions[i].height = 28

for ws in (par, pro, res, don, lis):
    ws.sheet_view.showGridLines = ws is don
wb.active = 1
wb.save(OUT)
print("ok", OUT)
