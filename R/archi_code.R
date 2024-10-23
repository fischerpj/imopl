## grep --color='auto' -P -n "[\x80-\xFF]" sample.txt

#' travaux_
#' 
#' @export
travaux_ <- function(){
  quant <- prix_unit <- NULL
  tibble::tribble(
    ~id,  ~element, ~categorie, ~quant, ~prix_unit, ~version, ~comment,
    "T1", "chauffage", "thermodynamique", 1, 15, 0.1, NA,
    "T2", "clim", "thermodynamique", 0, 1, 0.1, NA,
    "T3", "ecs", "thermodynamique", 0, 1, 0.1, NA,
    "F1", "portes", "fermetures", 1, 1.5, 0.1, NA,
    "F2", "fen\U00CAtres", "fermetures", 2, .8+.79, 0.1, NA,
    "F3", "porte-fen\U00CAtres", "fermetures", 4, 1.2+.81, 0.1, NA,
    "F4", "baies", "fermetures", 3, 3+2, 0.1, NA,
    "F5", "loggia", "fermetures", 0, 1,  0.1, NA,
    "F6", "store", "fermetures", 1, 5, 0.1, NA,
    "F7", "isolation_int", "fermetures", 65, 0.1, 0.3, NA,
    "F7", "opaque", "fermetures", 0, 1, 0.1, "sdb",
    "E1", "tableau", "electricite", 0, 1, 0.1, NA,
    "E2", "prises", "electricite", 113, 0.19, 0.2, "matos+pose",
    "E3", "luminaires", "electricite", 0, 1,  0.1, NA,
    "K1", "meubles", "cuisine", 1, 12, 0.1, NA,
    "K2", "electro", "cuisine", 0, 1,  0.1, NA,
    "W1", "salle_bain", "eau", 1, 5,  0.1, NA,
    "W2", "salle_eau", "eau", 1, 5,  0.1, NA,
    "W3", "wc_guest", "eau", 1, 1, 0.1, NA,
    "W4", "lave-linge", "eau", 0, 1,  0.1, NA,
    "S1", "rev\U00CAtement_sol", "surface", 113, 0.04+0.055+0.055, 0.2, "carreau+d\U00E9pose/pose",
    "S2", "rev\U00CAtement_murs", "surface", 200, 0.03,  0.1, NA,
    "S3", "rev\U00CAtement_plafond", "surface", 110, 0.045,  0.1, NA,
    "S4", "rev\U00CAtement_porte", "surface", 0, 1,  0.1, NA,
    "S5", "stockage", "surface",0, 1,  0.1, NA,
    "S6", "isolation_int\U00E9rieure", "surface",0, 1,  0.1, NA,
    "S7", "rev\U00CAtement_terrasse", "surface",0, 1,  0.1, NA,
    "M1", "literie", "equipement",0, 1,  0.1, NA,
    "M2", "salon", "equipement",0, 1,  0.1, NA,
    "X1", "recyclage", "divers",0, 1,  0.1, NA,
    "X2", "outillage", "divers",0, 1,  0.3, NA,
  ) |> 
    dplyr::mutate(value = quant*prix_unit)
}

#' rooms_
#' 
#' @export
rooms_ <- function(){
  hauteur <-2.45
  larg <- long <- surf <- calc <- NULL
  tibble::tribble(
    ~id, ~desc, ~surf, ~isout, ~type, ~h, ~larg,~long, ~comment,
    "R1", "chambre1", 12.08, F, "room", hauteur, 3.51, 3.66, NA,
    "R2", "chambre2", 11.17, F, "room", hauteur, 3.09, 3.30, "+placard2 + 0.84*0.24",
    "R3", "chambre3", 12.95, F, "room", hauteur, 3.30, 3.39, NA,
    "R4", "sejour",   37.82, F, "room", hauteur, 4.00, 6.485, NA,
    "R5", "studio",   0, F, "room", hauteur, NA, NA, NA,
    "RK", "cuisine",  8.67, F,  "kitchen", hauteur, NA, NA, NA,
    "RB1", "salle_bain", 4.29, F, "water", hauteur, NA, NA, NA,
    "RB2", "salle_eau",  5.85, F, "water", hauteur, NA, NA, NA,
    "RB3", "wc_invite",  1.06, F, "water", hauteur, NA, NA, NA,
    "P0", "entree",   7.6, F, "passage", hauteur, 2.57, 2.70, NA,
    "P1", "couloir1", 2.97, F, "passage", hauteur, NA, NA, NA,
    "P2", "couloir2", 6.39, F, "passage", hauteur, NA, NA, NA,
    "B1", "placard1", 0.96, F, "storage", hauteur, 1.06, 0.9, NA,
    "B2", "placard2", 0.72, F,  "storage", hauteur, 1.485, 0.63, NA,
    "B3", "placard3", 0.72, F, "storage", hauteur, 1.485, 0.63, NA,
    "X1", "balcon",  15.39, T,  "passage", hauteur, NA, NA, NA,
    "X2", "loggia",   6.37, T, "passage", hauteur, NA, NA, NA,
    "X3", "garage",  15.85, T, "storage", hauteur, NA, NA, NA,
    "X4", "cave",     9, T, "storage", hauteur, NA, NA, NA,
  ) |>
    dplyr::mutate(calc = round(larg*long,2)) |>
    tidyr::unite("super", surf,calc, remove=FALSE, sep = "/") |>
    dplyr::relocate(calc, .after='surf') |>
        data.table::data.table()
}

geom_ <- function(){
  tibble::tribble(
  ~type, ~local, ~surface, ~larg, ~long, ~materiau, ~quant, ~prix_unit,
  "electricite","tous",  0, 0.88, 2.1, "prise", 113, 0.15,
  "chauffage","tous",  0, 0.88, 2.1, "prise", 1, 15,
  "ouverture","entree",  NA, 0.88, 2.1, "bois", 1, 1.2,
  "ouverture","cuisine",   0, 1.5, 2.32, "pvc", 1, 1.2,
  "ouverture","cuisine",   0, 1.5, 2.32, "volet", 1, 0.685,
  "ouverture","chambre3",  0, 1.5, 2.32, "pvc", 1, 1.2,
  "ouverture","chambre3",  0, 1.5, 2.32, "volet", 1, 0.685,
  "ouverture","chambre2",  0, 1.5, 2.32, "pvc", 1, 0.8,
  "ouverture","chambre2",  0, 1.5, 2.32, "volet", 1, 0.685,
  "ouverture","chambre1",  0, 1.5, 2.32, "pvc", 1, 0.8,
  "ouverture","chambre1",  0, 1.5, 2.32, "volet", 1, 0.685,
  "ouverture","chambre1",  0, 1.5, 2.32, "pvc", 1, 1,
  "ouverture","chambre1",  0, 1.5, 2.32, "volet", 1, 0.685,
  "ouverture","sejour",  0, 2.55, 2.32, "volet", 2, 2*0.685,
  "ouverture","sejour",  0, 2.55, 2.32, "pvc", 2, 2.5,
  "ouverture","sejour",  0, 2.5, 5, "store", 1, 1,
  "ouverture","studio",   0, 2.55, 2.32, "pvc", 1, 2.5,
  "ouverture","studio",   0, 2.55, 2.32, "volet", 1, 2*0.685,
  "couloir","entree",    7.6, 2.57, 2.7, "sol", 1, 0.1,
  "cuisine","cuisine",  0, NA, NA, "integree", 1, 12,
  "cuisine","cuisine",  8.67, NA, NA, "sol", 1, 0.1,
  "pi\U00E8ce","sejour",   37.82, 4, 6.485, "sol", 1, 0.1,
  # "pi\U00E8ce","studio",   37.82, 4, 2.9,
  "couloir","couloir",  6.39, NA, NA, "sol", 1, 0.1,
  "chambre","placard1", 0.96, NA, NA, "sol", 1, 0.1,
  "chambre","placard2", 0.72, NA, NA, "sol", 1, 0.1,
  "chambre","placard3", 0.72, NA, NA, "sol", 1, 0.1,
  "chambre","chambre1", 12.08, 3.51, 3.66, "sol", 1, 0.1,
  "chambre","chambre2", 11.17, 3.09, 3.30, "sol", 1, 0.1,
  "chambre","chambre3", 12.95, 3.3, 3.39, "sol", 1, 0.1,
  # "chambre","chambre1", 12.08, 3.51, 3.66,
  # "chambre","chambre2", 2, 0.55, 1.20,
  # "chambre","chambre3", 12.95, 3.3, 3.39,
  "sanitaire","salle_eau", 5.85, NA, NA, "sdb", 1, 6,
  "sanitaire","salle_bain", 4.29, NA, NA, "sdb", 1, 6,
  "sanitaire","wc",  1.06, NA, NA, "sol", 1, 0.1,
  "sanitaire","wc",  0, NA, NA, "wc", 1, 1,
  "couloir","degagement", 2.97, NA, NA, "sol", 1, 0.1,
) |>
    data.table::data.table()
}