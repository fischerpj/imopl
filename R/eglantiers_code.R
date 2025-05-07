#' rooms_eglantiers
#' 
#' @export
rooms_eglantiers_ <- function(){
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

#' params_
#' 
#' @export
params_ <- function(){
  params <- list(
    carrez = 112, 
  hauteur= 2.8,
  longueur= 13.8,
  L1= 10.3,
  largeur= 8.1,
  surface_murs = 108,
  angle_toiture = pi/6,
  renovm2 = 1.5,
  offre_achat_tc = 400,
  frais_mutation = 29.298,
  agence_rate = 0.0484,
  adresse= "églantiers #11",
  agence = "Human-Immo",
  notaire_name = "Eugénie Delpuech",
  annual_consumption = 15000,
  avg_annual_radiation = 1600,
  efficiency = 0.18
  )
  
  within(params,{
    toit_S <- round(largeur*L1/cos(angle_toiture),2)
    puissance_chauffe <- carrez*100
    radiateurs <- round(puissance_chauffe/1000,0)
    panel_m2 = annual_consumption/(avg_annual_radiation*efficiency)
  })
}

#' travaux_eglantiers_
#' 
#' @export
travaux_eglantiers_ <- function(params= params_()){
  quant <- prix_unit <- NULL
  
  with(params,
  tibble::tribble(
    ~id,  ~projet, ~element, ~categorie, ~quant, ~prix_unit, ~version, ~comment,
    "G1", "EGL", "toiture", "gros-oeuvre", toit_S, 0.165, 0.1, NA,
    "G2", "EGL", "velux", "gros-oeuvre", 0, 1.2, 0.1, "114*118",
    "G3", "EGL","cloisons", "gros-oeuvre", 0, 0.05, 0.3, "tbc, à revoir",
    "S1", "EGL", "chappe", "sols", L1*largeur, 0.105, 0.1, NA,
    "S2", "EGL", "isolant", "sols", L1*largeur, 0.05, 0.1, NA,
    "S3", "EGL", "chappe-légère", "sols", 0*L1*largeur, 0.100, 0.1, NA,
    "S4", "EGL","rev\U00CAtement_sol", "sols", L1*largeur, 0.04+0.055+0.055, 0.2, "carreau+d\U00E9pose/pose",
    "E1", "EGL","tableau", "electricite", 0, 1, 0.1, NA,
    "E2", "EGL","prises", "electricite", carrez, 0.15, 0.2, "matos+pose ",
    "E3", "EGL","photovoltaique", "electricite", 18, 1, 0.2, "Ensol.fr",
    "E4", "EGL","luminaires", "electricite", 0, 1,  0.1, NA,
    "F1", "EGL","ouv.porte", "gros-oeuvre", 0, 1.0, 0.1, "215*90",
    "F2", "EGL","ouv.baie", "gros-oeuvre", 1, 6.0, 0.1, "250*250",
    "F3", "EGL","fen\U00CAtres", "ouverture", 7, .8+.79, 0.1, NA,
    "F4", "EGL","porte entrée", "ouverture", 1, 1.5, 0.1, NA,
    "I1", "EGL","isolation_toit", "isolation", toit_S, 0.05, 0.3, NA,
    "I2", "EGL","isolation_ext", "isolation", longueur*largeur*hauteur, 0.05, 0.3, NA,
    "W1", "EGL","salle_bain", "water", 1, 6,  0.1, NA,
    "K1", "EGL","cuisine", "cuisine", 1, 13,  0.1, NA,
    "M1", "EGL","rev\U00CAtement_murs", "murs&plafond",longueur*largeur*hauteur , 0.03,  0.1, "refaire calcul extensif",
    "M2", "EGL","rev\U00CAtement_plafond", "murs&plafond", carrez, 0.045,  0.1, NA,  "T1", "MTS", "chauffage", "thermodynamique", 1, 15, 0.1, NA,
    "C1", "EGL","radiateur", "chauffage", 11, 0.3, 0.1, NA,
    "C2", "EGL","clim", "chauffage", 0, 1, 0.1, NA,
    "C3", "EGL","poêle bois", "chauffage", 1, 4, 0.1, NA,
    "C4", "EGL","ôter cheminée", "chauffage", 1, 1, 0.1, "tbc",
    "C5", "EGL","ôter cuve fioul", "chauffage", 1, 2, 0.1, "tbc",
    "C5b", "EGL","ôter chaudière fioul", "chauffage", 1, 1, 0.1, "tbc",
    "X2", "EGL","outillage", "divers",0, 1,  2, NA,
    "T2", "MTS","clim", "thermodynamique", 0, 1, 0.1, NA,
    "T3", "MTS","ecs", "thermodynamique", 0, 1, 0.1, NA,
    "F1", "MTS","portes", "fermetures", 1, 1.5, 0.1, NA,
    "F2", "MTS","fen\U00CAtres", "fermetures", 2, .8+.79, 0.1, NA,
    "F3", "MTS","porte-fen\U00CAtres", "fermetures", 4, 1.2+.81, 0.1, NA,
    "F4", "MTS","baies", "fermetures", 3, 3+2, 0.1, NA,
    "F5", "MTS","loggia", "fermetures", 0, 1,  0.1, NA,
    "F6", "MTS","store", "fermetures", 1, 5, 0.1, NA,
    "F7", "MTS","isolation_int", "fermetures", 65, 0.1, 0.3, NA,
    "F7", "MTS","opaque", "fermetures", 0, 1, 0.1, "sdb",
    "E1", "MTS","tableau", "electricite", 0, 1, 0.1, NA,
    "E2", "MTS","prises", "electricite", 113, 0.19, 0.2, "matos+pose",
    "E3", "MTS","luminaires", "electricite", 0, 1,  0.1, NA,
    "K1", "MTS","meubles", "cuisine", 1, 12, 0.1, NA,
    "K2", "MTS","electro", "cuisine", 0, 1,  0.1, NA,
    "W1", "MTS","salle_bain", "eau", 1, 5,  0.1, NA,
    "W2", "MTS","salle_eau", "eau", 1, 5,  0.1, NA,
    "W3", "MTS","wc_guest", "eau", 1, 1, 0.1, NA,
    "W4", "MTS","lave-linge", "eau", 0, 1,  0.1, NA,
    "S1", "MTS","rev\U00CAtement_sol", "surface", 113, 0.04+0.055+0.055, 0.2, "carreau+d\U00E9pose/pose",
    "S2", "MTS","rev\U00CAtement_murs", "surface", 200, 0.03,  0.1, NA,
    "S3", "MTS","rev\U00CAtement_plafond", "surface", 110, 0.045,  0.1, NA,
    "S4", "MTS","rev\U00CAtement_porte", "surface", 0, 1,  0.1, NA,
    "S5", "MTS","stockage", "surface",0, 1,  0.1, NA,
    "S6", "MTS","isolation_int\U00E9rieure", "surface",0, 1,  0.1, NA,
    "S7", "MTS","rev\U00CAtement_terrasse", "surface",0, 1,  0.1, NA,
    "M1", "MTS","literie", "equipement",0, 1,  0.1, NA,
    "M2", "MTS","salon", "equipement",0, 1,  0.1, NA,
    "X1", "MTS","recyclage", "divers",0, 1,  0.1, NA,
    "X2", "MTS","outillage", "divers",0, 1,  0.3, NA,
  )) |> 
    dplyr::mutate(value = quant*prix_unit)
}