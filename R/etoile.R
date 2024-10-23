#' etoile_rooms_
#' 
#' @export
etoile_rooms_ <- function(){
  hauteur <-2.45
  larg <- long <- surf <- calc <- NULL
  tibble::tribble(
    ~id, ~desc, ~surf, ~isout, ~type, ~h, ~larg,~long, ~ajust,~comment, ~as_pvc,
    "R1", "chambre1", 12, F, "room", hauteur, 3.27, 3.65,0, "bureau", F,
    "R2", "chambre2", 12, F, "room", hauteur, 3.30, 3.68,0, "coucher", F,
    "R3", "sejour",   23, F, "room", hauteur, 4.60, 5.00,0, "séjour", T,
    "RK", "cuisine",  8, F,  "kitchen", hauteur, 2.75, 2.90, 0,NA, T,
    "RB", "salle_bain", 4, F, "water", hauteur, 1.90, 2.30, +0.5*0.7,"", F,
    "RC", "wc", 4, F, "water", hauteur, 0.93, 1.50, 0,"", T,
    "P0", "entree",   7.6, F, "passage", hauteur, 2.24, 2.89,-0.42*0.60, NA, T,
    "P1", "couloir1", 2.97, F, "passage", hauteur,0.86, 3.17, 0,NA, T,
    "P2", "couloir2", 2.97, F, "passage", hauteur, 0.87, 3.43, 0,NA, T,
    "X1", "balcon",  15.39, T,  "passage", hauteur, 0, 0, 0,"sejour", F,
    "X2", "balcon",   6.37, T, "passage", hauteur,0, 0, 0,"cuisine", F,
  ) |>
    dplyr::mutate(calc = round(larg*long+ajust,2)) |>
    tidyr::unite("super", surf,calc, remove=FALSE, sep = "/") |>
    dplyr::relocate(calc, .after='surf') |>
    data.table::data.table()
}
