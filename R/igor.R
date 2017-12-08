#' Alternative egor backend
#' 
#' Keeping stuff in linked but separate tables.
#' 
#' @param alters data frame with at least two columns: ego id, alter id, and optional alter attributes
#' @param egos data frame with ego attributers
#' @param aaties data frame with alter-alter ties and their attributes
#' @param id_ego,id_alter,id_from,id_to names of variables in supplied tables.
#'   Defaults are initial columns of respective tables.
#'   
#' @export

igor <- function(alters, egos=NULL, aaties=NULL,
                 id_ego=NULL, id_alter=NULL, id_from=NULL, id_to=NULL
                 ) {
  # Alter df
  stopifnot(inherits(alters, "data.frame"))
  if(is.null(id_ego)) {
    id_ego <- names(alters)[1]
  } else {
    stopifnot(id_ego %in% names(alters))
    stopifnot(any(!is.na(alters[[id_ego]])))
  }
  if(is.null(id_alter)) {
    id_alter <- names(alters)[2]
  } else {
    stopifnot(id_alter %in% names(alters))
    stopifnot(any(!is.na(alters[[id_alter]])))
  }
  
  # Ego df
  if(!is.null(egos)) {
    stopifnot(inherits(egos, "data.frame"))  
    stopifnot(id_ego %in% names(egos))
    stopifnot(any(!is.na(egos[[id_ego]])))
    stopifnot(any(!duplicated(egos[[id_ego]])))
    stopifnot( all(alters[[id_ego]] %in% egos[[id_ego]]) )
  }
  
  # AA ties
  if(!is.null(aaties)) {
    stopifnot(inherits(aaties, "data.frame"))
    if(is.null(id_from)) id_from <- names(aaties[2])
    if(is.null(id_to)) id_to <- names(aaties[3])
    stopifnot(id_ego %in% names(aaties))
    stopifnot(id_from %in% names(aaties))
    stopifnot(id_to %in% names(aaties))
    stopifnot( all(alters[[id_ego]] %in% aaties[[id_ego]]) )
    stopifnot( all(alters[[id_ego]] %in% aaties[[id_from]]) )
    stopifnot( all(alters[[id_ego]] %in% aaties[[id_to]]) )
  }

  structure(
    list(
      egos = egos,
      alters = alters,
      aaties = aaties
    ),
    class="igor"
  )
}





if(FALSE) {
  egodf <- data.frame(
    id_ego = 1:2,
    female= 0:1
  )
  
  alterdf <- data.frame(
    id_ego = c(1,1),
    id_alter = 1:2,
    female = c(FALSE, TRUE)
  )
  
  aaties <- data.frame(
    id_ego = 1,
    from = 1,
    to = 2,
    close = 100
  )
  
  igor(alterdf)
  igor(alterdf, egodf)
  igor(alterdf, egodf, aaties)
}