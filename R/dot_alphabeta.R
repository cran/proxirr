#' Calculate Alpha Irreplaceability
#'
#' This function calculates Alpha Irreplaceability for a single feature.
#'
#' If na.allow is \code{TRUE} and any of the required values is NA, returns NA.
#'
#' If triage is set to \code{TRUE}, alpha irreplaceability is set to 0 if the target
#' cannot be achieved (i.e., if the target is greater than the global value),
#' and calculated normally otherwise.
#'
#' @param local numeric - The feature's local representation at the site.
#' @param global numeric - The feature's globally available representation.
#' @param target numeric - The feature's target.
#' @param triage logical - Should features with unachievable targets be ignored?
#' Defaults to FALSE. If FALSE, these species will be always assigned an Alpha
#' irreplaceability of 1 wherever they occur. If TRUE, these species will always
#' be assigned an Alpha irreplaceabiltiy of 0.
#' @param na.allow logical - Return NA if NA values are found in the inputs?
#' @return A number between 0 and 1.
#' @examples
#' .alpha(local=0,    global=100,  target=50)
#' .alpha(local=15,   global=100,  target=50)
#' .alpha(local=35,   global=100,  target=50)
#' .alpha(local=49.5, global=100,  target=50)
#' .alpha(local=55,   global=100,  target=50)
#' .alpha(local=100,  global=100,  target=0)
#' .alpha(local=5,    global=100,  target=110)
#' .alpha(local=5,    global=100,  target=110, triage=TRUE)
#' .alpha(local=32,   global=100,  target=NA, na.allow = TRUE)
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @references \doi{10.1111/cobi.13806}
#' @export
#' @keywords internal
.alpha = function(local, global, target, triage = FALSE, na.allow = FALSE) {
  # NA run
  if(na.allow) if(is.na(local) | is.na(global) | is.na(target) | is.na(triage)) return(NA)
  # check local
  if(!is.numeric(local) | is.nan(local) | is.na(local)) stop('local value must be a non-negative number')
  if(local < 0) stop('local value must be a non-negative number')
  # check global
  if(!is.numeric(global) | is.nan(global) | is.na(global)) stop('global value must be a positive number')
  if(global <= 0) stop('global value must be a positive number')
  # check target
  if(!is.numeric(target) | is.nan(target) | is.na(target)) stop('target value must be a non-negative number')
  if(target < 0) stop('target value must be a non-negative number')
  # check local-global ratio
  if(local > global) stop('local value greater than global value')
  # check triage
  if(!is.logical(triage)) stop('triage must be logical')
  # main run
  if(target == 0) {
    i = 0
  } else if(triage & target > global) {
    i = 0
  } else if(target >= global) {
    if(local > 0) {
      i = 1
    } else {
      i = 0
    }
  } else {
    i = min(1,local/(global-target))
  }
  return(i)
}


#' Calculate Beta Irreplaceability
#'
#' This function calculates Beta Irreplaceability from a vector of Alpha
#' Irreplaceability values. Alpha Irreplaceability values can be calculated using
#' \code{\link{.alpha}()}.
#'
#' @param alphas vector - A vector of numbers between 0-1.
#' @param na.rm logical - Should missing values (NA) be removed?
#' @return A number between 0 and 1.
#' @examples
#' .beta(alphas=c(0.1,0.32,0.5))
#' .beta(alphas=c(0.1,0.32,0.9))
#' .beta(alphas=c(0.1,0.32,1))
#' .beta(alphas=c(0.1,0.32,NA), na.rm=TRUE)
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @references \doi{10.1111/cobi.13806}
#' @export
#' @keywords internal
.beta = function(alphas, na.rm = FALSE) {
  if(!is.vector(alphas)) stop('alphas must be a vector')
  if(!is.logical(na.rm)) stop('na.rm must be logical')
  for(a in alphas) {
    if(is.nan(a)) {
      stop('at least one alpha element is not a number')
    } else {
      if(is.na(a)) {
        if(!na.rm) stop('at least one NA element. Try with na.rm=TRUE')
      } else {
        if(0 > a | a > 1)  stop('at least one alpha element is not between 0 and 1')
      }
    }
  }
  b = 1 - prod(1 - alphas, na.rm = na.rm)
  return(b)
}


#' Calculate Gamma of Irreplaceability
#'
#' This function calculates Gamma of Irreplaceability from a vector of Alpha
#' Irreplaceability values. Alpha Irreplaceability values can be calculated using
#' \code{\link{.alpha}()}.
#' 
#' Gamma of Irreplaceability is not a true irreplaceability metric, but allows
#' for ranking between sites better than Beta. It is a non-negative number where
#' the integer component represents the number of species for whom the site is
#' wholly irreplaceable (Alpha=1) plus a decimal component calculated as a Beta
#' calculated on all species for whom the site is not irreplaceable (Alpha<1).
#'
#' @param alphas vector - A vector of numbers between 0-1.
#' @param na.rm logical - Should missing values (NA) be removed?
#' @return A non-negative number.
#' @examples
#' .gamma(alphas=c(0.1,0.32,0.5))
#' .gamma(alphas=c(0.1,0.32,0.9))
#' .gamma(alphas=c(0.1,0.32,1))
#' .gamma(alphas=c(0.1,0.32,1,1))
#' .gamma(alphas=c(0.1,0.32,NA), na.rm=TRUE)
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @references \doi{10.1111/cobi.13806}
#' @export
#' @keywords internal
.gamma = function(alphas, na.rm = FALSE) {
  if(!is.vector(alphas)) stop('alphas must be a vector')
  if(!is.logical(na.rm)) stop('na.rm must be logical')
  for(a in alphas) {
    if(is.nan(a)) {
      stop('at least one alpha element is not a number')
    } else {
      if(is.na(a)) {
        if(!na.rm) stop('at least one NA element. Try with na.rm=TRUE')
      } else {
        if(0 > a | a > 1)  stop('at least one alpha element is not between 0 and 1')
      }
    }
  }
  g = sum(alphas == 1, na.rm = na.rm) + .beta(alphas[alphas != 1], na.rm = na.rm)
  return(g) 
}

# Hic Sunt Dracones
