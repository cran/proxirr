#' Calculate Alpha Irreplaceability
#'
#' This function calculates Alpha irreplaceability. Inputs can be single
#' parameter values needed to calculate Alpha, vectors of parameter values to
#' calculate a vector of Alpha values, or a \code{data.frame} with columns
#' containing parameters needed to calculate a vector of Alpha values.
#' \itemize{
#'   \item \strong{Single Alpha measurement:} If \code{local}, \code{global} and
#'   \code{target} are numeric, it calculates and returns Alpha irreplaceability.
#'   \item \strong{Vectorized Alpha measurement:} If \code{local}, \code{global}
#'   and \code{target} are vectors of the same length, a vector of Alpha
#'   irreplaceability values will be calculated and returned.
#'   \item \strong{Dataframe Alpha measurement:} If \code{df} is provided and
#'   \code{local}, \code{global} and \code{target} are strings representing
#'   field names in \code{df}, a vector of Alpha irreplaceability values will be
#'   caluclated and returned. \strong{Optionally:} If \code{alpha_col} is also
#'   provided, a \code{data.frame} identical to \code{df} will be returned with
#'   the calculated Alpha values in the \code{alpha_col} column.
#' }
#'
#' @param local number, vector or string - The feature's representation at the
#' site, or the name of the column containing the feature's representation at
#' the site.
#' @param global number, vector or string - The feature's globally available
#' representation, or the name of the column containing the feature's globally
#' available representation.
#' @param target number, vector or string - The feature's target, or the name of
#' the column containing the feature's target.
#' @param df data.frame - Optional; an input data.frame.
#' @param alpha_col string - The name of the column where to write alpha values.
#' If both df and alpha_col are provided, the output will be the input dataframe
#' with the additional column.
#' @param overwrite logical - Should \code{alpha_col} be overwritten if it
#' already exists?
#' @param triage logical - Should features with unachievable targets be ignored?
#' Defaults to \code{FALSE}. If \code{FALSE}, these species will be always
#' assigned an Alpha irreplaceability of \code{1} wherever they occur. If
#' \code{TRUE}, these species will always be assigned an Alpha irreplaceabiltiy
#' of \code{0}.
#' @param na.allow logical - Allaw \code{NA} values in input? If \code{TRUE},
#' \code{NA} values in local, global or target walues will result in \code{NA}
#' being returned, otherwise an error will be raised. Defaults to \code{FALSE}
#' for single Alpha calculations, and to \code{TRUE} for calculations over
#' vectors or data.frames.
#' @return A number, vector or data.frame
#' @examples
#' alpha(1, 100, 45)
#' alpha(c(1,25,45), c(100,100,100), c(50,50,50))
#' dtfrm = data.frame(
#'   loc = c(1,25,45),
#'   glob = c(100,100,100),
#'   targ = c(50,50,50)
#' )
#' alpha('loc', 'glob', 'targ', df = dtfrm)
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @export
alpha = function(local, global, target, df = NULL, alpha_col = NULL, triage = FALSE, na.allow = NULL, overwrite = FALSE) {
  # custom vector test
  # needed for vector evaluation, because is.vector(NA) evaluates to TRUE
  vtest = function(x) return(is.null(dim(x)) & length(x)>1) # replicated in beta
  #
  # single run
  if(is.null(df) & !vtest(local) & !vtest(global) & !vtest(target)) {
    # explicit NA failure preferred for single alpha
    if(is.null(na.allow)) na.allow = FALSE
    alpha = proxirr::.alpha(local, global, target, triage, na.allow)
    return(alpha)
  }
  #
  # vector run
  if(is.null(df) & vtest(local) & vtest(global) & vtest(target)) {
    if((length(local) != length(global)) | (length(local) != length(target))) {
      stop('local, global and target are of different lengths')
    }
    # graceful NA failure preferred for vector alpha
    if(is.null(na.allow)) na.allow = TRUE
    alphas = mapply(proxirr::.alpha, local, global, target, triage, na.allow)
    return(alphas)
  }
  #
  # data.frame run
  if(is.data.frame(df)) {
    if(!is.character(local) | !is.character(global) | !is.character(target)) {
      stop('If df is supplied, local, global and target must be character strings')
    }
    # graceful NA failure preferred for data.frame alpha
    if(is.null(na.allow)) na.allow = TRUE
    alphas = mapply(proxirr::.alpha, df[,local], df[,global], df[,target], triage, na.allow)
    if(is.null(alpha_col)) {
      return(alphas)
    } else {
      if(alpha_col %in% colnames(df) & !overwrite) {
        stop('Column already exists. Set overwrite to TRUE or choose a new name')
      }
      df[,alpha_col] = alphas
      return(df)
    }
  }
  stop('Unexpected input.')
}


#' Calculate Beta Irreplaceability
#'
#' This function calculates Beta irreplaceability. Inputs can be either a vector
#' of Alpha values, or a \code{data.frame} containing all necessary parameters
#' needed to calculate Alpha values on a row-by-row basis.
#' \itemize{
#'   \item \strong{Vector Beta measurement:} If \code{data} is a vector of Alpha
#'   irreplaceability values, a single Beta value will be calculated and
#'   returned.
#'   \item \strong{Dataframe Beta measurement:} If \code{data} is a
#'   \code{data.frame} and \code{local}, \code{global} and \code{target} are
#'   strings representing field names in \code{data}, a vector of Alpha
#'   irreplaceability values will be caluclated using these fields, and a Beta
#'   irreplaceability value will be calculated on these, and returned.
#' }

#' @param data vector or data.frame - The input over which to calculate Beta.
#' @param local string - The name of the column containing the feature's
#' representation at the site. Needed if data is a \code{data.frame}
#' @param global string - The name of the column containing the feature's total
#' available representation. Needed if data is a \code{data.frame}
#' @param target string - The name of the column containing the feature's target.
#'Needed if data is a \code{data.frame}
#' @param triage logical - Should features with unachievable targets be ignored?
#' Defaults to FALSE. If FALSE, these species will be always assigned an Alpha
#' irreplaceability of 1 wherever they occur. If TRUE, these species will always
#' be assigned an Alpha irreplaceabiltiy of 0.
#' @param na.rm logical - Should lines with missing values (NA) be ignored? If
#' data is a \code{vector}, NA values will be removed when calculating Beta. If
#' data is a \code{data.frame}, Alpha values will be calculated using
#' \code{\link{alpha}} with \code{na.allow} set to TRUE, and then Beta
#' calculated ignoring \code{NA} values.
#' @return A number
#' @examples
#' beta(c(0.01, 0.5, 0.5))
#' dtfrm = data.frame(
#'   loc = c(1,25,45),
#'   glob = c(100,100,100),
#'   targ = c(50,50,50)
#' )
#' beta(dtfrm, local = 'loc', global = 'glob', target = 'targ')
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @export
beta = function(data, local = NULL, global = NULL, target = NULL, triage = FALSE, na.rm = TRUE) {
  # custom vector test
  # needed for vector evaluation, because is.vector(NA) evaluates to TRUE
  vtest = function(x) return(is.null(dim(x)) & length(x)>1) # replicated in alpha
  #
  # data vector run
  if(vtest(data)) {
    b = proxirr::.beta(data, na.rm = na.rm)
    return(b)
  }
  if(is.data.frame(data)) {
    alphas = proxirr::alpha(local, global, target, df = data, triage = triage, na.allow = na.rm)
    b = proxirr::.beta(alphas, na.rm = na.rm)
    return(b)
  }
  stop('Unexpected input; data should be a vector or data.frame.')
}


# Hic Sunt Dracones
