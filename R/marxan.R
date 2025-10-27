#' Marxan input.dat parameter reader
#'
#' Reads a marxan input.dat file and returns a single parameter.
#'
#' @param input string - The address of a Marxan input.dat file.
#' @param parameter string - The name of the parameter to read.
#' @return A string.
#' @examples
#' \dontrun{
#' .marxan_read_input('/data/marxan/analysis01/input.dat', 'INPUTDIR')
#' }
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @export
#' @keywords internal
.marxan_input_parameter = function(input, parameter) {
  dat = readLines(input)
  par = strsplit(dat[grep(paste0(trimws(parameter),' '),dat)],split=' ')[[1]][2]
  return(par)
}

#' Marxan data reader
#'
#' Reads a marxan spec.dat or puvsp.dat file.
#'
#' @param input string - The address of a Marxan input.dat file.
#' @param datafile string - The file to read. either 'spec' or 'puvspr'.
#' @return A data.frame
#' @examples
#' \dontrun{
#' .marxan_data_reader('/data/marxan/analysis01/input.dat', 'spec')
#' .marxan_data_reader('/data/marxan/analysis01/input.dat', 'puvspr')
#' .marxan_data_reader('/data/marxan/analysis01/input.dat', 'pu')
#' }
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @export
#' @keywords internal
.marxan_data_reader = function(input, datafile) {
  # utils requirement
  # Normally utils is installed with base, but it is still a separate package.
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("Package \"utils\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # datafile address
  if(datafile == 'spec') {
    address = file.path(
      dirname(input),
      proxirr::.marxan_input_parameter(input, 'INPUTDIR'),
      proxirr::.marxan_input_parameter(input, 'SPECNAME')
    )
  } else if(datafile == 'puvspr') {
    address = file.path(
      dirname(input),
      proxirr::.marxan_input_parameter(input, 'INPUTDIR'),
      proxirr::.marxan_input_parameter(input, 'PUVSPRNAME')
    )
  } else if(datafile == 'pu') {
    address = file.path(
      dirname(input),
      proxirr::.marxan_input_parameter(input, 'INPUTDIR'),
      proxirr::.marxan_input_parameter(input, 'PUNAME')
    )
  } else {
    stop('unsupported data file')
  }
  # read and output data
  # try importing as a csv, then as a tab-separated, then fail
  output = try(utils::read.csv(address,stringsAsFactors=FALSE),silent=TRUE)
  if(inherits(output, "try-error"))
    output = try(utils::read.delim(address,stringsAsFactors=FALSE),silent=TRUE)
  if(inherits(output, "try-error"))
    stop("the file appears to be neither comma- or tab- delimited")
  return(output)
}

#' Marxan feature alpha irreplaceability parameters
#'
#' Returns a feature's target and global representation
#'
#' @param spec data.frame - The spec.dat as a data.frame.
#' @param puvspr data.frame - The puvspr.dat as a data.frame.
#' @param spid integer - The species ID.
#' @return A list
#' @examples
#' \dontrun{
#' .marxan_alpha_parameters(spec_dataframe, puvsp_dataframe, 17)
#' }
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @export
#' @keywords internal
.marxan_alpha_parameters = function(spec, puvspr, spid) {
  # species global representation
  global_val = sum(puvspr[puvspr$species==spid,'amount'])
  # species target
  # note: in Marxan prop is used over target
  if('prop' %in% names(spec)) {
    target_val = global_val * spec[spec$id==spid,'prop']
  } else if('target' %in% names(spec)) {
    target_val = spec[spec$id==spid,'target']
  } else {
    stop('Spec file missing both "prop" and "target"')
  }
  # compile list and return output list
  output = list(
    'global' = global_val,
    'target' = target_val
  )
  return(output)
}

#' Marxan: Alpha irreplaceabilities
#'
#' Given a valid Marxan input.dat file, it returns a copy of the PUVSPRNAME file
#' (puvspr.dat), with additional columns containing the feature's global value,
#' target value, and alpha irreplaceability.
#'
#' The global value is obtained as the sum of the feature's "amount" column in
#' PUVSPRNAME.
#'
#' The target value is obtained from the SPECNAME file (spec.dat), either as the
#' proportion of the global value (if the "prop" column is present in
#' SPECNAME), or as the "target" value in SPECNAME (otherwise). This behaviour
#' mirrors Marxan's default behaviour.
#'
#' @param input string - The address of the input.dat file.
#' @param triage logical - Should features with unachievable targets be given an
#' irreplaceability of 0? See \code{\link{alpha}}.
#' @return A data.frame
#' @examples
#' \dontrun{
#' marxan_alphas('/data/marxan/analysis01/input.dat')
#' marxan_alphas('C:\data\marxan\analysis01\input.dat')
#' }
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @export
marxan_alphas = function(input, triage=FALSE) {
  # load spec
  spec   = proxirr::.marxan_data_reader(input, 'spec')
  # load puvspr
  puvspr = proxirr::.marxan_data_reader(input, 'puvspr')
  # process species parameters
  lalpha = lapply(
    spec$id,
    function(x) {proxirr::.marxan_alpha_parameters(spec, puvspr, x)}
  )
  # process global, target and alpha values
  puvspr$global = sapply(puvspr$species, function(x) {
    g = try(lalpha[[x]]$global, silent=TRUE);
    if(inherits(g, "try-error")) g = NA
    return(g)
  })
  puvspr$target = sapply(puvspr$species, function(x) {
    t = try(lalpha[[x]]$target, silent=TRUE);
    if(inherits(t, "try-error")) t = NA
    return(t)
  })
  # must use alpha, because .alpha is not vectorized
  puvspr$alpha = proxirr::alpha(
    local  = puvspr$amount,
    global = puvspr$global,
    target = puvspr$target,
    triage = triage,
    na.allow = TRUE)
  # return
  return(puvspr)
}

#' Marxan: Beta irreplaceabilities
#'
#' Given a valid Marxan input.dat file, returns a copy of the PUNAME file
#' (pu.dat), with an additional column containing the planning unit's beta
#' irreplaceability.
#'
#' @param input string - The address of the input.dat file.
#' @param triage logical - Should features with unachievable targets be given an
#' irreplaceability of 0? See \code{\link{beta}}.
#' @return A data.frame
#' @examples
#' \dontrun{
#' marxan_betas('/data/marxan/analysis01/input.dat')
#' marxan_betas('C:\data\marxan\analysis01\input.dat')
#' }
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @export
marxan_betas = function(input, triage=FALSE) {
  # preload pu file
  pu = proxirr::.marxan_data_reader(input, 'pu')
  # precalculate all alphas
  alphas = proxirr::marxan_alphas(input, triage)
  # calculate betas
  pu$beta = sapply(
    pu$id,
    function(x) {proxirr::.beta(alphas[alphas$pu==x,'alpha'])}
  )
  # return
  return(pu)
}


#' Marxan: Gamma irreplaceabilities
#'
#' Given a valid Marxan input.dat file, returns a copy of the PUNAME file
#' (pu.dat), with an additional column containing the planning unit's gamma of
#' irreplaceability.
#'
#' @param input string - The address of the input.dat file.
#' @param triage logical - Should features with unachievable targets be given an
#' irreplaceability of 0? See \code{\link{gamma}}.
#' @return A data.frame
#' @examples
#' \dontrun{
#' marxan_gammas('/data/marxan/analysis01/input.dat')
#' marxan_gammas('C:\data\marxan\analysis01\input.dat')
#' }
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @export
marxan_gammas = function(input, triage=FALSE) {
  # preload pu file
  pu = proxirr::.marxan_data_reader(input, 'pu')
  # precalculate all alphas
  alphas = proxirr::marxan_alphas(input, triage)
  # calculate gammas
  pu$gamma = sapply(
    pu$id,
    function(x) {proxirr::.gamma(alphas[alphas$pu==x,'alpha'])}
  )
  # return
  return(pu)
}


#' Marxan: Save Alpha, Beta and Gamma Irreplaceabilities
#'
#' Saves the outputs of \code{\link{marxan_alphas}}, \code{\link{marxan_betas}},
#' and \code{\link{marxan_gammas}} in the marxan output folder.
#'
#' Three files ('_proxirr_alphas.csv', '_proxirr_betas.csv',
#' '_proxirr_gammas.csv') will be created prefixed with the scenario name
#' indicated in 'input.dat' ('SCENNAME').
#'
#' @param input string - The address of the input.dat file.
#' @param alphas logical - Should the Alpha Irreplaceability output be saved?
#' @param betas logical - Should the Beta Irreplaceability output be saved?
#' @param gammas logical - Should the Gamma of Irreplaceability output be saved?
#' @param triage logical - Should features with unachievable targets be given an
#' irreplaceability of 0? See \code{\link{alpha}}, \code{\link{beta}} and
#' \code{\link{beta}}.
#' @return TRUE
#' @examples
#' \dontrun{
#' marxan_run('/data/marxan/analysis01/input.dat')
#' marxan_run('C:\data\marxan\analysis01\input.dat')
#' }
#' @author Daniele Baisero, \email{daniele.baisero@gmail.com}
#' @export
marxan_run = function(input, alphas=TRUE, betas=TRUE, gammas=TRUE, triage=FALSE) {
  # save alphas
  if(alphas) {
    path.alphas = file.path(
      dirname(input),
      proxirr::.marxan_input_parameter(input, 'OUTPUTDIR'),
      paste(
        proxirr::.marxan_input_parameter(input, 'SCENNAME'),
        'proxirr_alphas.csv',
        sep = '_')
    )
    utils::write.csv(
      proxirr::marxan_alphas(input, triage),
      path.alphas,
      row.names = FALSE
    )
    message('Alphas saved to: ', path.alphas)
  }
  # save betas
  if(betas) {
    path.betas = file.path(
      dirname(input),
      proxirr::.marxan_input_parameter(input, 'OUTPUTDIR'),
      paste(
        proxirr::.marxan_input_parameter(input, 'SCENNAME'),
        'proxirr_betas.csv',
        sep = '_')
    )
    utils::write.csv(
      proxirr::marxan_betas(input, triage),
      path.betas,
      row.names = FALSE
    )
    message('Betas saved to:  ', path.betas)
  }
    # save gammas
  if(gammas) {
    path.gammas = file.path(
      dirname(input),
      proxirr::.marxan_input_parameter(input, 'OUTPUTDIR'),
      paste(
        proxirr::.marxan_input_parameter(input, 'SCENNAME'),
        'proxirr_gammas.csv',
        sep = '_')
    )
    utils::write.csv(
      proxirr::marxan_gammas(input, triage),
      path.gammas,
      row.names = FALSE
    )
    message('Gammas saved to:  ', path.gammas)
  }
  # return
  return(invisible(TRUE))
}

# Hic Sunt Dracones
