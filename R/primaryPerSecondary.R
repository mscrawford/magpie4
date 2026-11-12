#' @title primaryPerSecondary
#' @description Calculates the amount of primary products needed per unit of secondary
#'              (and tertiary) product, accounting for processing chains and co-product allocation.
#'              This function traces back from secondary/tertiary products through processing
#'              stages to the original primary crop inputs.
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#'              "regglo" (regional and global) or any other aggregation level defined in gdxAggregate
#' @param allocation Method for allocating primary product use when co-products are produced.
#'                   Options:
#'                   "mass" (default): Allocation based on dry matter content of outputs
#'                   "value": Allocation based on economic value of outputs
#'                   "none": No allocation - full primary input attributed to each output
#'                             (sum of allocations > 100 percent when co-products exist)
#' @param composition Where the primary composition of a region's secondary production comes from.
#'                   "shares" (default): from the input data (f20_processing_shares and conversion
#'                             factors), i.e. per tonne of secondary made from the primaries in their
#'                             historical shares.
#'                   "levels": from the model solution (ov20_dem_processing, ov20_secondary_substitutes,
#'                             ov_prod_reg), i.e. per tonne of the secondary the region actually produced.
#'                             With "mass" or "value" allocation, sum over secondaries of factor times
#'                             (production - processing use) then equals the processing use of each primary
#'                             (vm_dem_processing), less free disposal. See details.
#'
#' @details With \code{composition = "levels"} the primary content of a region's production of secondary j is
#'          the processing input that actually made j, divided over the whole production of j:
#'          \itemize{
#'            \item each process p in no_milling_ginning20 contributes ov20_dem_processing(p, k) times the
#'                  allocation weight of j in (p, k); several processes making j from k are each counted with
#'                  their own input (e.g. maize to distillers grain by distilling and by fermentation);
#'            \item tonnes relabelled by v20_secondary_substitutes carry the content of the substitute input that
#'                  covers them, pool by pool as in the q20_processing_substitution_* equations (oils by oils,
#'                  molasses by sugar, oilcakes and distillers grain by oilcake_substitutes20, split between the
#'                  two in nitrogen of the nominal origin, brans by kcereals20). The whole substitute input of a
#'                  pool is allocated, including any input above the pool's need;
#'            \item secondary inputs (e.g. sugar to alcohol, oils used as substitutes) carry the region's own
#'                  content of that secondary (a linear system, solved per region and year);
#'            \item the divisor is the whole production ov_prod_reg(j), so tonnes from the processing balance
#'                  flow dilute the content and the content of overproduced (wasted) tonnes stays with the
#'                  produced ones;
#'            \item milling and ginning outputs (brans, fibres, maize and rice oils and cakes from milling) carry
#'                  no primary content: their input is food use (milling) or the whole production (ginning) of the
#'                  crop, which is not processing use (vm_dem_processing) and stays with the crop's own uses;
#'            \item a region that does not produce j gets the factor of the world as a single region.
#'          }
#'          Input to a process without outputs (free disposal) and substitute input of a pool without
#'          relabelled tonnes are not attributed to any secondary.
#'
#'          Guards (each with a warning naming region, year and secondary; the identity above then no longer holds
#'          for that region and year):
#'          \itemize{
#'            \item processing or substitute input into a secondary the region does not produce
#'                  (ov_prod_reg <= 0) is not attributed;
#'            \item where a secondary is almost entirely made from itself (oils used as substitutes for oils,
#'                  oilcakes for oilcakes: production minus that input below 0.1 percent of production), the
#'                  content per tonne is ill-determined, and the region gets the World factor for it;
#'            \item NA levels are read as 0.
#'          }
#'          The pool structure is that of module 20 realization substitution_may21; for a gdx without its symbols
#'          (ov20_secondary_substitutes, oilcake_substitutes20, kcereals20, no_milling_ginning20) the function stops.
#'
#' @return MAgPIE object containing the amount of primary product needed per unit of secondary product (tDM/tDM)
#'
#' @author Kristine Karstens, David M Chen
#' @seealso \code{\link{production}}, \code{\link{trade}}
#' @importFrom magclass collapseNames mbind dimSums setNames getItems getYears add_dimension getNames
#' @importFrom gdx2 readGDX
#' @examples
#' \dontrun{
#' x <- primaryPerSecondary(gdx)
#' }
#' @export

primaryPerSecondary <- function(gdx,
                                file = NULL,
                                level = "reg",
                                allocation = "mass",
                                composition = "shares") {

  # Validate allocation method
  if (!allocation %in% c("mass", "value", "none")) {
    stop("allocation must be one of: 'mass', 'value', or 'none'")
  }
  if (!composition %in% c("shares", "levels")) {
    stop("composition must be one of: 'shares' or 'levels'")
  }

  # Read required sets from GDX
  ksd       <- readGDX(gdx, "ksd")           # Secondary products
  kcr       <- readGDX(gdx, "kcr")           # Crop products
  kpr       <- readGDX(gdx, "kpr")           # Products that can be processed
  primCrops <- intersect(kcr, kpr)           # Primary crops that can be processed
  simYears  <- readGDX(gdx, "t")             # Simulation years

  # Read processing parameters
  procShares <- readGDX(gdx, "f20_processing_shares")[, simYears, ] # Which primary produces which secondary
  # Conversion factors; "levels" uses the ones the model solved with
  procConvF  <- readGDX(gdx, if (composition == "levels") "i20_processing_conversion_factors"
                        else "f20_processing_conversion_factors")[, simYears, ]

  # Rename kpr dimension to distinguish from ksd
  getNames(procShares, dim = "kpr") <- paste0("kpr_", getNames(procShares, dim = "kpr"))
  getNames(procConvF, dim = "kpr")  <- paste0("kpr_", getNames(procConvF, dim = "kpr"))

  # ================================================================================
  # STEP 1: Calculate allocation weights based on chosen method
  # ================================================================================

  if (allocation == "mass") {
    # Mass-based allocation: Use conversion factors (DM output per DM input)
    # Allocation weight = output_mass / sum(all_output_masses)
    allocWeights <- procConvF / dimSums(procConvF, dim = "ksd")
    # Replace NaN with 0 (occurs when no outputs)
    allocWeights[is.nan(allocWeights)] <- 0
    allocWeights[is.infinite(allocWeights)] <- 0

  } else if (allocation == "value") {
    # Value-based allocation: Use economic value of outputs
    # Allocation weight = (output_mass * price) / sum(output_mass * price for all outputs)
    prices <- readGDX(gdx, "f15_prices_initial")[, , ksd]

    # Value = mass * price for each product
    productValues <- procConvF * prices
    
    # Normalize to get allocation weights
    allocWeights <- productValues / dimSums(productValues, dim = "ksd")
    allocWeights[is.nan(allocWeights)] <- 0
    allocWeights[is.infinite(allocWeights)] <- 0

  } else if (allocation == "none") {
    # No allocation: Each output gets full attribution of all inputs
    # Create dummy with ones where conversion factors are non-zero
    allocWeights <- procConvF
    allocWeights[allocWeights != 0] <- 1
  }

  if (composition == "levels") {
    if (level != "reg") {
      stop("Aggregation levels other than reg are currently not supported.")
    }
    getNames(allocWeights, dim = "kpr") <- sub("^kpr_", "", getNames(allocWeights, dim = "kpr"))
    x <- .primaryPerSecondaryLevels(gdx, allocWeights, simYears, ksd, setdiff(kpr, ksd))
    return(out(x[, , primCrops], file))
  }

  # ================================================================================
  # STEP 2: Calculate primary per secondary (direct processing)
  # ================================================================================
  # For each secondary product, how much of each primary product is needed?
  # Formula: (1 / conversion_factor) * processing_share * allocation_weight

  # Inverse of conversion factor = primary input per unit secondary output
  primPerSecd <- procShares * allocWeights / procConvF

  # Handle division by zero
  primPerSecd[is.nan(primPerSecd)] <- 0
  primPerSecd[is.infinite(primPerSecd)] <- 0

  # Sum over processing types to get total primary per secondary
  primPerSecd <- dimSums(primPerSecd, dim = "processing20")

  # ================================================================================
  # STEP 3: Handle tertiary processing (secondary products that are further processed)
  # ================================================================================
  # Some secondary products (ksd) can also be processed further (are in kpr)
  # e.g., sugar, molasses, oils can be inputs to further processing

  secdPrims <- intersect(kpr, ksd)  # Secondary products that can be processed further
  tertTest  <- dimSums(procConvF[,,paste0("kpr_", secdPrims)], dim = c("processing20", "kpr"))
  tertSecds <- where(tertTest > 0)$true$data

  if (length(secdPrims) > 0) {
    # Extract relationships where secondary products are inputs to tertiary processing
    secdPerTert <- primPerSecd[, , paste0("kpr_", secdPrims)][, , tertSecds]

    # Rename dimensions to track the processing chain
    getNames(secdPerTert, dim = "ksd") <- paste0("ktr_", getNames(secdPerTert, dim = "ksd"))
    getNames(secdPerTert, dim = "kpr") <- gsub("kpr_", "", getNames(secdPerTert, dim = "kpr"))
    getSets(secdPerTert, fulldim = FALSE)[3] <- "ktr.ksd"

    # Calculate primary needed for tertiary via secondary
    # tertiary output needs secondary input, which needs primary input
    primPerTert <- secdPerTert * primPerSecd[, simYears, secdPrims]

    # Handle division by zero
    primPerTert[is.nan(primPerTert)] <- 0
    primPerTert[is.infinite(primPerTert)] <- 0

    # Sum over the intermediate secondary processing step
    primPerTert <- dimSums(primPerTert, dim = "ksd")

    # Clean up dimension names
    getNames(primPerTert, dim = "kpr") <- gsub("kpr_", "", getNames(primPerTert, dim = "kpr"))
    getNames(primPerTert, dim = "ktr") <- gsub("ktr_", "", getNames(primPerTert, dim = "ktr"))

    # Combine direct secondary and tertiary calculations
    # For products that are both secondary and tertiary, we need the total
    getNames(primPerSecd, dim = "kpr") <- gsub("kpr_", "", getNames(primPerSecd, dim = "kpr"))

    # Add tertiary contributions to the direct secondary and reduce to real primary products
    primPerSecd[, , tertSecds] <- primPerSecd[, , tertSecds] + primPerTert[, , tertSecds]
    realPrim    <- setdiff(kpr, ksd)
    primPerSecd <- primPerSecd[, , realPrim]

  } else {
    # No tertiary processing, just clean up names and reduce to real primary products
    getNames(primPerSecd, dim = "kpr") <- gsub("kpr_", "", getNames(primPerSecd, dim = "kpr"))
    realPrim    <- setdiff(kpr, ksd)
    primPerSecd <- primPerSecd[, , realPrim]
  }

  # ================================================================================
  # STEP 4: Filter to only primary crops
  # ================================================================================
  x <- primPerSecd[, , primCrops]

  # ================================================================================
  # STEP 5: Apply regional aggregation if requested
  # ================================================================================
  if (level != "reg") {
    stop("Aggregation levels other than reg are currently not supported.")
    x <- gdxAggregate(gdx = gdx, x = x, weight = NULL, to = level, absolute = FALSE)
  }

  out(x, file)
}

# Primary content per tonne of each region's secondary production, built from the model levels
# (composition = "levels"; see the details of primaryPerSecondary). allocWeights: (t, processing20.ksd.kpr).
.primaryPerSecondaryLevels <- function(gdx, allocWeights, simYears, ksd, realPrim) {
  needed  <- c("ov20_secondary_substitutes", "oilcake_substitutes20", "kcereals20", "no_milling_ginning20")
  missing <- needed[vapply(needed, function(s) is.null(readGDX(gdx, s, react = "silent")), logical(1))]
  if (length(missing) > 0) {
    stop("composition = 'levels' needs module 20 realization substitution_may21 (not in gdx: ",
         paste(missing, collapse = ", "), ")")
  }
  if (dim(allocWeights)[1] != 1) {
    stop("composition = 'levels' expects global processing conversion factors")
  }
  lev <- function(name) {
    x <- unwrap(collapseNames(readGDX(gdx, name, select = list(type = "level"))[, simYears, ]))
    if (anyNA(x)) {
      warning("primaryPerSecondary: ", sum(is.na(x)), " NA level(s) of ", name, " read as 0")
      x[is.na(x)] <- 0
    }
    return(x)
  }
  dem   <- lev("ov20_dem_processing")                    # i, t, processing_subst20, kpr
  subst <- lev("ov20_secondary_substitutes")             # i, t, ksd, kpr
  prod  <- lev("ov_prod_reg")[, , ksd, drop = FALSE]     # i, t, ksd
  nrAtt <- collapseNames(readGDX(gdx, "fm_attributes")[, , "nr"])
  nr    <- as.vector(nrAtt)
  names(nr) <- getItems(nrAtt, dim = 3)
  weights   <- unwrap(allocWeights)                            # 1, t, processing20, ksd, kpr
  processes <- intersect(readGDX(gdx, "processing20"), readGDX(gdx, "no_milling_ginning20"))
  kprIn     <- dimnames(dem)[[4]]
  regions   <- dimnames(dem)[[1]]

  # substitution pools of q20_processing_substitution_*; the protein pool splits between its outputs in N of the
  # nominal origin (the unit of its equation), the other pools have one output
  pools <- list(list(input = "oils", output = "oils", nWeighted = FALSE),
                list(input = "sugar", output = "molasses", nWeighted = FALSE),
                list(input = readGDX(gdx, "oilcake_substitutes20"), output = c("distillers_grain", "oilcakes"),
                     nWeighted = TRUE),
                list(input = readGDX(gdx, "kcereals20"), output = "brans", nWeighted = FALSE))

  # tonnes of each input (column) whose content goes into the production of each secondary (row)
  edges <- function(d, s, w) {
    e <- matrix(0, length(ksd), length(kprIn), dimnames = list(ksd, kprIn))
    for (p in processes) {
      e <- e + sweep(w[p, ksd, kprIn], 2, d[p, kprIn], "*")
    }
    for (pool in pools) {
      unit   <- if (pool$nWeighted) nr[colnames(s)] else rep(1, ncol(s))
      weight <- as.vector(s[pool$output, , drop = FALSE] %*% unit)
      if (sum(weight) > 0) {
        e[pool$output, pool$input] <- e[pool$output, pool$input] +
          outer(weight / sum(weight), d["substitutes", pool$input])
      }
    }
    return(e)
  }

  # content per tonne of production: p_j z_j = e_jk + sum_m e_jm z_m, where p > 0; elsewhere the fallback
  solveContent <- function(e, p, fallback, where) {
    es  <- matrix(0, length(ksd), length(ksd), dimnames = list(ksd, ksd))
    sec <- intersect(kprIn, ksd)
    es[, sec] <- e[, sec]
    lost <- p <= 0 & rowSums(e) > 1e-8
    if (any(lost)) {
      warning("primaryPerSecondary: ", where, ": input into ", paste(ksd[lost], collapse = ", "),
              " without production; its primary content is not attributed")
    }
    weak <- p > 0 & p - diag(es) <= 1e-3 * p
    if (any(weak)) {
      warning("primaryPerSecondary: ", where, ": ", paste(ksd[weak], collapse = ", "),
              " almost entirely made from itself; World factor used")
    }
    pos <- p > 0 & !weak
    z   <- fallback
    if (any(pos)) {
      rhs <- e[pos, realPrim, drop = FALSE] + es[pos, !pos, drop = FALSE] %*% fallback[!pos, , drop = FALSE]
      z[pos, ] <- solve(diag(p[pos], sum(pos)) - es[pos, pos, drop = FALSE], rhs)
    }
    return(z)
  }

  x <- new.magpie(regions, simYears, as.vector(outer(ksd, realPrim, paste, sep = ".")), fill = 0,
                  sets = c("i", "t_all", "ksd", "kpr"))
  zero <- matrix(0, length(ksd), length(realPrim), dimnames = list(ksd, realPrim))
  for (t in simYears) {
    w <- weights[1, t, , , ]
    world <- solveContent(edges(apply(dem[, t, , , drop = FALSE], c(3, 4), sum),
                                apply(subst[, t, , , drop = FALSE], c(3, 4), sum), w),
                          apply(prod[, t, , drop = FALSE], 3, sum), zero, paste("World", t))
    for (r in regions) {
      z <- solveContent(edges(dem[r, t, , ], subst[r, t, , ], w), prod[r, t, ], world, paste(r, t))
      x[r, t, ] <- as.vector(z)
    }
  }
  return(x)
}
