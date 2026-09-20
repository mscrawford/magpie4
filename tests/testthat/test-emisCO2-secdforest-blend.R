# emisCO2: the secondary-forest natural-origin cohorts.
#
# Defect fixed 2026-09-20 (see the CONSTANTS note in R/emisCO2.R). GAMS blends the FRA-calibrated age-class
# curve pm_carbon_density_secdforest_ac with the uncalibrated natveg curve
# pm_carbon_density_secdforest_ac_uncalib using the PRESOLVE natural-origin share
# (35_natveg/pot_forest_may24/presolve.gms:253-257), haircuts the blend afterwards (presolve.gms:487-488) and
# exports it as p35_carbon_density_secdforest. emisCO2 emulated that blend with the POSTSOLVE export
# p35_secdforest_natural over the solved area and with the uncalibrated curve un-haircut, which overstated the
# secdforest vegc stock by +490 MtC World in 2100 with the edge switch ON and shifted it by up to 20 MtC with
# it OFF. Reading the blend verbatim as the density fixes the level but moves the natural-origin composition
# drift (~300 Mt CO2/yr) from land-use change into the density channel (the reported Indirect line); the owner
# decided on 2026-09-20 to keep the level and keep the drift in land-use change. The emulation is therefore
# kept, with the presolve natural-origin area recovered from the exported blend and a haircut-consistent gap.
#
# These tests need the two 600 MB L4 gate gdx and cost one ~30 s emisCO2 call each, so every block skips when
# the files are absent and the runs are cached across blocks. Override the paths with the environment
# variables MAGPIE4_L4GATE_ON_GDX / MAGPIE4_L4GATE_OFF_GDX.

.emisCO2PrefixRev <- "40c00056"   # last revision before the fix; R/emisCO2.R there is the pre-fix function

.l4GateGdx <- function(which) {
  fromEnv <- Sys.getenv(paste0("MAGPIE4_L4GATE_", which, "_GDX"), "")
  if (nzchar(fromEnv)) {
    return(fromEnv)
  }
  runDir <- switch(which,
                   ON  = "SSP2base_L4gate_ON_2026-09-19_14.25.45",
                   OFF = "SSP2base_L4gate_OFF_2026-09-19_14.30.30")
  # nolint start: absolute_path_linter. The L4 gate runs are machine-local; override with the env vars above.
  file.path("/Users/turnip/Documents/Work/Projects/Fragmentation/libraries/magpie/output", runDir, "fulldata.gdx")
  # nolint end
}

.skipUnlessGateGdx <- function(which) {
  gdx <- .l4GateGdx(which)
  testthat::skip_if_not(file.exists(gdx), paste0("L4 gate gdx not available: ", gdx))
  gdx
}

# --- instrument 1: capture emisCO2's internal objects ------------------------------------------------------
# totalStock is magpie4::carbonstock (read straight from ov_carbon_stock), totalStockCheck is emisCO2's own
# area x density reconstruction, output is the per-cell result in MtC/yr before pool/land aggregation. The
# capture is spliced in as one extra statement immediately before the .validateCalculation() call, so no
# calculation is altered.
.injectStockCapture <- function(fn, env) {
  parts      <- as.list(body(fn))
  statements <- parts[-1]
  isValidate <- vapply(statements,
                       function(e) is.call(e) && identical(deparse(e[[1]]), ".validateCalculation"),
                       logical(1))
  stopifnot(sum(isValidate) == 1)
  j   <- which(isValidate)
  cap <- bquote(assign("captured",
                       list(totalStock = totalStock, totalStockCheck = totalStockCheck, output = output),
                       envir = .(env)))
  body(fn) <- as.call(c(list(as.name("{")), statements[seq_len(j - 1)], list(cap),
                        statements[j:length(statements)]))
  fn
}

# --- instrument 2: intercept one gdx symbol inside emisCO2's own body ---------------------------------------
# Masks readGDX in a child of the package namespace, so only the calls written inside emisCO2 are affected.
# `transform` receives the real value and returns what emisCO2 should see; returning NULL hides the symbol,
# which puts the function on its no-blend (upstream) path.
.interceptGdxSymbol <- function(fn, symbol, transform) {
  realReadGDX <- get("readGDX", envir = environment(fn))
  e <- new.env(parent = environment(fn))
  e$readGDX <- function(gdx, ...) {
    dots  <- list(...)
    named <- names(dots)
    if (is.null(named)) named <- rep("", length(dots))
    requested <- unlist(dots[named == ""])
    if (length(requested) == 1L && identical(as.character(requested), symbol)) {
      return(transform(realReadGDX(gdx, ...)))
    }
    realReadGDX(gdx, ...)
  }
  environment(fn) <- e
  fn
}

.hideGdxSymbol <- function(fn, symbol) {
  .interceptGdxSymbol(fn, symbol, function(x) NULL)
}

.emisCO2Cache <- new.env(parent = emptyenv())

.runEmisCO2 <- function(key, fn, gdx) {
  if (!is.null(.emisCO2Cache[[key]])) {
    return(.emisCO2Cache[[key]])
  }
  captureEnv   <- new.env()
  instrumented <- .injectStockCapture(fn, captureEnv)
  warnEnv      <- new.env(parent = emptyenv())
  warnEnv$warns <- character(0)
  collect <- function(w) {
    assign("warns", c(warnEnv$warns, conditionMessage(w)), envir = warnEnv)
    invokeRestart("muffleWarning")
  }
  out <- withCallingHandlers(instrumented(gdx, level = "cell", sum_cpool = FALSE, sum_land = FALSE),
                             warning = collect)
  res <- list(out = out, warnings = warnEnv$warns,
              totalStock      = captureEnv$captured$totalStock,
              totalStockCheck = captureEnv$captured$totalStockCheck,
              cellOutput      = captureEnv$captured$output)
  assign(key, res, envir = .emisCO2Cache)
  res
}

# The pre-fix function, sourced from a fixed revision into an environment whose parent is the namespace.
.prefixEmisCO2 <- function() {
  if (!is.null(.emisCO2Cache$prefixFn)) {
    return(.emisCO2Cache$prefixFn)
  }
  repo <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  testthat::skip_if_not(dir.exists(file.path(repo, ".git")), "not a git working copy")
  testthat::skip_if(unname(Sys.which("git")) == "", "git not on PATH")
  src <- suppressWarnings(system2("git",
                                  c("-C", shQuote(repo), "show",
                                    shQuote(paste0(.emisCO2PrefixRev, ":R/emisCO2.R"))),
                                  stdout = TRUE, stderr = FALSE))
  testthat::skip_if(!is.null(attr(src, "status")) || length(src) == 0,
                    paste0("cannot read R/emisCO2.R at ", .emisCO2PrefixRev))
  testthat::skip_if(any(grepl("p35_carbon_density_secdforest", src, fixed = TRUE)),
                    paste0(.emisCO2PrefixRev, " is not a pre-fix revision"))
  srcFile <- withr::local_tempfile(fileext = ".R", .local_envir = .emisCO2Cache)
  writeLines(src, srcFile)
  prefixEnv <- new.env(parent = asNamespace("magpie4"))
  sys.source(srcFile, envir = prefixEnv, keep.source = FALSE)
  .emisCO2Cache$prefixFn <- prefixEnv$emisCO2
  .emisCO2Cache$prefixFn
}

# ov_carbon_stock(secdforest, <pool>, actual) in MtC -- the model's own stock, per cluster and year.
.refSecdforestStock <- function(gdx, pool) {
  cs <- gdx2::readGDX(gdx, "ov_carbon_stock", select = list(type = "level"))
  magclass::collapseNames(cs[, , "secdforest"][, , pool][, , "actual"])
}

.recSecdforestStock <- function(run, pool) {
  magclass::collapseNames(run$totalStockCheck[, , "secdforest"][, , pool])
}

# secdforest vegc World flux of one emission type, Mt CO2/yr (the cell output is MtC/yr).
.secdforestChannel <- function(run, type) {
  x <- run$cellOutput[, , type][, , "secdforest"][, , "vegc"]
  .worldByYear(x) * 44 / 12
}

.worldByYear <- function(x) {
  stats::setNames(vapply(magclass::getYears(x), function(y) sum(x[, y, ]), numeric(1)), magclass::getYears(x))
}

# The presolve natural-origin share and haircut-consistent gap, recomputed here from the gdx alone.
.presolveNatural <- function(gdx) {
  blend <- gdx2::readGDX(gdx, "p35_carbon_density_secdforest")
  years <- magclass::getYears(blend)
  cal   <- magclass::collapseNames(gdx2::readGDX(gdx, "pm_carbon_density_secdforest_ac")[, years, "vegc"])
  uncalRaw <- gdx2::readGDX(gdx, "pm_carbon_density_secdforest_ac_uncalib")[, years, "vegc"]
  uncal <- magclass::collapseNames(uncalRaw)
  blendVeg <- magclass::collapseNames(blend[, , "vegc"])
  degr <- gdx2::readGDX(gdx, "p35_degr_applied")[, years, "secdforest"]
  retention <- cal
  retention[, , ] <- 1
  for (driver in magclass::getItems(degr, dim = "degr35")) {
    retention <- retention * (1 - magclass::collapseNames(degr[, , driver]))
  }
  gap  <- cal - uncal * retention
  area <- gdx2::readGDX(gdx, "ov35_secdforest", select = list(type = "level"))[, years, ]
  natPost <- gdx2::readGDX(gdx, "p35_secdforest_natural")[, years, ]
  sharePost <- area
  sharePost[, , ] <- 0
  hasArea <- area > 1e-10
  sharePost[hasArea] <- pmin(natPost[hasArea], area[hasArea]) / area[hasArea]
  defined <- abs(gap) > 1e-10
  share <- gap
  share[, , ] <- 0
  share[defined]  <- (cal - blendVeg)[defined] / gap[defined]
  share[!defined] <- sharePost[!defined]
  list(share = share, gap = gap, area = area, natPre = share * area, natPost = natPost,
       sharePost = sharePost, defined = defined, retention = retention, blendVeg = blendVeg, cal = cal)
}


test_that("edge-ON: reconstructed secdforest vegc and litc stocks equal ov_carbon_stock", {
  gdx <- .skipUnlessGateGdx("ON")
  run <- .runEmisCO2("fixON", magpie4::emisCO2, gdx)

  for (pool in c("vegc", "litc")) {
    rec <- .recSecdforestStock(run, pool)
    ref <- .refSecdforestStock(gdx, pool)[, magclass::getYears(rec), ]
    expect_lt(max(abs(rec - ref)), 1e-6) # MtC, every cluster and year
  }
  expect_equal(run$warnings, character(0))
})


test_that("edge-OFF: reconstructed secdforest vegc and litc stocks equal ov_carbon_stock", {
  gdx <- .skipUnlessGateGdx("OFF")
  run <- .runEmisCO2("fixOFF", magpie4::emisCO2, gdx)

  for (pool in c("vegc", "litc")) {
    rec <- .recSecdforestStock(run, pool)
    ref <- .refSecdforestStock(gdx, pool)[, magclass::getYears(rec), ]
    expect_lt(max(abs(rec - ref)), 1e-6)
  }
  expect_equal(run$warnings, character(0))
})


test_that("the presolve natural-origin share recovered from the blend is a share, and reproduces the level", {
  for (which in c("ON", "OFF")) {
    gdx <- .skipUnlessGateGdx(which)
    p <- .presolveNatural(gdx)
    expect_true(all(p$share >= -1e-9 & p$share <= 1 + 1e-9))
    # the level identity that makes this construction exact: area * cal - natPre * gap == area * blend
    lvl <- magclass::dimSums(p$area * p$cal, dim = 3) - magclass::dimSums(p$natPre * p$gap, dim = 3)
    ref <- .refSecdforestStock(gdx, "vegc")[, magclass::getYears(p$area), ]
    expect_lt(max(abs(lvl - ref)), 1e-6)
    # where the gap vanishes the share is 0/0 and the postsolve share stands in; those cells are the ac0
    # class only, and its gap is identically zero in every year, so the choice cannot affect any flux term.
    anyUndefined <- apply(as.array(!p$defined), 3, any)
    undefinedAc <- unique(magclass::getItems(p$gap, dim = "ac")[which(anyUndefined)])
    expect_identical(undefinedAc, "ac0")
    expect_equal(max(abs(p$gap[, , undefinedAc])), 0)
  }
})


test_that("edge-OFF: the recovered presolve share equals the postsolve share through 2060", {
  # NOT because the natural-origin area is zero there -- it is 9-23 Mha from 2025 on. The two coincide
  # because the natural-origin cohorts are harvest-protected, so within a 5-year step (one age class) the
  # presolve and postsolve natural areas agree; they part company on the 10-year steps from 2070.
  gdx <- .skipUnlessGateGdx("OFF")
  p <- .presolveNatural(gdx)
  through2060 <- paste0("y", seq(1995, 2060, 5))
  # The natural-origin AREA agrees to gdx write precision (measured 3.1e-06 Mha). The implied SHARE agrees to
  # 3.3e-16 in every cluster-age-class holding more than 0.01 Mha of secondary forest; the largest residual
  # anywhere, 3.0e-03, sits in a 1.0e-03 Mha cluster where natPre 5.69e-05 vs natPost 6.00e-05 Mha is the
  # precision of the postsolve export itself, amplified by dividing by a tiny area.
  expect_lt(max(abs((p$natPre - p$natPost)[, through2060, ])), 1e-5)
  bigEnough <- p$area[, through2060, ] > 0.01
  expect_lt(max(abs((p$share - p$sharePost)[, through2060, ])[bigEnough]), 1e-6)
  expect_lt(max(abs((p$share - p$sharePost)[, through2060, ])), 1e-2)
  worldNat <- .worldByYear(p$natPre)
  expect_gt(min(worldNat[paste0("y", seq(2025, 2060, 5))]), 9)   # Mha, not zero
  expect_lt(max(worldNat[paste0("y", seq(2025, 2060, 5))]), 25)
  expect_gt(max(abs((p$natPre - p$natPost)[, paste0("y", c(2070, 2080, 2090, 2100)), ])), 0.1)
})


test_that("the composition drift stays in land-use change: Indirect tracks the pre-fix code", {
  # The channel test the audit asked for: emisCC on secdforest vegc must stay within 5 Mt CO2/yr of the
  # pre-fix code in every year on both runs, while emisArea absorbs the level correction.
  prefixFn <- .prefixEmisCO2()
  for (which in c("ON", "OFF")) {
    gdx    <- .skipUnlessGateGdx(which)
    fixed  <- .runEmisCO2(paste0("fix", which), magpie4::emisCO2, gdx)
    prefix <- .runEmisCO2(paste0("prefixSource", which), prefixFn, gdx)

    ccFixed  <- .secdforestChannel(fixed,  "cc")
    ccPrefix <- .secdforestChannel(prefix, "cc")
    expect_lt(max(abs(ccFixed - ccPrefix), na.rm = TRUE), 5)

    # the level correction lands in lu, and cc + lu still reproduces the net change
    luFixed    <- .secdforestChannel(fixed,  "lu")
    luPrefix   <- .secdforestChannel(prefix, "lu")
    totalFixed <- .secdforestChannel(fixed,  "total")
    expect_equal(unname(ccFixed + luFixed), unname(totalFixed), tolerance = 1e-6)
    expect_gt(max(abs(luFixed - luPrefix), na.rm = TRUE), 5)
  }
  # the edge-ON level correction, World 2100 (Mt CO2/yr), lands in lu
  fixedON  <- .runEmisCO2("fixON", magpie4::emisCO2, .l4GateGdx("ON"))
  prefixON <- .runEmisCO2("prefixSourceON", prefixFn, .l4GateGdx("ON"))
  expect_equal(.secdforestChannel(fixedON, "total")[["y2100"]] -
                 .secdforestChannel(prefixON, "total")[["y2100"]], 44.44, tolerance = 1e-2)
})


test_that("failing-first: the pre-fix rebuild misses the secdforest vegc stock by 490 MtC", {
  gdx    <- .skipUnlessGateGdx("ON")
  prefix <- .runEmisCO2("prefixHiddenON",
                        .hideGdxSymbol(magpie4::emisCO2, "p35_carbon_density_secdforest"), gdx)
  fixed  <- .runEmisCO2("fixON", magpie4::emisCO2, gdx)

  rec <- .recSecdforestStock(prefix, "vegc")
  ref <- .refSecdforestStock(gdx, "vegc")[, magclass::getYears(rec), ]
  gap <- rec - ref
  world <- .worldByYear(gap)

  expect_equal(world[["y2100"]], 489.99, tolerance = 1e-3)  # MtC World
  expect_equal(max(abs(gap)), 128.27, tolerance = 1e-3)     # MtC, worst cluster-year
  expect_true(all(abs(world[paste0("y", c(1995, 2000, 2005, 2010, 2015, 2020))]) < 1e-6))
  expect_gt(world[["y2050"]], 90)
  expect_lt(max(abs(.recSecdforestStock(fixed, "vegc") - ref)), 1e-6)

  # net land CO2 consequence, World 2100, Mt CO2/yr
  expect_equal(sum(fixed$out[, "y2100", "total"]) - sum(prefix$out[, "y2100", "total"]),
               44.44, tolerance = 1e-2)
})


test_that("the stock-consistency check now catches an above-ground mismatch under dynSom", {
  # Before the fix the check collapsed to a soilc-only comparison under dynSom, so no above-ground error
  # could ever trip it. Feeding emisCO2 a blend that does not match ov_carbon_stock (one cluster scaled)
  # must now warn -- and the old soilc-only condition must still be satisfied, which is exactly why the old
  # check stayed silent on the 490 MtC error.
  gdx <- .skipUnlessGateGdx("ON")
  perturb <- function(x) {
    x[1, , "vegc"] <- x[1, , "vegc"] * 1.05
    x
  }
  run <- .runEmisCO2("perturbedBlendON",
                     .interceptGdxSymbol(magpie4::emisCO2, "p35_carbon_density_secdforest", perturb), gdx)

  expect_true(any(grepl("differ from magpie4::carbonstock", run$warnings)))
  soilcOld <- max(abs(magclass::dimSums(run$totalStock[, , "soilc"], dim = 3) -
                        magclass::dimSums(run$totalStockCheck[, , "soilc"], dim = 3)), na.rm = TRUE)
  expect_lt(soilcOld, 1e-3)  # the pre-fix condition: satisfied, hence silent
  expect_gt(max(abs(.recSecdforestStock(run, "vegc") - .refSecdforestStock(gdx, "vegc")), na.rm = TRUE), 1)
})


test_that("fallback branch: a gdx without p35_carbon_density_secdforest reproduces the pre-fix result", {
  gdx      <- .skipUnlessGateGdx("ON")
  prefixFn <- .prefixEmisCO2()
  prefix   <- .runEmisCO2("prefixSourceON", prefixFn, gdx)
  hidden   <- .runEmisCO2("prefixHiddenON",
                          .hideGdxSymbol(magpie4::emisCO2, "p35_carbon_density_secdforest"), gdx)

  expect_identical(magclass::getNames(hidden$out), magclass::getNames(prefix$out))
  expect_identical(magclass::getYears(hidden$out), magclass::getYears(prefix$out))
  expect_identical(as.vector(as.array(hidden$out)), as.vector(as.array(prefix$out)))
  # including the warning behaviour: the no-blend path deliberately keeps the old soilc-only relaxation, so
  # upstream gdx (whose above-ground reconstruction still carries the 10-year-step share mismatch) stay quiet
  expect_equal(hidden$warnings, character(0))
  expect_equal(prefix$warnings, character(0))
})


test_that("the exported blend differs from the calibrated curve in vegc only", {
  # Why litc needs no treatment: the M52 calibration and the edge haircut are both vegc-only
  # (presolve.gms:487-490), so the litc blend is the calibrated litc curve.
  gdx <- .skipUnlessGateGdx("ON")
  blend <- gdx2::readGDX(gdx, "p35_carbon_density_secdforest")
  cal   <- gdx2::readGDX(gdx, "pm_carbon_density_secdforest_ac")[, magclass::getYears(blend), ]
  expect_identical(magclass::getItems(blend, dim = "ag_pools"), c("vegc", "litc"))
  expect_identical(magclass::getItems(blend, dim = 3), magclass::getItems(cal, dim = 3))
  expect_identical(as.vector(blend[, , "litc"]), as.vector(cal[, , "litc"]))
  expect_gt(max(abs(blend[, , "vegc"] - cal[, , "vegc"])), 1)
})
