
# ============================================================
# DICE2023 in R - Part 1: Parameters & Initialization
# ============================================================

dice2023_params <- function() {
  p <- list()
  
  # Time
  p$tstep    <- 5
  p$T        <- 81   # periods
  p$yr0      <- 2020
  
  # Population and technology
  p$gama     <- 0.300
  p$pop1     <- 7752.9
  p$popadj   <- 0.145
  p$popasym  <- 10825.0
  p$dk       <- 0.100
  p$q1       <- 135.7
  p$AL1      <- 5.84
  p$gA1      <- 0.066
  p$delA     <- 0.0015
  
  # Emissions / sigma
  p$gsigma1  <- -0.015
  p$delgsig  <- 0.96
  p$asymgsig <- -0.005
  p$e1       <- 37.56
  p$miu1     <- 0.05
  p$fosslim  <- 6000
  p$CumEmiss0 <- 633.5
  
  # Damage
  p$a1       <- 0.0
  p$a2base   <- 0.003467
  p$a3       <- 2.00
  
  # Abatement cost
  p$expcost2 <- 2.6
  p$pback2050 <- 515.0
  p$gback    <- -0.012
  p$cprice1  <- 6.0
  p$gcprice  <- 0.025
  
  # MIU limits
  p$limmiu2070 <- 1.0
  p$limmiu2120 <- 1.1
  p$limmiu2200 <- 1.05
  p$limmiu2300 <- 1.0
  p$delmiumax  <- 0.12
  
  # Preferences
  p$betaclim <- 0.5
  p$elasmu   <- 0.95
  p$prstp    <- 0.001
  p$pi       <- 0.050
  
  # Capital
  p$k0       <- 295.0
  p$siggc1   <- 0.01
  
  # Scaling
  p$scale1   <- 0.00891061
  p$scale2   <- -6275.91
  
  # FAIR carbon cycle
  p$emshare0 <- 0.2173
  p$emshare1 <- 0.224
  p$emshare2 <- 0.2824
  p$emshare3 <- 0.2763
  p$tau0     <- 1000000
  p$tau1     <- 394.4
  p$tau2     <- 36.53
  p$tau3     <- 4.304
  p$teq1     <- 0.324
  p$teq2     <- 0.44
  p$d1       <- 236.0
  p$d2       <- 4.07
  p$irf0     <- 32.4
  p$irC      <- 0.019
  p$irT      <- 4.165
  p$fco22x   <- 3.93
  
  # FAIR initial conditions
  p$mat0     <- 886.5128014
  p$res00    <- 150.093
  p$res10    <- 102.698
  p$res20    <- 39.534
  p$res30    <- 6.1865
  p$mateq    <- 588.0
  p$tbox10   <- 0.1477
  p$tbox20   <- 1.099454
  p$tatm0    <- 1.24715
  
  # Non-CO2 GHG
  p$eland0         <- 5.9
  p$deland         <- 0.1
  p$F_Misc2020     <- -0.054
  p$F_Misc2100     <- 0.265
  p$F_GHGabate2020 <- 0.518
  p$F_GHGabate2100 <- 0.957
  p$ECO2eGHGB2020  <- 9.96
  p$ECO2eGHGB2100  <- 15.5
  p$emissrat2020   <- 1.40
  p$emissrat2100   <- 1.21
  p$Fcoef1         <- 0.00955
  p$Fcoef2         <- 0.861
  
  # Savings rate fixed after period 37
  p$optlrsav <- 0.28
  
  return(p)
}

cat("Parameters function defined.\
")



# ============================================================
# DICE2023 in R - Part 2: Pre-compute exogenous time paths
# ============================================================

compute_exogenous <- function(p) {
  T <- p$T
  t_idx <- 1:T
  
  ex <- list()
  
  # Population (millions)
  ex$L <- p$pop1 * (p$popasym / p$pop1)^(1 - (1 - p$popadj)^(t_idx - 1))
  
  # TFP
  ex$AL <- p$AL1 * exp(p$gA1 / p$delA * (1 - exp(-p$delA * (t_idx - 1))))
  
  # Sigma growth rate
  gsig <- pmax(p$gsigma1 * p$delgsig^(t_idx - 1), p$asymgsig)
  # Sigma (emissions intensity)
  ex$sigma <- numeric(T)
  ex$sigma[1] <- p$e1 / (p$q1 * (1 - p$miu1))
  for (i in 2:T) {
    ex$sigma[i] <- ex$sigma[i-1] * exp(p$tstep * gsig[i-1])
  }
  
  # Backstop price
  pback_t <- p$pback2050 * exp(p$gback * p$tstep * (t_idx - 7))  # period 7 ~ 2050
  ex$pbacktime <- pback_t
  
  # Land emissions (GtCO2/yr)
  ex$eland <- p$eland0 * (1 - p$deland)^(t_idx - 1)
  
  # Non-CO2 GHG abateable emissions
  ex$CO2E_GHGabateB <- ifelse(
    t_idx <= 16,
    p$ECO2eGHGB2020 + ((p$ECO2eGHGB2100 - p$ECO2eGHGB2020) / 16) * (t_idx - 1),
    p$ECO2eGHGB2100
  )
  
  # Misc forcings
  ex$F_Misc <- ifelse(
    t_idx <= 16,
    p$F_Misc2020 + ((p$F_Misc2100 - p$F_Misc2020) / 16) * (t_idx - 1),
    p$F_Misc2100
  )
  
  # Emission ratio CO2e / industrial CO2
  ex$emissrat <- ifelse(
    t_idx <= 16,
    p$emissrat2020 + ((p$emissrat2100 - p$emissrat2020) / 16) * (t_idx - 1),
    p$emissrat2100
  )
  
  ex$sigmatot  <- ex$sigma * ex$emissrat
  ex$cost1tot  <- ex$pbacktime * ex$sigmatot / p$expcost2 / 1000
  
  # Discount factor rr
  ex$rr <- 1 / (1 + p$prstp + p$pi * p$siggc1)^(p$tstep * (t_idx - 1))
  
  # MIU upper bounds
  ex$miuup <- numeric(T)
  for (i in t_idx) {
    yr <- p$yr0 + (i - 1) * p$tstep
    if (yr < 2070)       ex$miuup[i] <- 1.0
    else if (yr < 2120)  ex$miuup[i] <- 1.1
    else if (yr < 2200)  ex$miuup[i] <- 1.05
    else                 ex$miuup[i] <- 1.0
  }
  ex$miuup[1] <- p$miu1  # fix first period
  
  return(ex)
}

cat("Exogenous paths function defined.\
")


# ============================================================
# DICE2023 in R - Part 3: Forward simulation (given MIU and S)
# ============================================================

run_dice <- function(MIU, S, p, ex) {
  T <- p$T
  
  # --- Allocate output vectors ---
  K      <- numeric(T)
  YGROSS <- numeric(T)
  YNET   <- numeric(T)
  Y      <- numeric(T)
  C      <- numeric(T)
  CPC    <- numeric(T)
  I      <- numeric(T)
  DAMFRAC<- numeric(T)
  DAMAGES<- numeric(T)
  ABATECOST <- numeric(T)
  MCABATE   <- numeric(T)
  CPRICE    <- numeric(T)
  ECO2   <- numeric(T)
  EIND   <- numeric(T)
  ECO2E  <- numeric(T)
  CCATOT <- numeric(T)
  
  # FAIR carbon cycle
  RES0   <- numeric(T)
  RES1   <- numeric(T)
  RES2   <- numeric(T)
  RES3   <- numeric(T)
  MAT    <- numeric(T)
  CACC   <- numeric(T)
  alpha  <- numeric(T)
  IRFt   <- numeric(T)
  FORC   <- numeric(T)
  TATM   <- numeric(T)
  TBOX1  <- numeric(T)
  TBOX2  <- numeric(T)
  F_GHGabate <- numeric(T)
  
  PERIODU    <- numeric(T)
  TOTPERIODU <- numeric(T)
  RFACTLONG  <- numeric(T)
  
  # --- Initial conditions ---
  K[1]          <- p$k0
  CCATOT[1]     <- p$CumEmiss0
  MAT[1]        <- p$mat0
  RES0[1]       <- p$res00
  RES1[1]       <- p$res10
  RES2[1]       <- p$res20
  RES3[1]       <- p$res30
  TATM[1]       <- p$tatm0
  TBOX1[1]      <- p$tbox10
  TBOX2[1]      <- p$tbox20
  F_GHGabate[1] <- p$F_GHGabate2020
  RFACTLONG[1]  <- 1000000
  
  # Solve alpha[1] via IRF equation (iterative)
  solve_alpha <- function(cacc_val, tatm_val, p) {
    target <- p$irf0 + p$irC * cacc_val + p$irT * tatm_val
    f <- function(a) {
      a * (p$emshare0 * p$tau0 * (1 - exp(-100 / (a * p$tau0))) +
             p$emshare1 * p$tau1 * (1 - exp(-100 / (a * p$tau1))) +
             p$emshare2 * p$tau2 * (1 - exp(-100 / (a * p$tau2))) +
             p$emshare3 * p$tau3 * (1 - exp(-100 / (a * p$tau3)))) - target
    }
    tryCatch(uniroot(f, c(0.1, 100))$root, error = function(e) 1.0)
  }
  
  for (i in 1:T) {
    # --- Production ---
    YGROSS[i] <- ex$AL[i] * (ex$L[i] / 1000)^(1 - p$gama) * K[i]^p$gama
    DAMFRAC[i] <- p$a1 * TATM[i] + p$a2base * TATM[i]^p$a3
    DAMAGES[i] <- YGROSS[i] * DAMFRAC[i]
    YNET[i]   <- YGROSS[i] * (1 - DAMFRAC[i])
    ABATECOST[i] <- YGROSS[i] * ex$cost1tot[i] * MIU[i]^p$expcost2
    Y[i]      <- YNET[i] - ABATECOST[i]
    
    # Savings: fix S after period 37
    s_i <- if (i > 37) p$optlrsav else S[i]
    I[i]  <- s_i * Y[i]
    C[i]  <- max(Y[i] - I[i], 0.001)
    CPC[i] <- 1000 * C[i] / ex$L[i]
    
    MCABATE[i] <- ex$pbacktime[i] * MIU[i]^(p$expcost2 - 1)
    CPRICE[i]  <- ex$pbacktime[i] * MIU[i]^(p$expcost2 - 1)
    
    # --- Emissions ---
    EIND[i]  <- ex$sigma[i] * YGROSS[i] * (1 - MIU[i])
    ECO2[i]  <- (ex$sigma[i] * YGROSS[i] + ex$eland[i]) * (1 - MIU[i])
    ECO2E[i] <- (ex$sigma[i] * YGROSS[i] + ex$eland[i] + ex$CO2E_GHGabateB[i]) * (1 - MIU[i])
    
    # --- Welfare ---
    PERIODU[i]    <- ((C[i] * 1000 / ex$L[i])^(1 - p$elasmu) - 1) / (1 - p$elasmu) - 1
    TOTPERIODU[i] <- PERIODU[i] * ex$L[i] * ex$rr[i]
    
    # --- FAIR carbon cycle (update for next period) ---
    CACC[i] <- CCATOT[i] - (MAT[i] - p$mateq)
    alpha[i] <- solve_alpha(CACC[i], TATM[i], p)
    IRFt[i]  <- alpha[i] * (
      p$emshare0 * p$tau0 * (1 - exp(-100 / (alpha[i] * p$tau0))) +
        p$emshare1 * p$tau1 * (1 - exp(-100 / (alpha[i] * p$tau1))) +
        p$emshare2 * p$tau2 * (1 - exp(-100 / (alpha[i] * p$tau2))) +
        p$emshare3 * p$tau3 * (1 - exp(-100 / (alpha[i] * p$tau3)))
    )
    FORC[i] <- p$fco22x * (log(MAT[i] / p$mateq) / log(2)) + ex$F_Misc[i] + F_GHGabate[i]
    
    if (i < T) {
      # Capital
      K[i+1] <- max((1 - p$dk)^p$tstep * K[i] + p$tstep * I[i], 1)
      
      # Cumulative emissions
      CCATOT[i+1] <- CCATOT[i] + ECO2[i] * (5 / 3.666)
      
      # FAIR reservoirs
      a_next <- solve_alpha(CACC[i], TATM[i], p)
      RES0[i+1] <- (p$emshare0 * p$tau0 * a_next * (ECO2[i+1] / 3.667)) *
        (1 - exp(-p$tstep / (p$tau0 * a_next))) +
        RES0[i] * exp(-p$tstep / (p$tau0 * a_next))
      RES1[i+1] <- (p$emshare1 * p$tau1 * a_next * (ECO2[i+1] / 3.667)) *
        (1 - exp(-p$tstep / (p$tau1 * a_next))) +
        RES1[i] * exp(-p$tstep / (p$tau1 * a_next))
      RES2[i+1] <- (p$emshare2 * p$tau2 * a_next * (ECO2[i+1] / 3.667)) *
        (1 - exp(-p$tstep / (p$tau2 * a_next))) +
        RES2[i] * exp(-p$tstep / (p$tau2 * a_next))
      RES3[i+1] <- (p$emshare3 * p$tau3 * a_next * (ECO2[i+1] / 3.667)) *
        (1 - exp(-p$tstep / (p$tau3 * a_next))) +
        RES3[i] * exp(-p$tstep / (p$tau3 * a_next))
      MAT[i+1]  <- p$mateq + RES0[i+1] + RES1[i+1] + RES2[i+1] + RES3[i+1]
      MAT[i+1]  <- max(MAT[i+1], 10)
      
      # Temperature
      TBOX1[i+1] <- TBOX1[i] * exp(-p$tstep / p$d1) + p$teq1 * FORC[i+1] * (1 - exp(-p$tstep / p$d1))
      TBOX2[i+1] <- TBOX2[i] * exp(-p$tstep / p$d2) + p$teq2 * FORC[i+1] * (1 - exp(-p$tstep / p$d2))
      TATM[i+1]  <- max(min(TBOX1[i+1] + TBOX2[i+1], 20), 0.5)
      
      # Non-CO2 GHG abateable forcing
      F_GHGabate[i+1] <- p$Fcoef2 * F_GHGabate[i] + p$Fcoef1 * ex$CO2E_GHGabateB[i] * (1 - MIU[i])
    }
  }
  
  UTILITY <- p$tstep * p$scale1 * sum(TOTPERIODU) + p$scale2
  
  list(
    year       = p$yr0 + (1:T - 1) * p$tstep,
    K = K, YGROSS = YGROSS, YNET = YNET, Y = Y,
    C = C, CPC = CPC, I = I,
    DAMFRAC = DAMFRAC, DAMAGES = DAMAGES,
    ABATECOST = ABATECOST, MCABATE = MCABATE, CPRICE = CPRICE,
    ECO2 = ECO2, EIND = EIND, ECO2E = ECO2E, CCATOT = CCATOT,
    MAT = MAT, TATM = TATM, TBOX1 = TBOX1, TBOX2 = TBOX2,
    FORC = FORC, alpha = alpha, IRFt = IRFt, CACC = CACC,
    F_GHGabate = F_GHGabate,
    MIU = MIU, S = S,
    PERIODU = PERIODU, TOTPERIODU = TOTPERIODU,
    UTILITY = UTILITY
  )
}

cat("Forward simulation function defined.\
")


# ============================================================
# DICE2023 in R - Part 4: Optimization of MIU and S
# ============================================================

if (!requireNamespace("nloptr", quietly = TRUE)) {
  install.packages("nloptr")
}
library(nloptr)

optimize_dice <- function(p, ex, verbose = TRUE) {
  T <- p$T
  
  # Decision variables: MIU[2:T] and S[1:37]
  # (MIU[1] is fixed at miu1; S[38:T] fixed at optlrsav)
  n_miu <- T - 1   # MIU periods 2..T
  n_s   <- 37      # S periods 1..37
  n_vars <- n_miu + n_s
  
  # Bounds
  miu_lo <- rep(0.0, n_miu)
  miu_hi <- ex$miuup[2:T]
  s_lo   <- rep(0.1, n_s)
  s_hi   <- rep(0.9, n_s)
  
  lb <- c(miu_lo, s_lo)
  ub <- c(miu_hi, s_hi)
  
  # Starting values
  x0_miu <- pmin(pmax(seq(0.05, 1.0, length.out = n_miu), miu_lo), miu_hi)
  x0_s   <- rep(0.25, n_s)
  x0     <- c(x0_miu, x0_s)
  
  iter_count <- 0
  
  # Objective (negative utility for minimization)
  obj_fn <- function(x) {
    iter_count <<- iter_count + 1
    MIU_full <- c(p$miu1, x[1:n_miu])
    S_full   <- c(x[(n_miu + 1):(n_miu + n_s)], rep(p$optlrsav, T - n_s))
    
    res <- tryCatch(
      run_dice(MIU_full, S_full, p, ex),
      error = function(e) list(UTILITY = -1e10)
    )
    
    if (verbose && iter_count %% 100 == 0) {
      cat(sprintf("  Iter %d | Utility = %.4f\
", iter_count, res$UTILITY))
    }
    
    -res$UTILITY  # minimize negative utility
  }
  
  if (verbose) cat("Starting DICE2023 optimization (BOBYQA)...\
")
  
  result <- bobyqa(
    x0    = x0,
    fn    = obj_fn,
    lower = lb,
    upper = ub,
    control = list(maxeval = 5000, ftol_rel = 1e-6)
  )
  
  MIU_opt <- c(p$miu1, result$par[1:n_miu])
  S_opt   <- c(result$par[(n_miu + 1):(n_miu + n_s)], rep(p$optlrsav, T - n_s))
  
  if (verbose) cat(sprintf("Optimization complete. Max Utility = %.4f\
", -result$value))
  
  list(
    MIU = MIU_opt,
    S   = S_opt,
    utility = -result$value,
    result  = result
  )
}

cat("Optimization function defined.\
")

# ============================================================
# DICE2023 in R - Part 5: run baseline and optimal policy
# ============================================================
library(nloptr)

run_baseline_simulation <- function() {
  p  <- dice2023_params()
  ex <- compute_exogenous(p)
  
  # Simple baseline:
  # - MIU increases linearly from miu1 to 1 by 2100 and stays there
  # - S is constant at 0.25 until period 37, then fixed at optlrsav
  T  <- p$T
  t_idx <- 1:T
  years <- p$yr0 + (t_idx - 1) * p$tstep
  
  MIU <- rep(0, T)
  MIU[1] <- p$miu1
  # ramp up to 1 by 2100, then stay at 1
  for (i in 2:T) {
    if (years[i] <= 2100) {
      MIU[i] <- min(1, p$miu1 + (years[i] - p$yr0) / (2100 - p$yr0) * (1 - p$miu1))
    } else {
      MIU[i] <- 1
    }
    # enforce GAMS-style upper bounds
    MIU[i] <- min(MIU[i], ex$miuup[i])
  }
  
  S <- rep(0.25, T)
  S[38:T] <- p$optlrsav
  
  sim <- run_dice(MIU, S, p, ex)
  sim
}

run_optimal_policy <- function(verbose = TRUE) {
  p  <- dice2023_params()
  ex <- compute_exogenous(p)
  
  opt <- optimize_dice(p, ex, verbose = verbose)
  sim <- run_dice(opt$MIU, opt$S, p, ex)
  
  list(
    params   = p,
    exogenous = ex,
    optimum  = opt,
    simulation = sim
  )
}

cat("Buttons defined: run_baseline_simulation() and run_optimal_policy().\n")


baseline <- run_baseline_simulation()
cat("Baseline utility:", baseline$UTILITY, "\n")

opt_res <- run_optimal_policy(verbose = TRUE)
cat("Optimal utility:", opt_res$simulation$UTILITY, "\n")

# Quick peek at optimal MIU and S for the first 10 periods
print(data.frame(
  year = opt_res$simulation$year[1:10],
  MIU  = round(opt_res$simulation$MIU[1:10], 3),
  S    = round(opt_res$simulation$S[1:10], 3),
  CPC  = round(opt_res$simulation$CPC[1:10], 1),
  TATM = round(opt_res$simulation$TATM[1:10], 3)
))
