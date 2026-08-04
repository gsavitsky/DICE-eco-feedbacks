(* ::Package:: *)

(* ============================================================ *)
(*  LentonSCM.wl                                                  *)
(*  Lenton (2000) Simple Carbon Model — terrestrial + ocean       *)
(*  carbon cycle module for DICE-C / DICE-CP                     *)
(*                                                                *)
(*  Source: STELLA model "Lenton_Carbon" module,                 *)
(*          DICE_2023_5_12.stmx                                  *)
(*                                                                *)
(*  EXTERNAL INTERFACE (supplied by other modules — not solved   *)
(*  here):                                                        *)
(*    TATM[t]   -- atmospheric temperature anomaly (Climate mod.) *)
(*    EIND[t]   -- industrial CO2 emissions (Economy module)      *)
(*    Fmelt[t]  -- permafrost melt carbon flux (Permafrost mod.)  *)
(*                 (STELLA source file has NO equation for this   *)
(*                 flow -- "Permafrost melt" was blank. Set to 0  *)
(*                 for standalone testing; must resolve before    *)
(*                 coupling to Permafrost_dynamics module.)       *)
(*                                                                *)
(*  For standalone validation below, TATM/EIND are stubbed as     *)
(*  simple placeholder functions -- replace with real linkages    *)
(*  once Climate and Economy modules are built.                  *)
(* ============================================================ *)

BeginPackage["LentonSCM`"]

(* ---------- Public symbols ---------- *)
LentonParams::usage = "Association of Lenton SCM parameters (numeric constants).";
LentonInitialConditions::usage = "Association of initial conditions for Lenton SCM state variables, at t=2020.";
LentonAuxEquations::usage = "Association of auxiliary (intermediate) variable definitions as pure functions.";
LentonODESystem::usage = "List of ODEs (as equalities suitable for NDSolve) for the Lenton SCM states.";
LentonStandaloneSolve::usage = "Runs a standalone NDSolve of the Lenton SCM using placeholder TATM/EIND inputs, for validation only.";

(* State variable symbols must be declared PUBLIC here (before Begin["`Private`"])
   or they get silently created inside the Private context instead, meaning
   Ca[t] typed in your own notebook refers to a different, undefined symbol
   than the Ca used internally by this package -- this was the cause of the
   blank plot / unevaluated Ca[2020] issue. *)
Ca::usage = "Atmospheric Carbon state variable, PgC.";
Cv::usage = "Vegetation Carbon state variable, PgC.";
Cs::usage = "Soil Carbon state variable, PgC.";
Col::usage = "Low Latitude Ocean Carbon state variable, PgC.";
Coh::usage = "High Latitude Surface Ocean Carbon state variable, PgC.";
Coi::usage = "Intermediate Ocean Carbon state variable, PgC.";
Cod::usage = "Deep Ocean Carbon state variable, PgC.";
Ldef::usage = "Cumulative deforestation state variable, PgC.";

Begin["`Private`"]

(* ============================================================ *)
(* 1. PARAMETERS                                                *)
(* ============================================================ *)

LentonParams = <|
  "kp"    -> 0.184,        (* photosynthesis rate, 1/yr *)
  "kr"    -> 0.092,        (* plant respiration rate, 1/yr *)
  "kt"    -> 0.092,        (* turnover rate, 1/yr *)
  "ksr"   -> 0.0337,       (* soil respiration rate, 1/yr *)
  "KM"    -> 120,          (* half-saturation point, ppmv *)
  "Ea"    -> 54830,        (* plant respiration activation energy, J/mol *)
  "kMM"   -> 1.478,        (* photosynthesis normalizing constant *)
  "kA"    -> 8.7039*^9,    (* plant respiration normalizing constant *)
  "kT"    -> 6.5*^7,       (* thermohaline overturning, m^3/s *)
  "kU"    -> 4.87*^7,      (* high latitude overturning, m^3/s *)
  "kwi"   -> 1.25*^7,      (* warm-intermediate exchange, m^3/s *)
  "kid"   -> 2*^8,         (* intermediate-deep exchange, m^3/s *)
  "ks"    -> 1752,         (* piston velocity, m/yr *)
  "kc"    -> 29,           (* compensation point, ppmv *)
  "kB"    -> 157.072,      (* soil respiration normalizing constant *)
  "kD"    -> 0.27,         (* deforestation vegetation-loss factor *)
  "R"     -> 8.314,        (* gas constant, J/(mol K) *)
  "Th"    -> 275.15,       (* high-latitude reference temp, K *)
  "Tl"    -> 295.15,       (* low-latitude reference temp, K *)
  "Al"    -> 0.85,         (* fractional area, low latitude *)
  "Ah"    -> 0.15,         (* fractional area, high latitude *)
  "aO"    -> 3.6*^14,      (* ocean surface area, m^2 *)
  "beta"  -> 11,           (* Revelle factor *)
  "pCO2w0" -> 286*^-6,     (* reference low-lat surface water pCO2 *)
  "pCO2c0" -> 270*^-6,     (* reference high-lat surface water pCO2 *)
  "rho"   -> 1027,         (* seawater density, kg/m^3 -- NB: STELLA units field says kg/m^2, presumed typo for kg/m^3 *)
  "Vl"    -> 3.06*^16,     (* low-latitude ocean volume, m^3 *)
  "Vh"    -> 5.4*^15,      (* high-latitude ocean volume, m^3 *)
  "Vi"    -> 3.6*^17,      (* intermediate ocean volume, m^3 *)
  "Vd"    -> 9.7*^17,      (* deep ocean volume, m^3 *)
  "eland0" -> 2.6,         (* land-use CO2 emissions in 2015, GtCO2/yr *)
  "deland" -> 0.022009596, (* decline rate of land emissions, 1/yr *)
  "ka"    -> 1.773*^20,    (* mole volume of atmosphere, mol *)
  "fgtm"  -> 8.3259*^13,   (* pre-industrial moles of atmosphere, mol/PgC *)
  (* switches -- keep as 1 unless doing a structural sensitivity run *)
  "OceanCO2switch"     -> 1,
  "TerrCO2switch"      -> 1,
  "PhotoTempSwitch"    -> 1,
  "TempRespSwitchSoil" -> 1,
  "TempRespSwitchVeg"  -> 1
|>;

(* ============================================================ *)
(* 2. INITIAL CONDITIONS  (t = 2020)                             *)
(* ============================================================ *)

LentonInitialConditions = <|
  "Ca0"   -> 887,    (* Atmospheric Carbon, PgC *)
  "Cv0"   -> 550,    (* Vegetation Carbon, PgC *)
  "Cs0"   -> 1500,   (* Soil Carbon, PgC *)
  "Col0"  -> 730,    (* Low-latitude ocean surface carbon, PgC *)
  "Coh0"  -> 140,    (* High-latitude ocean surface carbon, PgC *)
  "Coi0"  -> 10040,  (* Intermediate ocean carbon, PgC *)
  "Cod0"  -> 23860,  (* Deep ocean carbon, PgC *)
  "Ldef0" -> 0       (* Cumulative deforestation, PgC *)
|>;

(* ============================================================ *)
(* 3. EXTERNAL INPUTS (placeholders for standalone testing --   *)
(*    replace with real module linkages when coupling)          *)
(* ============================================================ *)

(* TATM: atmospheric temperature anomaly from Climate module.
   Placeholder: linear ramp roughly matching DICE 2023 baseline
   trajectory, purely for standalone sanity-checking. *)
TATMplaceholder[t_] := 1.1 + 0.012*(t - 2020);

(* EIND: industrial CO2 emissions from Economy module (function of
   MIU, output, sigma -- see Economy subfile). Placeholder: flat
   emissions at 2020 industrial level, GtCO2/yr, converted inside
   GHG emissions flow via *0.2727 factor as in STELLA source. *)
EINDplaceholder[t_] := 37.56;

(* Fmelt: permafrost melt carbon flux from Permafrost_dynamics
   module. STELLA source file has NO equation wired to this flow
   (blank in .stmx) -- flagged for Greta to confirm against her
   STELLA source directly. Zero here is NOT a physical assumption,
   just a standalone-testing stub. *)
Fmeltplaceholder[t_] := 0;

(* ============================================================ *)
(* 4. AUXILIARY (INTERMEDIATE) EQUATIONS                        *)
(*    Written as functions of the state variables + external    *)
(*    inputs, evaluated at time t.                               *)
(* ============================================================ *)

With[{p = LentonParams},

  (* vegetation carbon net of deforestation losses *)
  CvsFn[Cv0_, Ldef_] := Cv0 - Ldef*p["kD"];

  (* partial pressure of CO2 in atmosphere, mole fraction *)
  pCO2Fn[Ca_] := Ca/p["ka"]*p["fgtm"];

  (* atmospheric CO2 in ppmv, subject to Terr CO2 switch *)
  pCO2ppmvFn[Ca_] := If[p["TerrCO2switch"] == 1, pCO2Fn[Ca]*10^6, 0.0004165];

  (* surface temperature, K -- NOTE: uses TATM directly, NOT   *)
  (* gated by PhotoTempSwitch (that gating only applies to the  *)
  (* "anomaly" term used in Photosynthesis, see below)          *)
  surfaceTempFn[TATM_] := 288 + TATM;

  (* "anomaly" term used only in the Photosynthesis flow --     *)
  (* gated by PhotoTempSwitch per STELLA source                 *)
  anomalyFn[TATM_] := If[p["PhotoTempSwitch"] == 1, TATM, 0];

  (* low/high latitude surface-water pCO2 (Revelle-scaled) *)
  pCO2wFn[Col_] := p["pCO2w0"]*(Col/LentonInitialConditions["Col0"])^p["beta"];
  pCO2cFn[Coh_] := p["pCO2c0"]*(Coh/LentonInitialConditions["Coh0"])^p["beta"];

  (* solubility coefficients (Weiss-type fit), low/high latitude *)
  bwFn[] := Exp[-60.2409 + 93.4517*(100/p["Tl"]) + 23.3585*Log[p["Tl"]/100]
                + 34.7*(0.023517 - 0.023656*(p["Tl"]/100) + 0.0047036*(p["Tl"]/100)^2)];
  bcFn[] := Exp[-60.2409 + 93.4517*(100/p["Th"]) + 23.3585*Log[p["Th"]/100]
                + 34.7*(0.023517 - 0.023656*(p["Th"]/100) + 0.0047036*(p["Th"]/100)^2)];

  (* air-sea CO2 exchange fluxes, low/high latitude, PgC/yr *)
  FawFn[Ca_, Col_] := If[p["OceanCO2switch"] == 1,
     p["ks"]*p["aO"]*p["Al"]*bwFn[]*(pCO2Fn[Ca] - pCO2wFn[Col])*12*^-15*p["rho"],
     p["ks"]*p["aO"]*p["Al"]*bwFn[]*(0.0004165 - pCO2wFn[Col])*12*^-15*p["rho"]];
  FacFn[Ca_, Coh_] := If[p["OceanCO2switch"] == 1,
     p["ks"]*p["aO"]*p["Ah"]*bcFn[]*(pCO2Fn[Ca] - pCO2cFn[Coh])*12*^-15*p["rho"],
     p["ks"]*p["aO"]*p["Ah"]*bcFn[]*(0.0004165 - pCO2cFn[Coh])*12*^-15*p["rho"]];

  (* land-use emissions, GtCO2/yr, exogenous decay from 2015 *)
  elandFn[t_] := p["eland0"]*(1 - p["deland"])^(t - 2015);

  LentonAuxEquations = <|
    "Cvs"        -> CvsFn,
    "pCO2"       -> pCO2Fn,
    "pCO2ppmv"   -> pCO2ppmvFn,
    "surfaceTemp"-> surfaceTempFn,
    "anomaly"    -> anomalyFn,
    "pCO2w"      -> pCO2wFn,
    "pCO2c"      -> pCO2cFn,
    "bw"         -> bwFn,
    "bc"         -> bcFn,
    "Faw"        -> FawFn,
    "Fac"        -> FacFn,
    "eland"      -> elandFn
  |>;
];

(* ============================================================ *)
(* 5. FLOW EQUATIONS + ODE SYSTEM                                *)
(*    State functions: Ca[t], Cv[t], Cs[t], Col[t], Coh[t],      *)
(*                      Coi[t], Cod[t], Ldef[t]                  *)
(* ============================================================ *)

BuildLentonODESystem[TATMfn_, EINDfn_, Fmeltfn_] := With[{p = LentonParams},
  Module[{Photosynthesis, PlantResp, Turnover, SoilResp, AirSeaLL, AirSeaHL,
          Deforestation, WarmInt, IntDeep, ColdInt, DeepInt, IntWarm, GHGemis, eqs},

    (* --- flows, each a function of state values at time t --- *)

    Photosynthesis[Ca_, Cv_, Ldef_, TATM_] :=
      p["kp"]*CvsFn[Cv, Ldef]*p["kMM"] *
      ((pCO2ppmvFn[Ca] - p["kc"])/(p["KM"] + (pCO2ppmvFn[Ca] - p["kc"]))) *
      ((15 + anomalyFn[TATM])^2 * (25 - anomalyFn[TATM]) / 5625);

    PlantResp[Cv_, TATM_] := If[p["TempRespSwitchVeg"] == 1,
      p["kr"]*Cv*p["kA"]*Exp[-p["Ea"]/(p["R"]*surfaceTempFn[TATM])],
      p["kr"]*Cv*p["kA"]*Exp[-p["Ea"]/(p["R"]*288)]];

    Turnover[Cv_] := p["kt"]*Cv;

    SoilResp[Cs_, TATM_] := If[p["TempRespSwitchSoil"] == 1,
      p["ksr"]*Cs*p["kB"]*2.7^(-1*(308.56/(surfaceTempFn[TATM] - 227.13))),
      p["ksr"]*Cs*p["kB"]*2.7^(-1*(308.56/(288 - 227.13)))];

    AirSeaLL[Ca_, Col_] := -FawFn[Ca, Col];
    AirSeaHL[Ca_, Coh_] := -FacFn[Ca, Coh];

    Deforestation[t_] := elandFn[t]/2.12;

    WarmInt[Col_]  := p["kwi"]*(Col/p["Vl"])*3.15576*^7*12*^-15;
    IntDeep[Coi_]  := p["kid"]*(Coi/p["Vi"])*3.15576*^7*12*^-15;
    ColdInt[Coh_]  := p["kU"]*(Coh/p["Vh"])*3.15576*^7*12*^-15;
    DeepInt[Cod_]  := p["kid"]*(Cod/p["Vd"])*3.15576*^7*12*^-15;
    IntWarm[Coi_]  := (p["kT"] + p["kwi"])*(Coi/p["Vi"])*3.15576*^7*12*^-15;

    GHGemis[t_] := EINDfn[t]*0.2727;

    (* --- ODE system --- *)
    eqs = {
      Ca'[t] == PlantResp[Cv[t], TATMfn[t]] + SoilResp[Cs[t], TATMfn[t]]
                + AirSeaLL[Ca[t], Col[t]] + AirSeaHL[Ca[t], Coh[t]]
                + GHGemis[t] + Fmeltfn[t] - Photosynthesis[Ca[t], Cv[t], Ldef[t], TATMfn[t]],

      Cv'[t] == Photosynthesis[Ca[t], Cv[t], Ldef[t], TATMfn[t]]
                - PlantResp[Cv[t], TATMfn[t]] - Turnover[Cv[t]],

      Cs'[t] == Turnover[Cv[t]] - SoilResp[Cs[t], TATMfn[t]],

      (* Col: inflow = intermediate-warm exch; outflows = Air-Sea(LL) flow (-Faw) and Warm-intermediate exch.
         dCol/dt = IntWarm[Coi] - (-Faw) - WarmInt[Col] = IntWarm[Coi] + Faw - WarmInt[Col] *)
      Col'[t] == IntWarm[Coi[t]] + FawFn[Ca[t], Col[t]] - WarmInt[Col[t]],

      (* Coh: no inflows; outflows = Air-Sea(HL) flow (-Fac) and cold-intermediate exch.
         dCoh/dt = -(-Fac) - ColdInt[Coh] = Fac - ColdInt[Coh] *)
      Coh'[t] == FacFn[Ca[t], Coh[t]] - ColdInt[Coh[t]],

      Coi'[t] == WarmInt[Col[t]] + ColdInt[Coh[t]] + DeepInt[Cod[t]]
                 - IntDeep[Coi[t]] - IntWarm[Coi[t]],

      Cod'[t] == IntDeep[Coi[t]] - DeepInt[Cod[t]],

      Ldef'[t] == Deforestation[t]
    };
    eqs
  ]
];

(* ============================================================ *)
(* 6. STANDALONE VALIDATION SOLVE                                *)
(*    Uses placeholder TATM/EIND/Fmelt -- for structural/         *)
(*    numerical sanity-checking of this module ONLY. Not a       *)
(*    substitute for full coupled-model validation.              *)
(* ============================================================ *)

LentonStandaloneSolve[tmax_: 2120] := Module[{eqs, ics, sol, ic},
  ic = LentonInitialConditions;
  eqs = BuildLentonODESystem[TATMplaceholder, EINDplaceholder, Fmeltplaceholder];
  ics = {
    Ca[2020] == ic["Ca0"], Cv[2020] == ic["Cv0"], Cs[2020] == ic["Cs0"],
    Col[2020] == ic["Col0"], Coh[2020] == ic["Coh0"], Coi[2020] == ic["Coi0"],
    Cod[2020] == ic["Cod0"], Ldef[2020] == ic["Ldef0"]
  };
  sol = NDSolve[Join[eqs, ics],
    {Ca, Cv, Cs, Col, Coh, Coi, Cod, Ldef},
    {t, 2020, tmax},
    Method -> {"ExplicitRungeKutta", "DifferenceOrder" -> 4},
    MaxStepSize -> 0.1,
    StartingStepSize -> 0.1
  ];
  sol
];

End[]
EndPackage[]
