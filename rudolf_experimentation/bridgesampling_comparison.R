# Lukas Cincikas 2021
# stanfunc.R library written by Rudolf Cardinal
# Used to make model fit comparisons using bridge sampling
# Note: large numbers of models take significant amounts of time to load; more efficient to comment out unnecessary model loading fort any particular comparison

source("https://egret.psychol.cam.ac.uk/rlib/stanfunc.R")

setwd("C:/Users/Lukas Cincikas/Documents/R/MPhil project files/rudolf_experimentation/fitcache")

#fit_M12 <- readRDS("fit_real_data_M12.rds")
#fit_M12_extended_alpha <- readRDS("fit_real_data_M12_extended_alpha.rds")
#fit_M12_exponential <- readRDS("fit_real_data_M12_exponential.rds")
#fit_M12_hyperbolic <- readRDS("fit_real_data_M12_hyperbolic.rds")
#fit_M12_alt <- readRDS("fit_real_data_M12_simplified_colour_choice.rds")
#fit_risk_adjustment_1A <- readRDS("fit_real_data_M12_risk_adjustment_1A.rds")
#fit_risk_adjustment_1B <- readRDS("fit_real_data_M12_risk_adjustment_1B.rds")
#fit_risk_adjustment_2A <- readRDS("fit_real_data_M12_risk_adjustment_2A.rds")
#fit_risk_adjustment_3A <- readRDS("fit_real_data_M12_risk_adjustment_3A.rds")
#fit_risk_adjustment_4 <- readRDS("fit_real_data_M12_risk_adjustment_4.rds")
#fit_risk_adjustment_5 <- readRDS("fit_real_data_M12_risk_adjustment_5.rds")
#fit_loss_chasing_1 <- readRDS("fit_real_data_M12_loss_chasing_1.rds")
#fit_loss_chasing_2 <- readRDS("fit_real_data_M12_loss_chasing_2.rds")
#fit_loss_chasing_3 <- readRDS("fit_real_data_M12_loss_chasing_3.rds")
#fit_loss_chasing_3A <- readRDS("fit_real_data_M12_loss_chasing_3A.rds")
#fit_loss_chasing_3B <- readRDS("fit_real_data_M12_loss_chasing_3B.rds")
#fit_loss_chasing_3C <- readRDS("fit_real_data_M12_loss_chasing_3C.rds")
#fit_loss_chasing_3D <- readRDS("fit_real_data_M12_loss_chasing_3D.rds")
#fit_loss_chasing_3E <- readRDS("fit_real_data_M12_loss_chasing_3E.rds")
#fit_combined <- readRDS("fit_real_data_M12_combined.rds")
#fit_combined_2 <- readRDS("fit_real_data_M12_combined_2.rds")
#fit_WSLS_1 <- readRDS("fit_real_data_M12_WSLS_1.rds")
#fit_WSLS_2 <- readRDS("fit_real_data_M12_WSLS_2.rds")

#bridge_fit_M12 <- readRDS("bridge_real_data_M12.rds")
#bridge_fit_M12_extended_alpha <- readRDS("bridge_real_data_M12_extended_alpha.rds")
#bridge_fit_M12_exponential <- readRDS("bridge_real_data_M12_exponential.rds")
#bridge_fit_M12_hyperbolic <- readRDS("bridge_real_data_M12_hyperbolic.rds")
#bridge_fit_M12_alt <- readRDS("bridge_real_data_M12_simplified_colour_choice.rds")
#bridge_fit_risk_adjustment_1A <- readRDS("bridge_real_data_M12_risk_adjustment_1A.rds")
#bridge_fit_risk_adjustment_1B <- readRDS("bridge_real_data_M12_risk_adjustment_1B.rds")
#bridge_fit_risk_adjustment_2A <- readRDS("bridge_real_data_M12_risk_adjustment_2A.rds")
#bridge_fit_risk_adjustment_3A <- readRDS("bridge_real_data_M12_risk_adjustment_3A.rds")
#bridge_fit_risk_adjustment_4 <- readRDS("bridge_real_data_M12_risk_adjustment_4.rds")
#bridge_fit_risk_adjustment_5 <- readRDS("bridge_real_data_M12_risk_adjustment_5.rds")
#bridge_fit_loss_chasing_1 <- readRDS("bridge_real_data_M12_loss_chasing_1.rds")
#bridge_fit_loss_chasing_2 <- readRDS("bridge_real_data_M12_loss_chasing_2.rds")
#bridge_fit_loss_chasing_3 <- readRDS("bridge_real_data_M12_loss_chasing_3.rds")
#bridge_fit_loss_chasing_3A <- readRDS("bridge_real_data_M12_loss_chasing_3A.rds")
#bridge_fit_loss_chasing_3B <- readRDS("bridge_real_data_M12_loss_chasing_3B.rds")
#bridge_fit_loss_chasing_3C <- readRDS("bridge_real_data_M12_loss_chasing_3C.rds")
#bridge_fit_loss_chasing_3D <- readRDS("bridge_real_data_M12_loss_chasing_3D.rds")
#bridge_fit_loss_chasing_3E <- readRDS("bridge_real_data_M12_loss_chasing_3E.rds")
#bridge_fit_combined <- readRDS("bridge_real_data_M12_combined.rds")
#bridge_fit_combined_2 <- readRDS("bridge_real_data_M12_combined_2.rds")
#bridge_fit_WSLS_1 <- readRDS("bridge_real_data_M12_WSLS_1.rds")
#bridge_fit_WSLS_2 <- readRDS("bridge_real_data_M12_WSLS_2.rds")


if (FALSE) {
model_comparison <<- stanfunc$compare_model_evidence(
  list(
    list(
      name="Model 12",
      bridgesample=bridge_fit_M12,
      stanfit=fit_M12
    ),
    list(
      name="Model 12 with extended alpha",
      bridgesample=bridge_fit_M12_extended_alpha,
      stanfit=fit_M12_extended_alpha
    ),
    list(
      name="Model 12 exponential delay aversion",
      bridgesample=bridge_fit_M12_exponential,
      stanfit=fit_M12_exponential
    ),
    list(
      name="Model 12 hyperbolic delay aversion",
      bridgesample=bridge_fit_M12_hyperbolic,
      stanfit=fit_M12_hyperbolic
    ),
    list(
      name="Model 12 risk adjustment 1A",
      bridgesample=bridge_fit_risk_adjustment_1A,
      stanfit=fit_risk_adjustment_1A
    ),
    list(
      name="Model 12 risk adjustment 1B",
      bridgesample=bridge_fit_risk_adjustment_1B,
      stanfit=fit_risk_adjustment_1B
    ),
    list(
      name="Model 12 risk adjustment 2A",
      bridgesample=bridge_fit_risk_adjustment_2A,
      stanfit=fit_risk_adjustment_2A
    ),
    list(
      name="Model 12 risk adjustment 3A",
      bridgesample=bridge_fit_risk_adjustment_3A,
      stanfit=fit_risk_adjustment_3A
    ),
    list(
      name="Model 12 risk adjustment 4",
      bridgesample=bridge_fit_risk_adjustment_4,
      stanfit=fit_risk_adjustment_4
    ),
    list(
      name="Model 12 risk adjustment 5",
      bridgesample=bridge_fit_risk_adjustment_5,
      stanfit=fit_risk_adjustment_5
    ),
    list(
      name="Model 12 loss chasing 1",
      bridgesample=bridge_fit_loss_chasing_1,
      stanfit=fit_loss_chasing_1
    ),
    list(
      name="Model 12 loss chasing 2",
      bridgesample=bridge_fit_loss_chasing_2,
      stanfit=fit_loss_chasing_2
    ),
    list(
      name="Model 12 loss chasing 3",
      bridgesample=bridge_fit_loss_chasing_3,
      stanfit=fit_loss_chasing_3
    ),
    list(
      name="Model 12 loss chasing 3A",
      bridgesample=bridge_fit_loss_chasing_3A,
      stanfit=fit_loss_chasing_3A
    ),
    list(
      name="Model 12 loss chasing 3B",
      bridgesample=bridge_fit_loss_chasing_3B,
      stanfit=fit_loss_chasing_3B
    ),
    list(
      name="Model 12 loss chasing 3C",
      bridgesample=bridge_fit_loss_chasing_3C,
      stanfit=fit_loss_chasing_3C
    ),
    list(
      name="Model 12 loss chasing 3D",
      bridgesample=bridge_fit_loss_chasing_3D,
      stanfit=fit_loss_chasing_3D
    ),
    list(
      name="Model 12 loss chasing 3E",
      bridgesample=bridge_fit_loss_chasing_3E,
      stanfit=fit_loss_chasing_3E
    ),
    list(
      name="Model 12 combined",
      bridgesample=bridge_fit_combined,
      stanfit=fit_combined
    ),
    list(
      name="Model 12 combined 2",
      bridgesample=bridge_fit_combined_2,
      stanfit=fit_combined_2
    ),
    list(
      name="Model 12 WSLS 1",
      bridgesample=bridge_fit_WSLS_1,
      stanfit=fit_WSLS_1
    ),
    list(
      name="Model 12 WSLS 2",
      bridgesample=bridge_fit_WSLS_2,
      stanfit=fit_WSLS_2
    )
  ),
  detail=TRUE
)
}

fit_M12_nal <- readRDS("fit_real_data_naltrexone_M12.rds")
fit_M11_nal <- readRDS("fit_real_data_naltrexone_M11.rds")
fit_M10_nal <- readRDS("fit_real_data_naltrexone_M10.rds")
fit_M9_nal  <- readRDS("fit_real_data_naltrexone_M9.rds")
fit_M4_nal  <- readRDS("fit_real_data_naltrexone_M4.rds")
fit_M3_nal  <- readRDS("fit_real_data_naltrexone_M3.rds")
fit_M12_combined_nal <- readRDS("fit_real_data_naltrexone_M12_combined.rds")
#fit_M12_combined_2_nal <- readRDS("fit_real_data_naltrexone_M12_combined_2.rds")
fit_M12_loss_chasing_3C_nal <- readRDS("fit_real_data_naltrexone_M12_loss_chasing_3C.rds")
fit_M12_risk_adjustment_4_nal <- readRDS("fit_real_data_naltrexone_M12_risk_adjustment_4.rds")
#fit_M12_WSLS_1_nal <- readRDS("fit_real_data_naltrexone_M12_WSLS_1.rds")

bridge_fit_M12_nal <- readRDS("bridge_real_data_naltrexone_M12.rds")
bridge_fit_M11_nal <- readRDS("bridge_real_data_naltrexone_M11.rds")
bridge_fit_M10_nal <- readRDS("bridge_real_data_naltrexone_M10.rds")
bridge_fit_M9_nal  <- readRDS("bridge_real_data_naltrexone_M9.rds")
bridge_fit_M4_nal  <- readRDS("bridge_real_data_naltrexone_M4.rds")
bridge_fit_M3_nal  <- readRDS("bridge_real_data_naltrexone_M3.rds")
bridge_fit_M12_combined_nal <- readRDS("bridge_real_data_naltrexone_M12_combined.rds")
#bridge_fit_M12_combined_2_nal <- readRDS("bridge_real_data_naltrexone_M12_combined_2.rds")
bridge_fit_M12_loss_chasing_3C_nal <- readRDS("bridge_real_data_naltrexone_M12_loss_chasing_3C.rds")
bridge_fit_M12_risk_adjustment_4_nal <- readRDS("bridge_real_data_naltrexone_M12_risk_adjustment_4.rds")
#bridge_fit_M12_WSLS_1_nal <- readRDS("bridge_real_data_naltrexone_M12_WSLS_1.rds")

  model_comparison_nal <<- stanfunc$compare_model_evidence(
    list(
      list(
        name="Model 12",
        bridgesample=bridge_fit_M12_nal,
        stanfit=fit_M12_nal
      ),
      list(
        name="Combined",
        bridgesample=bridge_fit_M12_combined_nal,
        stanfit=fit_M12_combined_nal
      ),
      list(
        name="Previous outcome variant 5",
        bridgesample=bridge_fit_M12_loss_chasing_3C_nal,
        stanfit=fit_M12_loss_chasing_3C_nal
      ),
      list(
        name="Odds sensitivity variant 4",
        bridgesample=bridge_fit_M12_risk_adjustment_4_nal,
        stanfit=fit_M12_risk_adjustment_4_nal
      )
    ),
    detail=TRUE
  )


if (FALSE) {
fit_M3 <- readRDS("fit_real_data_M3.rds")
fit_M4 <- readRDS("fit_real_data_M4.rds")
fit_M9 <- readRDS("fit_real_data_M9.rds")
fit_M10 <- readRDS("fit_real_data_M10.rds")
fit_M11 <- readRDS("fit_real_data_M11.rds")
fit_M12 <- readRDS("fit_real_data_M12.rds")

bridge_fit_M3 <- readRDS("bridge_real_data_M3.rds")
bridge_fit_M4 <- readRDS("bridge_real_data_M4.rds")
bridge_fit_M9 <- readRDS("bridge_real_data_M9.rds")
bridge_fit_M10 <- readRDS("bridge_real_data_M10.rds")
bridge_fit_M11 <- readRDS("bridge_real_data_M11.rds")
bridge_fit_M12 <- readRDS("bridge_real_data_M12.rds")

model_comparison_simple <<- stanfunc$compare_model_evidence(
  list(
    list(
      name="Model 03",
      bridgesample=bridge_fit_M3,
      stanfit=fit_M3
    ),
    list(
      name="Model 04",
      bridgesample=bridge_fit_M4,
      stanfit=fit_M4
    ),
    list(
      name="Model 09",
      bridgesample=bridge_fit_M9,
      stanfit=fit_M9
    ),
    list(
      name="Model 10",
      bridgesample=bridge_fit_M10,
      stanfit=fit_M10
    ),
    list(
      name="Model 11",
      bridgesample=bridge_fit_M11,
      stanfit=fit_M11
    ),
    list(
      name="Model 12",
      bridgesample=bridge_fit_M12,
      stanfit=fit_M12
    )
  ),
  detail=TRUE
)
}

#print(model_comparison_simple)
#print(model_comparison)
print(model_comparison_nal)