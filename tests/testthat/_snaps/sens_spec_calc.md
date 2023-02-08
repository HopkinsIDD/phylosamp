# sens_spec_calc is deprecated

    Code
      expect_equal(sens_spec_calc(cutoff = 2, mut_rate = 1, mean_gens_pdf = c(0.02,
        0.08, 0.15, 0.75), max_link_gens = 1)[1, "sensitivity"][[1]], 0.7357589,
      tolerance = 0.01)
    Warning <lifecycle_warning_deprecated>
      `sens_spec_calc()` was deprecated in phylosamp 1.0.0.
      i Please use `gendist_sensspec_cutoff()` instead.
      `gen_dists()` was deprecated in phylosamp 1.0.0.
      i Please use `gendist_distribution()` instead.

