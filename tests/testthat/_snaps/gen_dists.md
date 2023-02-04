# gen_dists is deprecated

    Code
      expect_equal(gen_dists(mut_rate = 1, mean_gens_pdf = c(0.2, 0.2, 0.2, 0.2, 0.2),
      max_dist = 0)[, "linked_prob"][[1]], 1)
    Warning <lifecycle_warning_deprecated>
      `gen_dists()` was deprecated in phylosamp 1.0.0.
      i Please use `gendist_distribution()` instead.

