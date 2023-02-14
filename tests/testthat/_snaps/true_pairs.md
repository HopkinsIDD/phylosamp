# true_pairs is deprecated

    Code
      expect_equal(true_pairs(eta = 1, rho = 1, M = 10, R = NULL, assumption = "stsl"),
      5)
    Warning <lifecycle_warning_deprecated>
      `true_pairs()` was deprecated in phylosamp 1.0.0.
      i Please use `translink_expected_links_true()` instead.
      The `eta` argument of `true_pairs()` is deprecated as of phylosamp 1.0.0.
      i Please use the `sensitivity` argument of `translink_expected_links_true()` instead.
    Message <simpleMessage>
      Calculating expected number of links assuming single-transmission and single-linkage
    Warning <lifecycle_warning_deprecated>
      `true_pairs_stsl()` was deprecated in phylosamp 1.0.0.
      i Please use `translink_expected_links_true_stsl()` instead.
      The `eta` argument of `true_pairs_stsl()` is deprecated as of phylosamp 1.0.0.
      i Please use the `sensitivity` argument of `translink_expected_links_true_stsl()` instead.

