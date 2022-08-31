
test_that("returns M*rho or M/2 * rho when sensitivity perfect", {
  
  expect_equal(
    true_pairs(eta=1, 
              rho=1, 
              M=10,
              R=NULL,
              assumption='stsl'), 
    5
  )
  
  expect_equal(
    true_pairs(eta=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='mtsl'), 
    10
  )
  
  expect_equal(
    true_pairs(eta=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='mtml'), 
    10
  )
})

test_that("fails when parameters invalid", {
  
  expect_error(
    true_pairs(eta=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='bananas')
  )
  
})