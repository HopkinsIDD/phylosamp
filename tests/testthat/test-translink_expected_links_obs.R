
test_that("returns M*rho or M/2 * rho when sensitivity perfect", {
  
  expect_equal(
    exp_links(eta=1, 
              chi=1, 
              rho=1, 
              M=10,
              R=NULL,
              assumption='stsl'), 
    5
  )
  
  expect_equal(
    exp_links(eta=1, 
              chi=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='mtsl'), 
    10
  )
  
  expect_equal(
    exp_links(eta=1, 
              chi=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='mtml'), 
    10
  )
})

test_that("fails when parameters invalid", {
  
  expect_error(
    exp_links(eta=1, 
              chi=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='bananas')
  )
  
})