# The simplest case: single-transmission, single-linkage, and perfect sensitivity
exp_links(eta=1, chi=0.9, rho=0.5, M=100, assumption='stsl')

# Multiple-transmission and imperfect sensitivity
exp_links(eta=0.99, chi=0.9, rho=1, M=50, R=1, assumption='mtsl')

# Small outbreak, larger sampling proportion
exp_links(eta=0.99, chi=0.95, rho=1, M=50, R=1.5, assumption='mtml')

# Large outbreak, small sampling proportion
exp_links(eta=0.99, chi=0.95, rho=0.05, M=1000, R=1.5, assumption='mtml')
