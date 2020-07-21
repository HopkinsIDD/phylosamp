# ebola-like pathogen
R <- 1.5
mut_rate <- 1

# use simulated generation distributions
data("gen_dist_sim")
mean_gens_pdf <- as.numeric(gen_dist_sim[gen_dist_sim$R == R, -(1:2)])

# get theoretical genetic distance dist based on mutation rate and generation parameters
dists <- as.data.frame(gen_dists(mut_rate = mut_rate, 
                                 mean_gens_pdf = mean_gens_pdf, 
                                 max_link_gens = 1))

dists <- reshape2::melt(dists, 
                        id.vars = "dist", 
                        variable.name = "status", 
                        value.name = "prob")

# get sensitivity and specificity using the same paramters
roc_calc <- sens_spec_roc(cutoff = 1:(max(dists$dist)-1), 
                          mut_rate = mut_rate,
                          mean_gens_pdf = mean_gens_pdf)