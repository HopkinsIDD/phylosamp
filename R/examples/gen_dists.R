# ebola-like pathogen
R <- 1.5
mut_rate <- 1

# use simulated generation distributions from the provided 'gen_dist_sim' data object
data("gen_dist_sim")
mean_gens_pdf <- as.numeric(gen_dist_sim[gen_dist_sim$R == R, -(1:2)])

# get theoretical genetic distance dist based on mutation rate and generation parameters
gen_dists(mut_rate = mut_rate, 
          mean_gens_pdf = mean_gens_pdf, 
          max_link_gens = 1)