# calculate the sensitivity and specificity for a specific genetic distance threshold of 2 mutations
sens_spec_calc(cutoff=2, 
               mut_rate=1, 
               mean_gens_pdf=c(0.02,0.08,0.15,0.75), 
               max_link_gens=1)

# calculate the sensitivity and specificity for a a range of genetic distance thresholds
sens_spec_calc(cutoff=1:10, 
               mut_rate=1, 
               mean_gens_pdf=c(0.02,0.08,0.15,0.75), 
               max_link_gens=1)