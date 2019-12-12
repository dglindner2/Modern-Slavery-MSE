precompute_bootstrap = src/bootstrap/precompute_bootstrap_simulations.R

output/bootstrap/bootstrap_ests_UKdat_4.rds output/bootstrap/bootstrap_ests_UKdat_5.rds output/bootstrap/bootstrap_ests_UKdat_6.rds: $(precompute_bootstrap)
	mkdir -p output/bootstrap; Rscript $<
	
output/bootstrap/bootstrap_ests_UKdat_5.rds: $(precompute_bootstrap)
	mkdir -p output/bootstrap; Rscript $<
	
output/bootstrap/bootstrap_ests_UKdat_6.rds: $(precompute_bootstrap)
	mkdir -p output/bootstrap; Rscript $<
