set_working_directory <- function() {
  # This only works if the script is run using source()
  current_path <- dirname(normalizePath(sys.frame(1)$ofile))
  setwd(current_path)
}

# Call the function to set the working directory dynamically
set_working_directory()

source("load_data.R")
source("ols.R")
source("coxph.R")

novelty <- "/Users/martin/Documents/__thesis/__analysis/bgg/eventnet_data/bgg_two_mode_novelty_COND_SIZE_DHE.csv"
usefulness <- "/Users/martin/Documents/__thesis/__analysis/bgg/eventnet_data/bgg_two_mode_rating_COND_SIZE_DHE.csv"

run_coxph(prepare_data(novelty, min_eventsize = 2, use_rhom = TRUE, use_dhe = TRUE, normalize = TRUE, sqrt_transform = TRUE),print_vifs = TRUE,fout="../results/vifs_coxph_novelty.tex")
run_ols(prepare_data(novelty, min_eventsize = 2,observed = TRUE, use_rhom = TRUE, use_dhe = TRUE, normalize = TRUE, sqrt_transform = TRUE),print_vifs = TRUE,fout="../results/vifs_ols_novelty.tex")
run_coxph(prepare_data(usefulness, min_eventsize = 2, use_rhom = TRUE, use_dhe = TRUE, normalize = TRUE, sqrt_transform = TRUE),print_vifs = TRUE,fout="../results/vifs_coxph_usefulness.tex")
run_ols(prepare_data(usefulness, min_eventsize = 2,observed = TRUE, use_rhom = TRUE, use_dhe = TRUE, normalize = TRUE, sqrt_transform = TRUE),print_vifs = TRUE,fout="../results/vifs_ols_usefulness.tex")
