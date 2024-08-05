# Function to check if a package is installed, install it if not, and load it
load_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

load_package("texreg")

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

file <- "../eventnet_data/bgg_two_mode_novelty_COND_SIZE_DHE.csv"
file2 <- "../eventnet_data/bgg_two_mode_rating_COND_SIZE_DHE.csv"

min_eventsize <- 2
models.coxph <- list(
  run_coxph(prepare_data(file, min_eventsize = min_eventsize, use_rhom = FALSE, use_dhe = FALSE, normalize = TRUE, sqrt_transform = FALSE)),
  run_coxph(prepare_data(file, min_eventsize = min_eventsize, use_rhom = TRUE, use_dhe = FALSE, normalize = TRUE, sqrt_transform = FALSE)),
  run_coxph(prepare_data(file2, min_eventsize = min_eventsize, use_rhom = TRUE, use_dhe = FALSE, normalize = TRUE, sqrt_transform = FALSE)),
  run_coxph(prepare_data(file, min_eventsize = min_eventsize, use_rhom = TRUE, use_dhe = TRUE, normalize = TRUE, sqrt_transform = FALSE)),
  run_coxph(prepare_data(file2, min_eventsize = min_eventsize, use_rhom = TRUE, use_dhe = TRUE, normalize = TRUE, sqrt_transform = FALSE))
)

filename <- "../results/h1.tex"

if (!is.null(filename)) {
  latex_code <- texreg(
    models.coxph,
    single.row = FALSE,
    siunitx = TRUE,
    digits = 2,
    stars = c(0.001, 0.01, 0.05, 0.1),
    custom.header = list("RHEM" = 1, "RHOM" = 2:3, "Designer x Mechanic" = 4:5),
    groups = list("RHEM Effects" = 1:11, "RHOM Effects" = 12:17, "Mixed-Mode Effects" = 18:20, "Mixed-Mode Controls" = 21:37),
    custom.model.names = c("base", "novelty", "usefulness", "novelty", "usefulness"),
    include.rsquared = TRUE,
    include.maxrs = FALSE,
    include.missings = FALSE,
    include.zph = FALSE,
    no.margin = TRUE,
    use.packages = FALSE,
    table = TRUE,
    return.string = TRUE # This makes texreg return the LaTeX code as a string
  )
  # Write the LaTeX code to a file
  cat(latex_code, file = filename)
} else {
  print(screenreg(
    models.coxph,
    single.row = TRUE,
    digits = 2,
    stars = c(0.001, 0.01, 0.05, 0.1),
    custom.header = list("RHEM" = 1, "RHOM" = 2:3, "Designer x Mechanic" = 4:5),
    custom.model.names = c("base", "novelty", "usefulness", "novelty", "usefulness"),
    groups = list("RHEM Effects" = 1:11, "RHOM Effects" = 12:17, "Mixed-Mode Effects" = 18:20, "Mixed-Mode Controls" = 21:37),
    include.rsquared = TRUE,
    include.maxrs = FALSE,
    include.missings = FALSE,
    include.zph = FALSE,
  ))
}
