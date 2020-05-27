
.onAttach <- function(libname, pkgname) {
  tip <- random_tip()
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}

random_tip <- function() {
  # Idea from https://github.com/tidyverse/ggplot2/blob/master/R/zzz.r
  tips <- c(
    "COSORE data are licensed under CC-BY-4: https://creativecommons.org/licenses/by/4.0/.",
    "Please strive to cite underlying dataset papers - DOIs are listed in csr_database() output.",
    "Type citation(\"cosore\") for the main COSORE database reference.",
    "For maximum impact and insight, involve COSORE data contributors as co-authors.",
    "Reward data sharing by including COSORE data contributors as co-authors."
  )

  sample(tips, 1)
}
