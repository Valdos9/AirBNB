pkgs <- c("tidyverse", "lubridate", "broom", "rmarkdown", "knitr")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
message("Setup complete. Open project2.Rmd and click Knit (requires Pandoc, bundled with RStudio).")
