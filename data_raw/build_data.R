# build_data.R
# Rebuild .rda data files from source .txt files
# This ensures the data is properly compiled and any fixes to source .txt are reflected in .rda

suppressPackageStartupMessages({
  library(usethis)
  library(dplyr)
})

# Use absolute paths
pkg_root <- "C:/Users/Victoria Mironova/Code/ggRootCellAtlas"
data_raw_dir <- file.path(pkg_root, "data_raw")

cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║ REBUILDING ggRootCellAtlas DATA FROM SOURCE .TXT FILES            ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")
cat("Package root:", pkg_root, "\n\n")

# Load all data from txt files (tab-delimited)
cat("Loading source .txt files (tab-delimited)...\n")
ggPm.At.longroot.longitudinal <- read.csv(file.path(data_raw_dir, "ggPm.At.longroot.longitudinal.txt"), sep = "\t")
ggPm.At.root.crosssection.m1 <- read.csv(file.path(data_raw_dir, "ggPm.At.root.crosssection.m1.txt"), sep = "\t")
ggPm.At.root.crosssection.m2 <- read.csv(file.path(data_raw_dir, "ggPm.At.root.crosssection.m2.txt"), sep = "\t")
ggPm.At.root.crosssection.t <- read.csv(file.path(data_raw_dir, "ggPm.At.root.crosssection.t.txt"), sep = "\t")
ggPm.At.root.crosssection.e1 <- read.csv(file.path(data_raw_dir, "ggPm.At.root.crosssection.e1.txt"), sep = "\t")
ggPm.At.root.crosssection.e2 <- read.csv(file.path(data_raw_dir, "ggPm.At.root.crosssection.e2.txt"), sep = "\t")
ggPm.At.root.crosssection.d <- read.csv(file.path(data_raw_dir, "ggPm.At.root.crosssection.d.txt"), sep = "\t")

cat("✓ Loaded 7 layout datasets\n\n")

# Verify data integrity before saving
cat("Verifying data before saving...\n")
cat("  e1 unique Atlas values:", length(unique(ggPm.At.root.crosssection.e1$Atlas)), "\n")
cat("  e2 unique Atlas values:", length(unique(ggPm.At.root.crosssection.e2$Atlas)), "\n")
cat("  e1 sample types:", paste(head(unique(ggPm.At.root.crosssection.e1$Atlas), 3), collapse=", "), "\n")
cat("  e2 sample types:", paste(head(unique(ggPm.At.root.crosssection.e2$Atlas), 3), collapse=", "), "\n\n")

# Save all data using usethis::use_data()
cat("Saving to data/ directory...\n")
use_data(ggPm.At.longroot.longitudinal, overwrite = TRUE)
use_data(ggPm.At.root.crosssection.m1, overwrite = TRUE)
use_data(ggPm.At.root.crosssection.m2, overwrite = TRUE)
use_data(ggPm.At.root.crosssection.t, overwrite = TRUE)
use_data(ggPm.At.root.crosssection.e1, overwrite = TRUE)
use_data(ggPm.At.root.crosssection.e2, overwrite = TRUE)
use_data(ggPm.At.root.crosssection.d, overwrite = TRUE)

cat("\n✓ All data files rebuilt successfully!\n")

# Final verification
cat("\nFinal verification:\n")
load("data/ggPm.At.root.crosssection.e2.rda")
cat("  Loaded e2 with cell types:\n")
print(sort(unique(ggPm.At.root.crosssection.e2$Atlas)))

cat("\n✓ Build complete - all .rda files updated from source .txt\n")
