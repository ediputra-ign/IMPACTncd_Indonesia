library(fst)
library(data.table)

# Check BMI table structure
tbl <- read_fst("./inputs/exposure_distributions/Table_BMI_BCTo.fst", as.data.table = TRUE)
cat("BMI table columns:", names(tbl), "\n")
cat("BMI table nrow:", nrow(tbl), "\n")
cat("Age range:", range(tbl$age, na.rm=TRUE), "\n")
cat("Year range:", range(tbl$year, na.rm=TRUE), "\n")
cat("\nFirst few rows:\n")
print(head(tbl))

# Check SES table
tbl2 <- read_fst("./inputs/exposure_distributions/Table_SES_edu_final.fst", as.data.table = TRUE)
cat("\nSES table columns:", names(tbl2), "\n")
cat("SES table nrow:", nrow(tbl2), "\n")
cat("Age range:", range(tbl2$age, na.rm=TRUE), "\n")
cat("\nFirst few rows:\n")
print(head(tbl2))
