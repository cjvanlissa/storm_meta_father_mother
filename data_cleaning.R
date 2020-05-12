if(!require(foreign)) install.packages("foreign")
if(!require(psych)) install.packages("psych")
if(!require(devtools)) install.packages("devtools")
if(!require(motley)) install_github("cjvanlissa/motley")
if(!require(worcs)) install_github("cjvanlissa/worcs")
spss_data <- read.spss("Meta-codebook_5-02-2020.sav", to.data.frame = TRUE)
spss_data[spss_data == 999] <- NA


descriptives <- descriptives(spss_data)
write.csv(descriptives, "spss_data_descriptives.csv", row.names = FALSE)

# Note: Many variables with only one value. Remove these.
spss_data <- spss_data[, !descriptives$unique == 1]

# Check for variables with only missing data
which(colSums(is.na(spss_data)) == max(colSums(is.na(spss_data))))
table(spss_data$B)
# Seems not to be a problem

# Check for cases with only missing data
head(spss_data[rowSums(is.na(spss_data)) == max(rowSums(is.na(spss_data))), ])
spss_data <- spss_data[!rowSums(is.na(spss_data)) == max(rowSums(is.na(spss_data))), ]


# Save to file ------------------------------------------------------------

write.csv(spss_data, "data.csv", row.names = FALSE)