df <- read.csv("data.csv", stringsAsFactors = FALSE)

# Categorize parenting behaviors ------------------------------------------
positive <- c("P_W", "P_S", "P_po", "P_C")
negative <- c("P_P", "P_H", "P_ne", "P_I")

df$p_negative <- apply(df[negative], 1, function(x){any(x == "Ja")})

# Reverse-code negative correlations
df$R[which(df$p_negative)] <- -1*df$R[which(df$p_negative)]

# Check which correlations are still negative after reverse-coding
tmp <- df[which(df$p_negative), c("R", negative)]
colSums(tmp[tmp$R < 0, ][, -1] == "Ja", na.rm =T)

mean(df$R[df$P_Dem == "Ja"], na.rm= T)

# Drop rows ---------------------------------------------------------------
# Drop non-eligible
df <- df[df$Eligible == "yes", ]
# Drop cases with max missingness
missing <- rowSums(is.na(df))
df <- df[!missing == max(missing), ]
# Drop missing ID_P
df <- df[!is.na(df$ID_P), ]



run_length <- rle(as.vector(df$ID_P))
all(run_length$lengths[seq(1, length(run_length$lengths), by = 2)]== run_length$lengths[seq(2, length(run_length$lengths), by = 2)])


# Create unique merge id --------------------------------------------------

df$id_merge <- gsub(" ", "", apply(df[, grep("^ID(?!_P$)", names(df), value = TRUE, perl = TRUE)], 1, paste, collapse = "_"))

df$id_merge <- gsub("_(\\d+|NA)$", "", df$id_merge)

#df <- df[!is.na(df$id_merge), ]

tmp <- table(df$ID_P, df$id_merge)

double_ids <- colnames(tmp)[apply(tmp, 2, sum) == 2]
single_ids <- colnames(tmp)[apply(tmp, 2, sum) == 1]

#tmp <- df[df$id_merge %in% single_ids, ]

#tmp <- df[df$id_merge %in% single_ids, c(grep("^ID(?!_P$)", names(df), value = TRUE, perl = TRUE), "ID_P")]

df$id_merge[df$id_merge %in% single_ids] <- gsub("(warmth|F-F|F-|father-|M-M|M-|mother-)", "", df$id_merge[df$id_merge %in% single_ids])
df$id_merge[df$id_merge %in% single_ids] <- gsub("_.{0,4}?$", "", df$id_merge[df$id_merge %in% single_ids], perl = TRUE)
df$id_merge <- gsub("^(24_girls__PBgiving_LAB_1-2_).{2}", "\\1", df$id_merge)

# Merge data --------------------------------------------------------------

df_mother <- df[df$ID_P == "mother", ]
df_father <- df[df$ID_P == "father", ]

# Check number of effect sizes
#rowSums(!table(is.na(df_mother$R), df_mother$ID) == 0)
#rowSums(!table(is.na(df_father$R), df_father$ID) == 0)
#rowSums(!table(is.na(df_father$R_m_f), df_father$ID) == 0)
#rowSums(!table(is.na(df_mother$R_m_f), df_mother$ID) == 0)
#rowSums(!table(is.na(df$R_m_f), df$ID) == 0)
#rowSums(!table(is.na(df$R), df$ID) == 0)

# Check if all unique
any(duplicated(df_father$id_merge))
any(duplicated(df_mother$id_merge))

# Check if all in order
if(!all(diff(match(df_father$id_merge, df_mother$id_merge)) == 1)) stop("Difference in order of rows between mother and father")
#which((diff(match(df_father$id_merge, df_mother$id_merge)) == 1))
df_mother$R_f <- df_father$R
df_mother$R_m_f_check <- df_father$R_m_f

if(!all(na.omit(rowSums(cbind(df_mother$R_m_f, -1*df_mother$R_m_f_check))) == 0)){
  df_mother[!rowSums(cbind(df_mother$R_m_f, -1*df_mother$R_m_f_check)) == 0& !is.na(rowSums(cbind(df_mother$R_m_f, -1*df_mother$R_m_f_check))), c("R_m_f", "R_m_f_check", "Authors", "Pub_Year") ]
  stop("R_m_f was different between the mother and father rows")
}
df_mother$R_m_f_check <- NULL

write.csv(df_mother, "data_cleaned.csv", row.names = FALSE)

