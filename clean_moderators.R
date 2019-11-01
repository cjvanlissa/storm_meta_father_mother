library(tidyLPA)
df <- read.csv("data_cleaned.csv", stringsAsFactors = FALSE)

#plot(density(df$male[df$male >20 & df$male < 60], na.rm = TRUE))
#plot(density(df$Age_m_dv, na.rm = TRUE))
#head(df[,c("Age_m_dv", "Age_measure")])

df$Age_m_dv[df$Age_measure %in% "Months"] <- df$Age_m_dv[df$Age_measure %in% "Months"]/12

#table(df$Age_m_dv, df$Age_measure)

#table(df$Eligible)

#plot(density(df$Age_m_dv[df$Age_m_dv<18], na.rm = TRUE))
#hist(df$Age_m_dv[df$Age_m_dv<18], na.rm = TRUE)


fit_mix <- estimate_profiles(df$Age_m_dv, 1:3)

#plot_profiles(tmp)
#plot_density(fit_mix)

#table(tmp$model_1_class_2$dff$Class)

df$Age_group <- tmp$model_1_class_2$dff$Class
# Do this for the final data, so number of rows is correct

df$same_reporter <- FALSE
df$same_reporter[df$P_CR == "Ja" & df$PB_CR == "Ja" | df$P_PR == "Ja" &  df$PB_PR == "Ja"  | df$P_OBS == "Ja" &  df$PB_OBS == "Ja"] <- TRUE

df$same_wave <- ifelse(df$Design == "longitudinal", "No", "Yes")

df$country_group <- "Western"
df$country_group[trimws(df$Country) %in% c("China", "CO")] <- "Non-Western"
df$country_group[grepl(",", df$Country)] <- "Other"
#table(df$country_group)

write.csv(df, "data_cleaned_mods.csv", row.names = FALSE)