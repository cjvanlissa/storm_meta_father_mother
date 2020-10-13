library(tidyLPA)
df <- read.csv("data_cleaned.csv", stringsAsFactors = FALSE)

#plot(density(df$male[df$male >20 & df$male < 60], na.rm = TRUE))
#length(unique(df$ID[df$male <20 | df$male > 60]))
#plot(density(df$Age_m_dv, na.rm = TRUE))
#head(df[,c("Age_m_dv", "Age_measure")])

df$Age_m_dv[df$Age_measure %in% "Months"] <- df$Age_m_dv[df$Age_measure %in% "Months"]/12

#table(df$Age_m_dv, df$Age_measure)

#table(df$Eligible)

plot(density(df$Age_m_dv[df$Age_m_dv<18], na.rm = TRUE))
#hist(df$Age_m_dv[df$Age_m_dv<18], na.rm = TRUE)
set.seed(4742)
fit_mix <- estimate_profiles(df$Age_m_dv, 1:3)

plot_profiles(fit_mix)
plot_density(fit_mix)

table(fit_mix$model_1_class_2$dff$Class)

df$age_group <- factor(fit_mix$model_1_class_2$dff$Class, labels = c("Young", "Old"))
rowSums(!table(df$age_group, df$ID) ==0)
df$age_group3 <- factor(fit_mix$model_1_class_3$dff$Class, labels = c("Young", "Med", "Old"))
rowSums(!table(df$age_group3, df$ID) ==0)
fit_mix$model_1_class_2$estimates

df$same_reporter <- FALSE
df$same_reporter[df$P_CR == "Ja" & df$PB_CR == "Ja" | df$P_PR == "Ja" &  df$PB_PR == "Ja"  | df$P_OBS == "Ja" &  df$PB_OBS == "Ja"] <- TRUE

df$observation <- "survey"
df$observation[df$P_OBS == "Ja" |  df$PB_OBS == "Ja"] <- "observation"
#table(df$observation)

df$same_wave <- ifelse(df$Design == "longitudinal", "No", "Yes")

df$country_group <- "Western"
df$country_group[trimws(df$Country) %in% c("China", "CO")] <- "Non-Western"
df$country_group[grepl(",", df$Country)] <- "Other"
#rowSums(!table(df$country_group, df$ID) ==0)

#create moderator variable for parenting dimension positive vs. negative, positive vs. negative vs. control, warm vs sens vs neg vs cont.
df$p_dimension[df$P_W == "Ja" | df$P_S == "Ja"| df$P_po =="Ja"] <- "positive"
df$p_dimension[df$P_P== "Ja" | df$P_I== "Ja"| df$P_H=="Ja" | df$P_ne=="Ja" | df$P_C== "Ja"] <- "negative"
#table(df$p_dimension)

df$p_cluster[df$P_W == "Ja" | df$P_S == "Ja" ] <- "positive"
df$p_cluster[df$P_H=="Ja"  ] <- "negative"
df$p_cluster[df$P_C== "Ja"| df$P_I=="Ja"|df$P_P== "Ja" ] <- "control"
#table(df$p_cluster)
write.csv(df, "data_cleaned_mods.csv", row.names = FALSE)

