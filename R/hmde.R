library(hmde)
library(tidyverse)

# VB model
#Analytic solution in function form
solution <- function(t, pars = list(y_0, beta, S_max)){
  return(
    pars$S_max + (y_0 - pars$S_max)*exp(-t * pars$beta)
  )
}

#Parameters
beta <- 0.35 #Growth rate
y_0 <- 1 #Starting size
S_max <- 20 #Asymptotic max size
time <- c(0,30)
pars_list <- list(y_0 = y_0,
                  beta = beta,
                  S_max = S_max)
y_final <- solution(time[2], pars_list)

#Plot of growth function
ggplot() +
  xlim(y_0, y_final) +
  ylim(0, beta*(S_max-y_0)*1.1) +
  labs(x = "Y(t)", y = "f", title = "von Berralanffy growth") +
  theme_classic() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold")) +
  geom_function(fun=hmde_model_des("vb_single_ind"),
                args=list(pars = list(S_max, beta)),
                colour="green4", linewidth=1,
                xlim=c(y_0, y_final))

#Size over time
ggplot() +
  geom_function(fun=solution,
                args=list(pars = pars_list),
                colour="green4", linewidth=1,
                xlim=c(time)) +
  xlim(time) +
  ylim(0, y_final*1.05) +
  labs(x = "Time", y = "Y(t)", title = "von Bertalanffy growth") +
  theme_classic() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))


lizard_vb_fit <- hmde_model("vb_multi_ind") |>
  hmde_assign_data(data = Lizard_Size_Data)  |>
  hmde_run(chains = 4, cores = 1, iter = 2000)


lizard_vb_estimates <- hmde_extract_estimates(model = "vb_multi_ind",
                                              fit = lizard_vb_fit,
                                              input_measurement_data = Lizard_Size_Data)

measurement_data_transformed <- lizard_vb_estimates$measurement_data %>%
  group_by(ind_id) %>%
  mutate(
    delta_y_obs = y_obs - lag(y_obs),
    obs_interval = time - lag(time),
    obs_growth_rate = delta_y_obs/obs_interval,
    delta_y_est = y_hat - lag(y_hat),
    est_growth_rate = delta_y_est/obs_interval
  ) %>%
  ungroup()

#Distributions of estimated growth and size
hist(measurement_data_transformed$y_hat,
     main = "Estimated size distribution",
     xlab = "Size (cm)")
hist(measurement_data_transformed$delta_y_est,
     main = "Estimated growth increments",
     xlab = "Growth increment (cm)")
hist(measurement_data_transformed$est_growth_rate,
     main = "Estimated annualised growth rate distribution",
     xlab = "Growth rate (cm/yr)")

#Quantitative R^2
cor(measurement_data_transformed$y_obs, measurement_data_transformed$y_hat)^2

#Plots of size over time for a sample of 5 individuals
sample_ids <- sample(1:nrow(lizard_vb_estimates$individual_data), size=5)
plot_data <- measurement_data_transformed %>%
  filter(ind_id %in% sample_ids)

ggplot(data=plot_data, aes(group = ind_id)) +
  geom_point(aes(x = time, y=y_obs, colour = as.factor(ind_id)),
             shape = 1) +
  geom_line(aes(x = time, y=y_obs, colour = as.factor(ind_id)),
            linetype = "dashed") +
  geom_point(aes(x = time, y=y_hat, colour = as.factor(ind_id)),
             shape = 2) +
  geom_line(aes(x = time, y=y_hat, colour = as.factor(ind_id)),
            linetype = "solid") +
  labs(x="Time (days)", y="Size (mm)", colour="Ind. ID") +
  theme_classic()

## Canham model


g_max <- 1 #Max growth rate
S_max <- 10 #Size at which the maximum growth occurs
k <- 0.75
y_0 <- 1 #Starting size
y_final <- 40

#Plot of growth function
ggplot() +
  xlim(y_0, y_final) +
  labs(x = "Y(t)", y = "f", title = "Canham growth") +
  theme_classic() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold")) +
  geom_function(fun=hmde_model_des("canham_single_ind"),
                args=list(pars = list(g_max, S_max, k)),
                colour="green4", linewidth=1,
                xlim=c(y_0, y_final))


# tree_canham_fit <- hmde_model("canham_multi_ind") |>
#   hmde_assign_data(data = Tree_Size_Data)  |>
#   hmde_run(chains = 4, cores = 4, iter = 2000)
#
# Tree_Size_Ests <- hmde_extract_estimates(model = "canham_multi_ind",
#                                          fit = tree_canham_fit,
#                                          input_measurement_data = Tree_Size_Data)
#
# saveRDS(tree_canham_fit, "inst/extdata/tree_canham_fit.rds")
# saveRDS(Tree_Size_Ests, "inst/extdata/Tree_Size_Ests.rds")

measurement_data_transformed <- Tree_Size_Ests$measurement_data %>%
  group_by(ind_id) %>%
  mutate(
    delta_y_obs = y_obs - lag(y_obs),
    obs_interval = time - lag(time),
    obs_growth_rate = delta_y_obs/obs_interval,
    delta_y_est = y_hat - lag(y_hat),
    est_growth_rate = delta_y_est/obs_interval
  ) %>%
  ungroup()

#Distributions of estimated growth and size
hist(measurement_data_transformed$y_hat,
     main = "Estimated size distribution",
     xlab = "Size (cm)")
hist(measurement_data_transformed$delta_y_est,
     main = "Estimated growth increments",
     xlab = "Growth increment (cm)")
hist(measurement_data_transformed$est_growth_rate,
     main = "Estimated annualised growth rate distribution",
     xlab = "Growth rate (cm/yr)")

#Quantitative R^2
cor(measurement_data_transformed$y_obs, measurement_data_transformed$y_hat)^2

#Plots of size over time for a sample of 5 individuals
sample_ids <- sample(1:nrow(Tree_Size_Ests$individual_data), size=5)
plot_data <- measurement_data_transformed %>%
  filter(ind_id %in% sample_ids)

ggplot(data=plot_data, aes(group = ind_id)) +
  geom_point(aes(x = time, y=y_obs, colour = as.factor(ind_id)),
             shape = 1) +
  geom_line(aes(x = time, y=y_obs, colour = as.factor(ind_id)),
            linetype = "dashed") +
  geom_point(aes(x = time, y=y_hat, colour = as.factor(ind_id)),
             shape = 2) +
  geom_line(aes(x = time, y=y_hat, colour = as.factor(ind_id)),
            linetype = "solid") +
  labs(x="Time (years)", y="DBH (cm)", colour="Ind. ID") +
  theme_classic()
