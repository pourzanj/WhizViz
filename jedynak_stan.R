library(stringr)
library(rstan)

#plot trajectories
logistic <- function(s, a, b, c, d) a*(1/(1 + exp(-b*s+c))) + d

adni_merge %>%
  filter(RID %in% sample(unique(adni_csf$RID), 20)) %>%
  ggplot(aes(AGE, -MMSE+30, group = RID)) + geom_point() + geom_line() +
  stat_function(fun = function(s) logistic(s, -110, 1, 75, 245), color = "red") +
  stat_function(fun = function(s) logistic(s, -110, 1, 80, 245), color = "green") +
  ylim(0,400)

adni_merge %>%
  filter(RID %in% c(906,sample(unique(adni_csf$RID), 20))) %>%
  ggplot(aes(AGE, HIPPO, group = RID, color = as.factor(RID))) + geom_point() + geom_smooth(span = 2, se = FALSE)  +
  stat_function(fun = function(s) logistic(s, -0.19, 2, 140, 0.72), color = "red") +
  xlim(50,100)

adni_csf %>%
  filter(RID %in% c(906,sample(unique(adni_csf$RID), 20))) %>%
  ggplot(aes(AGE, ABETA, group = RID, color = as.factor(RID))) + geom_point() + geom_smooth(span = 2, se = FALSE) +
  stat_function(fun = function(s) logistic(s, -110, 0.8, 50, 245), color = "red")

#try synthetic data
N <- 5
patients <- tibble(patient_id = 1:N, age = sample(60:80, N, replace = TRUE)) %>%
  mutate(alpha = abs(rnorm(N, 1, 1)), beta = rnorm(N, 0, 1))

tot_obs <- 100
K <- 4
logistic <- function(s, a, b, c, d) a*(1/(1 + exp(-b*s+c))) + d
theta <- matrix(c(1, 1.0, 0, 0,
                  1, 1.0, 3, 0,
                  1, 1.2, -2, 0,
                  1, 0.9, -6, 0), nrow = 4, byrow = TRUE)
sigma <- c(0.1, 0.1, 0.1, 0.1)

obs <- tibble(patient_id = sample(1:N, tot_obs, replace = TRUE),
              biomarker = sample(1:K, tot_obs, replace = TRUE)) %>%
  inner_join(patients) %>%
  mutate(age = age + rnorm(tot_obs, 0, 2.5)) %>%
  mutate(age_normalized = age - mean(age)) %>%
  mutate(s = alpha*age_normalized + beta) %>%
  arrange(patient_id, biomarker, age) %>%
  mutate(value = Vectorize(logistic)(s, theta[biomarker,1], theta[biomarker,2], theta[biomarker,3], theta[biomarker,4])) %>%
  mutate(noise = rnorm(tot_obs)*sigma[biomarker]) %>%
  mutate(y = value + noise)

obs %>% ggplot(aes(age, y, group = patient_id, color = as.factor(patient_id))) + geom_point() + geom_line() + facet_grid(biomarker ~ ., scale = "free")

#run in Stan
dat <- list(N = N, K = K, tot_obs = tot_obs, b_prior = 1, c_prior = 10, sigma_prior = 1, patient_idx = pull(obs, patient_id),
            biomarker_idx = pull(obs, biomarker), age = pull(obs, age_normalized), y = pull(obs, y))
fit <- stan("jedynak.stan", data = dat, chains = 1, iter = 4000)

#plot samples over data
samples <- extract(fit)
obs %>% ggplot(aes(age, y, group = patient_id)) + geom_point() + geom_line() +
  stat_function(fun = function(s) {samp <- 1; logistic(s, samples$a[samp], samples$b[samp], samples$c[samp], samples$d[samp])}, color = "red")

# plots should show differences in biomarkers amongst different DX
adni_merge <- read_csv("~/Alzheimers/data/ADNI/ADNIMERGE.csv") %>%
  select(RID, VISCODE, SITE, AGE, PTGENDER, DX, MMSE, Hippocampus, WholeBrain_bl) %>%
  mutate(HIPPO = Hippocampus/WholeBrain_bl*100)

adni_csf <- read_csv("~/Alzheimers/data/ADNI/UPENNBIOMK_MASTER.csv") %>%
  select(RID, VISCODE, SITE, ABETA, TAU)

large_sites <- adni_merge %>% group_by(SITE) %>% summarize(n = n()) %>% filter(n > 300) %>% select(SITE)

adni_merge %>% filter(DX %in% c("NL", "MCI", "Dementia")) %>%
  mutate(DX = factor(DX, levels = c("NL", "MCI", "Dementia"))) %>%
  #inner_join(large_sites) %>%
  ggplot(aes(HIPPO)) +
  geom_histogram() +
  geom_vline(xintercept = 0.72, color = "red") +
  geom_vline(xintercept = 0.65, color = "red") +
  geom_vline(xintercept = 0.53, color = "red") +
  facet_grid(DX ~ .)

adni_merge %>% filter(DX %in% c("NL", "MCI", "Dementia")) %>%
  mutate(DX = factor(DX, levels = c("NL", "MCI", "Dementia"))) %>%
  ggplot(aes(MMSE)) +
  geom_histogram() +
  facet_grid(DX ~ .) +
  scale_x_log10()

adni_csf %>% filter(DX %in% c("NL", "MCI", "Dementia")) %>%
  mutate(DX = factor(DX, levels = c("NL", "MCI", "Dementia"))) %>%
  ggplot(aes(ABETA)) +
  geom_histogram() +
  geom_vline(xintercept = 245, color = "red") +
  geom_vline(xintercept = 135, color = "red") +
  facet_grid(DX ~ .)

adni_csf %>% filter(DX %in% c("NL", "MCI", "Dementia")) %>%
  mutate(DX = factor(DX, levels = c("NL", "MCI", "Dementia"))) %>%
  ggplot(aes(TAU)) +
  geom_histogram() +
  geom_vline(xintercept = 50, color = "red") +
  geom_vline(xintercept = 100, color = "red") +
  facet_grid(DX ~ .) +
  xlim(0,300)

############################
############################
#import MMSE and hippcampus
normalize_biomarker <- function(x) (x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))

adni_merge <- read_csv("~/Alzheimers/data/ADNI/ADNIMERGE.csv") %>%
  select(RID, VISCODE, SITE, AGE, PTGENDER, DX, MMSE, Hippocampus, WholeBrain_bl) %>% 
  
  #normalize Hippocampus to Wholebrain_bl (these is correlation between the two)
  mutate(HIPPO = Hippocampus/WholeBrain_bl * 100) %>%
  
  #change age to age at time of measurement
  mutate(months_since_bl = ifelse(VISCODE == "bl", 0,
                                  str_extract(VISCODE, "[0-9]+"))) %>%
  mutate(AGE = AGE + as.integer(months_since_bl)/12) %>%
  select(RID, VISCODE, PTGENDER, AGE, DX, MMSE, HIPPO)

  #normalize biomarkers to increase over time
  #mutate(MMSE = -MMSE, HIPPO = -HIPPO)
  
  #normalize biomarkers to bet between 0 and 1
  #mutate_at(vars(MMSE, HIPPO), normalize_biomarker)

#import ABETA and TAU
adni_csf <- read_csv("~/Alzheimers/data/ADNI/UPENNBIOMK_MASTER.csv") %>%
  select(RID, VISCODE, ABETA, TAU) %>%
  
  #get age at measurement from adni_merge
  inner_join(adni_merge) %>%
  select(RID, VISCODE, PTGENDER, AGE, DX, ABETA, TAU)

  #change age to age at time of measurement
  mutate(months_since_bl = ifelse(VISCODE == "bl", 0,
                                str_extract(VISCODE, "[0-9]+"))) %>%
  mutate(AGE = AGE + as.integer(months_since_bl)/12) %>%
  select(RID, VISCODE, PTGENDER, AGE, DX, MMSE, HIPPO)

  #normalize biomarkers to increase over time
  #mutate(ABETA = -ABETA)
  
  #normalize biomarkers to bet between 0 and 1
  #mutate_at(vars(ABETA, TAU), normalize_biomarker)

#turn RIDs in to an index for Stan
adni_merge_unique_ids <- adni_merge %>% pull(RID) %>% unique
adni_csf_unique_ids <- adni_csf %>% pull(RID) %>% unique
unique_ids <- c(adni_merge_unique_ids, adni_csf_unique_ids) %>% unique

set.seed(1991)
num_csf_obs <- adni_csf %>%
  group_by(RID) %>%
  summarize(PTGENDER = head(PTGENDER,1),AGE = head(AGE, 1), DX = head(DX,1), patient_idx = head(RID,1), n = n(), unique_epochs = length(unique(VISCODE)), median_abeta = median(ABETA)) %>%
  arrange(desc(n)) %>%
  filter(RID %in% c(331,729,835,671,1010,5,16,677,4090,4874))
  #filter(unique_epochs > 5) %>%
  #sample_n(10)

adni_ids <- tibble(RID = unique_ids) %>%
  inner_join(num_csf_obs) %>%
  dplyr::select(RID) %>%
  add_row(RID = 906) %>%
  #filter(RID %in% c(106,112,331,835)) %>%
  mutate(patient_idx = row_number()) %>%
  dplyr::select(RID, patient_idx)

#spread and bind both dataframes
adni <- gather(adni_merge, biomarker, y, MMSE, HIPPO) %>%
  rbind(gather(adni_csf, biomarker, y, ABETA, TAU)) %>%
  #inner_join(adni_ids) %>%
  filter(!is.na(y))  %>%
  mutate(age_normalized = AGE - mean(AGE)) %>%
  #filter(biomarker %in% c("ABETA","TAU")) %>%
  mutate(biomarker_idx = as.integer(as.factor(biomarker)))

# save to csv
adni <- adni %>%
  select(-VISCODE, -biomarker_idx, -age_normalized) %>%
  mutate(DX = factor(DX, levels = c("NL", "NL to MCI", "MCI", "MCI to Dementia", "Dementia", "MCI to NL", "Dementia to MCI", "NL to Dementia"))) %>%
  mutate(BIOMARKER = biomarker) %>%
  mutate(VALUE = y) %>%
  select(-biomarker,-y) %>%
  filter(!is.na(VALUE))

save(adni, file = "adni_precleaned.Rdata")
  
#plot data
adni %>%
  ggplot(aes(age_normalized, y, group = RID, color = as.factor(RID))) +
  geom_point(aes(text = DX)) +
  facet_grid(biomarker ~ RID, scale = "free")# +
  #geom_smooth(span = 2, se = FALSE)

ggplotly(p)

adni %>%
  filter(biomarker == "ABETA") %>%
  ggplot(aes(age_normalized, y, group = RID, color = as.factor(RID))) + geom_point() + geom_smooth(span = 2, se = FALSE) +
  stat_function(fun = function(s) logistic(s, -110, 2.28, 19.52, 245), color = "black")

#look at demographics of patients we plot
adni %>%
  #filter(patient_idx %in% c(45, 85, 46, 50, 54)) %>%
  group_by(RID) %>% summarize(PTGENDER = head(PTGENDER,1),AGE = head(AGE, 1), DX = head(DX,1), patient_ids = head(RID,1))

#fit to stan
N <- max(adni$patient_idx)
dat <- list(N = N, K = max(adni$biomarker_idx), tot_obs = nrow(adni),
            patient_idx = pull(adni, patient_idx),
            biomarker_idx = pull(adni, biomarker_idx), age = pull(adni, age_normalized), y = pull(adni, y))
fit_normal <- stan("jedynak_unnormalized_biomarkers.stan", data = dat, chains = 1, iter = 4000, refresh = 40)
fit_spline <- stan("jedynak_isplines.stan", data = dat, chains = 1, iter = 4000, refresh = 40)
fit_spline <- stan("jedynak_random_sigma.stan", data = dat, chains = 1, iter = 3000, warmup = 2000, refresh = 10)

fit <- fit_spline

#plot over data
shat_samples <- (rstan::extract(fit, pars = "shat")$shat)[sample(1:1000,100),,]
sample_to_grid_s <- function(shat_sample) shat_sample %>% t %>% as_tibble %>% mutate(grid = seq(-20,20,0.5)) %>% gather(patient_idx,shat,-grid) %>% mutate(patient_idx = as.integer(str_extract(patient_idx,"[0-9]+")))
master_grid_s <- map(1:dim(shat_samples)[1], function(i) sample_to_grid_s(shat_samples[i,,]) %>% mutate(sample_num = i)) %>% bind_rows %>% full_join(adni_ids) 

disease_prog_plot <- master_grid_s %>%
  ggplot(aes(grid, shat, group = sample_num, color = as.factor(RID))) +
  geom_line(alpha = 0.1) +
  facet_grid(. ~ RID, scale = "free")


fhat_samples <- (rstan::extract(fit, pars = "fhat")$fhat)[sample(1:1000,100),,,]
biomarker_names <- c("ABETA","HIPPO","MMSE","TAU")

sample_to_grid <- function(fhat_sample_i) map(1:N, function(j) fhat_sample_i[j,,] %>% t %>% as_tibble %>% set_names(biomarker_names) %>% mutate(grid = seq(-20,20,0.5)) %>% mutate(patient_idx = j) %>% gather(biomarker,yhat,-patient_idx,-grid)) %>% bind_rows
master_grid <- map(1:dim(fhat_samples)[1], function(i) sample_to_grid(fhat_samples[i,,,]) %>% mutate(sample_num = i)) %>% bind_rows %>% full_join(adni_ids)

lp_samples <-(rstan::extract(fit, pars = "lp__"))$lp__
max_lp_idx <- which(lp_samples == max(lp_samples))
post_med <- adni %>% group_by(RID) %>%
  summarize(patient_idx = head(patient_idx,1), min_age = min(age_normalized), max_age = max(age_normalized)) %>%
  arrange(patient_idx) %>%
  mutate(alpha_post_med = (rstan::extract(fit, pars = "alpha"))$alpha[max_lp_idx,]) %>%
  mutate(beta_post_med = (rstan::extract(fit, pars = "beta"))$beta[max_lp_idx,])

est_log_grid <- post_med %>%
  pmap(function(RID,patient_idx,min_age,max_age,alpha_post_med, beta_post_med)
    tibble(adjusted_age = seq(-20,20,by=0.5)) %>%
      mutate(s = alpha_post_med*adjusted_age + beta_post_med) %>%
      mutate(RID = RID) %>%
      mutate_at(vars(s), do.call(funs,logistic_functions))) %>% 
  bind_rows() %>%
  gather(biomarker, yhat, ABETA, HIPPO, MMSE, TAU)

biomarker_plot <- adni %>%
  ggplot(aes(age_normalized, y, group = RID, color = as.factor(RID))) +
  geom_line(aes(grid, yhat, group = sample_num), alpha = 0.1, data = master_grid) +
  #geom_line(aes(adjusted_age,yhat),data = est_log_grid, color = "black") +
  geom_point(aes(text = DX), color = "black") +
  facet_grid(biomarker ~ RID, scale = "free")

grid.arrange(disease_prog_plot,biomarker_plot,nrow=2)

#deprecatated
get_post_sample <- function(i) {
  adni %>% group_by(RID) %>%
  summarize(patient_idx = head(patient_idx,1), min_age = min(age_normalized), max_age = max(age_normalized)) %>%
  arrange(patient_idx) %>%
  mutate(alpha_post_draw = (rstan::extract(fit, pars = c("alpha"))$alpha)[i,]) %>%
  mutate(beta_post_draw = (rstan::extract(fit, pars = c("beta"))$beta)[i,])
}

logistic_par_names <- c("a","b", "c", "d")
biomarker_names <- c("ABETA","HIPPO","MMSE","TAU")

ith_biomarker_samples_to_df <- function(i) {
  rstan::extract(fit, pars = logistic_par_names) %>%
    map(function(mat) mat[,i]) %>%
    as_tibble %>%
    mutate(biomarker = biomarker_names[i]) %>%
    head(100) %>%
    bind_cols()
}

ith_patient_samples_to_df <- function(i) {
  rstan::extract(fit, pars = c("alpha", "beta")) %>%
    map(function(mat) mat[,i]) %>%
    as_tibble %>%
    mutate(patient_idx = i) %>%
    head(100) %>%
    bind_cols()
}

patient_samples <- map(1:N, ith_patient_samples_to_df) %>% bind_rows
biomarker_samples <- map(1:4, ith_biomarker_samples_to_df) %>% bind_rows

samples <- expand(biomarker_samples, biomarker, patient_idx = patient_samples$patient_idx) %>%
  inner_join(biomarker_samples) %>%
  inner_join(patient_samples) %>%
  inner_join(adni_ids)

logistic_functions <- tibble(a = (summary(fit, pars = c("a"))$summary)[,"50%"],
                             b = (summary(fit, pars = c("b"))$summary)[,"50%"],
                             c = (summary(fit, pars = c("c"))$summary)[,"50%"],
                             d = (summary(fit, pars = c("d"))$summary)[,"50%"]) %>%
  pmap(function(s,a,b,c,d) {function(s) logistic(s,a,b,c,d)}) %>%
  set_names(c("ABETA","HIPPO","MMSE","TAU"))

post_sample_to_grid <- function(df) {
  df %>% pmap(function(RID,patient_idx,min_age,max_age,alpha_post_draw, beta_post_draw)
    tibble(adjusted_age = seq(min_age-10,max_age+10,by=0.2)) %>%
      mutate(s = alpha_post_draw*adjusted_age + beta_post_draw) %>%
      mutate(RID = RID) %>%
      mutate_at(vars(s), do.call(funs,logistic_functions))) %>% 
    bind_rows() %>%
    gather(biomarker, yhat, ABETA, HIPPO, MMSE, TAU)
}

logistic_function_grid <- sample(1:2000,100) %>% map(function(i) get_post_sample(i) %>% post_sample_to_grid %>% mutate(sample_num = i)) %>% bind_rows
p + geom_line(aes(adjusted_age, yhat, group = sample_num), alpha = 0.1, data = logistic_function_grid) + geom_line(aes(adjusted_age,yhat),data = est_log_grid, color = "black")



p + geom_line(aes(adjusted_age,yhat),data = est_log_grid, color = "black")

p + geom_line(aes(adjusted_age, yhat, group = sample_num), alpha = 0.1, data = logistic_function_grid) + geom_line(aes(adjusted_age,yhat),data = est_log_grid, color = "black")

#compare to optimization
jedynak_model <- stan_model(file = "jedynak.stan")
fit_opt <- optimizing(jedynak_model, data = dat, hessian = TRUE)

qplot(summary(fit, pars = "alpha")$summary[,"50%"], fit_opt$par[1:1737])
qplot(summary(fit, pars = "beta")$summary[,"50%"], fit_opt$par[1738:(1737+1737)])

#pick out five specific patients to look at
num_obs <- adni %>%
  group_by(RID) %>%
  summarize(PTGENDER = head(PTGENDER,1),AGE = head(AGE, 1), DX = head(DX,1), patient_idx = head(patient_idx,1), n = n()) %>%
  arrange(desc(n))

selected_patients <- c(43)

longitudinal_plot <- adni %>%
  filter(patient_idx %in% selected_patients) %>%
  #ggplot(aes(AGE, y, group = patient_idx, color = as.factor(patient_idx))) +
  ggplot() +
  geom_point(aes(AGE, y, group = patient_idx, color = as.factor(patient_idx))) +
  geom_smooth(aes(AGE, y, group = patient_idx, color = as.factor(patient_idx)),se = FALSE, span = 2) +
  facet_grid(biomarker ~ ., scale = "free") +
  theme_bw() +
  xlab("Age") +
  ylab("Biomarker Progression") +
  scale_color_discrete(name = "Patient ID", labels = c("1","2","3", "4", "5"), guide = FALSE)

#add posterior predictive s-curves
samp <- rstan::extract(fit, pars = "fhat_43")$fhat_43
posterior_predictive_43 <- samp %>%
  apply(c(2,3), median) %>% as_tibble %>%
  mutate(x = seq(60,100,0.2)) %>%
  gather(biomarker, sample, -x) %>%
  mutate(biomarker = ifelse(biomarker == "V1", "ABETA", biomarker)) %>%
  mutate(biomarker = ifelse(biomarker == "V2", "HIPPO", biomarker)) %>%
  mutate(biomarker = ifelse(biomarker == "V3", "MMSE", biomarker)) %>%
  mutate(biomarker = ifelse(biomarker == "V4", "TAU", biomarker))

longitudinal_plot + geom_line(aes(x=x,y=sample), data= posterior_predictive_43)

#
num_obs %>% filter(patient_idx %in% selected_patients)

s <- rstan::extract(fit)
alpha <- s$alpha[,selected_patients] %>%
  as_tibble %>%
  gather(patient, sample) %>%
  mutate(parameter = "α")

beta <- s$beta[,selected_patients] %>%
  as_tibble %>%
  gather(patient, sample) %>%
  mutate(parameter = "β")

alpha_beta_samples <- rbind(alpha, beta) %>% mutate(patient = factor(patient))
levels(alpha_beta_samples$patient) <- c("1", "2", "3", "4", "5")

posterior_histogram_plot <- alpha_beta_samples %>% ggplot(aes(sample, group = patient, fill = patient)) +
  geom_histogram(position = "identity", binwidth = 0.01) +
  facet_grid(patient ~ parameter, scales = "free") +
  theme_bw() +
  scale_fill_discrete(name = "Patient ID", labels = c("1","2","3", "4", "5")) +
  xlab("Parameter Value") +
  ylab("Sample Count")

library(gridExtra)
library(grid)
grid.arrange(longitudinal_plot, posterior_histogram_plot, ncol = 2)

#plot s_curves
mean_age <- 73.62293
logistic_functions_df <- tibble(s = seq(60, 90, 0.1)) %>%
  mutate(ABETA = logistic(s-mean_age, 1, 1, 0, 0)) %>%
  mutate(HIPPO = logistic(s-mean_age, 1, 0.36, 0.37, 0)) %>%
  mutate(MMSE = logistic(s-mean_age, 1, 2.03, 3.93, 0)) %>%
  mutate(TAU = logistic(s-mean_age, 1, 0.715, 2.03, 0)) %>%
  gather(Biomarker, Value, -s)

logistic_biomarkers_plot <- logistic_functions_df %>%
  ggplot(aes(s, Value, group = Biomarker, color = Biomarker)) +
  geom_line() +
  xlab("Adjusted Age") +
  ylab("Biomarker Progression") +
  theme_bw() +
  scale_color_manual(values = c("#00B9E3", "#619CFF", "#DB72FB", "#FF61C3")) +
  xlab("")
  
#histogram of where patients are
max_patient_age <- adni %>%
  filter(patient_idx %in% selected_patients) %>%
  group_by(patient_idx) %>%
  summarize(AGE = max(AGE)) %>%
  mutate(patient = paste0("V", 1:5)) %>%
  select(-patient_idx)

s_histogram_df <- alpha %>% select(-parameter) %>%
  rename(alpha = sample) %>%
  mutate(beta = beta$sample) %>%
  inner_join(max_patient_age) %>%
  mutate(s = alpha*AGE + beta + mean_age) %>%
  mutate(patient = factor(patient))

levels(s_histogram_df$patient) <- c("1", "2", "3", "4", "5")

s_posterior_predictive_plot <- s_histogram_df %>% ggplot(aes(s, group = patient, fill = patient)) +
  geom_histogram() +
  facet_grid(patient ~ ., scales = "free") +
  theme_bw() +
  xlab("Adjusted Age") +
  ylab("Sample Count") +
  scale_fill_discrete(name = "Patient ID", labels = c("1","2","3", "4", "5")) +
  xlim(60, 90)

grid.arrange(logistic_biomarkers_plot, s_posterior_predictive_plot, nrow = 2)
