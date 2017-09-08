functions {
  real[] f(real[] s, real[] a, real[] b, real[] c, real[] d) {

    int tot_obs;
    real ret[dims(s)[1]];
    tot_obs = dims(s)[1];

    //apply logistic function to each observation
    for(obs in 1:tot_obs) {
      ret[obs] = a[obs]/(1 + exp(-b[obs]*s[obs]+c[obs])) + d[obs];
    }
    
    return ret;
  }
}
data {
  int<lower=0> N; //number of patients
  int<lower=0> K; //number biomarkers
  int<lower=0> tot_obs;
  
  int patient_idx[tot_obs];
  int biomarker_idx[tot_obs];
  
  vector[tot_obs] age;
  real y[tot_obs];
}
parameters {
  
  real<upper=0> a_abeta;
  real<upper=0> a_hippo;
  real<lower=0> a_tau;
  
  real<lower=0> d_abeta;
  real<lower=0> d_hippo;
  real<lower=0> d_tau;
  
  //real<lower=0> b_abeta;
  real<lower=0> b_hippo;
  real<lower=0> b_mmse;
  real<lower=0> b_tau;
  
  //real c_abeta;
  real c_hippo;
  real c_mmse;
  real c_tau;
  
  real sigma_abeta;
  real sigma_hippo;
  real sigma_mmse;
  real sigma_tau;
  
  vector<lower=0>[N] alpha;
  vector[N] beta;
}
transformed parameters {
  real a[K];
  real d[K];
  real<lower=0, upper=10> b[K];
  real c[K];
  real<lower=0> sigma[K];
  
  a[1] = a_abeta;
  a[2] = a_hippo;
  a[3] = -30;
  a[4] = a_tau;
  
  d[1] = d_abeta;
  d[2] = d_hippo;
  d[3] = 30;
  d[4] = d_tau;
  
  b[1] = 1;
  b[2] = b_hippo;
  b[3] = b_mmse;
  b[4] = b_tau;
  
  c[1] = 0;
  c[2] = c_hippo;
  c[3] = c_mmse;
  c[4] = c_tau;
  
  sigma[1] = sigma_abeta;
  sigma[2] = sigma_hippo;
  sigma[3] = sigma_mmse;
  sigma[4] = sigma_tau;
}
model {
  real s[tot_obs] = to_array_1d(alpha[patient_idx] .* age + beta[patient_idx]);
  y ~ normal(f(s, a[biomarker_idx], b[biomarker_idx], c[biomarker_idx], d[biomarker_idx]), sigma[biomarker_idx]);
  
  alpha ~ normal(1, 10);
  beta ~ normal(0, 10);
  
  a_abeta ~ normal(-110, 1);
  a_hippo ~ normal(-0.19,0.1);
  a_tau ~ normal(50,10);
  
  d_abeta ~ normal(245,1);
  d_hippo ~ normal(0.72,0.1);
  d_tau ~ normal(50,10);
  
  //b_abeta;
  b_hippo ~ normal(1,1);
  b_mmse ~ normal(1,1);
  b_tau ~ normal(1,1);
  
  //c_abeta;
  c_hippo ~ normal(0,20);
  c_mmse ~ normal(0,20);
  c_tau ~ normal(0,20);
}
generated quantities {
  vector[81] grid;
  real fhat[N,K,81];
  for(i in 1:81) grid[i] = -20 + 0.5*(i-1);
  for(n in 1:N) {
    for(k in 1:K)
      fhat[n,k,] = f(to_array_1d(alpha[n]*grid+beta[n]),rep_array(a[k], 81),rep_array(b[k], 81),rep_array(c[k], 81),rep_array(d[k], 81));
  }
}