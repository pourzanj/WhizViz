library(Ryacas)


K <- 9
M <- 4
ts <- map(1:(K+2*M), function(num) Sym(paste0("t",num)))

e <- list(t1=-20,t2=-20,t3=-20,t4=-20,t5=-16,t6=-12,t7=-8,t8=-4,t9=0,t10=4,t11=8,t12=12,t13=16,t14=20,t15=20,t16=20,t17=20) # K=9
#e <- list(t1=0,t2=0,t3=0,t4=0,t5=0.2,t6=0.4,t7=0.6,t8=0.8,t9=1.0,t10=1.0,t11=1.0,t12=1.0) # K = 4
#e <- list(t1=0,t2=0,t3=0,t4=0,t5=0.15,t6=0.3,t7=0.45,t8=0.6,t9=0.75, t10=1.0,t11=1.0,t12=1.0, t13=1.0) # K = 5
#e <- list(t1=0,t2=0,t3=0,t4=0,t5=0.15,t6=0.3,t7=0.45,t8=0.6,t9=0.75, t10=0.9,t11=1.0,t12=1.0, t13=1.0,t14=1.0) # K = 6 buggy numerically
#e <- list(t1=0,t2=0,t3=0,t4=0,t5=0.1428571,t6=0.2857143,t7=0.4285714,t8=0.5714286,t9=0.7142857, t10=0.8571429,t11=1.0,t12=1.0, t13=1.0,t14=1.0) # K = 6
#e <- list(t1=0,t2=0,t3=0,t4=0,t5=0.15,t6=0.3,t7=0.45,t8=0.6,t9=0.75, t10=0.9,t11=0.95,t12=1.0, t13=1.0,t14=1.0,t15=1.0) # K = 7
#e <- list(t1=0,t2=0,t3=0,t4=0,t5=0.1,t6=0.2,t7=0.3,t8=0.4,t9=0.5,t10=0.6,t11=0.7,t12=0.8,t13=0.9,t14=1.0,t15=1.0,t16=1.0,t17=1.0)

node_df <- tibble(node = names(e), location = unlist(e))

M1 <- function(i,K,ts) {
  if(i < 4 | i > K + 4) {
    #ret <- expression(0)
    ret <- 0
  }
  else {
    #if its the same node avoid dividing by zero. just set to 0
    #if(ts[[i+1]]-ts[[i]] == 0) ret <- expression(0)
    if(ts[[i+1]]-ts[[i]] == 0) ret <- 0
    else ret <- 1/(ts[[i+1]]-ts[[i]])
  }
  
  return(ret)
}

#when we replace ts with e. these are no longer yacas expr but actual numbers
M1_expr <- map(1:(K+2*M-1), M1, K = K, ts = e)

#takes a yacas expr, substitutes in the actual node values and returns a 1D R function
#yacas_to_R_func <- function(expr) {function(s) {Eval(yacas(expr), env = e) %>% as.expression %>% eval(list(y=s))}}

#from list of function define single piecewise function
#create_M1_piecewise <- function(f,i) {function(x) ifelse(between(x, e[[i]], e[[i+1]]), f(x), 0)}
create_M1_piecewise <- function(f,i) {function(x) ifelse(between(x, e[[i]], e[[i+1]]), f, 0)}

#create and plot functions
#M1_functions <- map(M1_expr, yacas_to_R_func) %>% map2(1:length(M1_expr), create_M1_piecewise)
M1_functions <- M1_expr %>% map2(1:length(M1_expr), create_M1_piecewise)

colors <- c("#F8766D", "#00BFC4", "#B79F00", "#619CFF", "#00BA38", "#F564E3")

plot_function_list <- function(fl) {
  p <- ggplot(data.frame(x = c(0, 1)), aes(x))
  for(i in 1:length(fl)) p <- p + stat_function(fun = fl[[i]], geom = "line", color = colors[[(i %% 6) + 1]])
  p + geom_label(aes(x = location, y = 0, label = node), data = node_df)
}

M1_functions_plot <- plot_function_list(M1_functions)

#M2
############################
##########################
############################

y <- Sym("y")

#recursion for creating higher order splines
M_ik <- function(i,k,ts,M1,M2) {
  if(ts[[i+k]]-ts[[i]] == 0) ret <- expression(0)
  else ret <- (k/(k-1))*((y-ts[[i]])*M1 + (ts[[i+k]]-y)*M2)/(ts[[i+k]]-ts[[i]])
  
  return(ret)
}

M2 <- function(i,ts,M1_expr) {
  
  A <- M_ik(i,2,ts,M1_expr[[i]],expression(0))
  B <- M_ik(i,2,ts,expression(0),M1_expr[[i+1]])
  
  return(list(A = A, B = B))
}

M2_expr <- map(1:(K+2*M-2),M2,ts=e,M1_expr = M1_expr)

#from list of function define single piecewise function
create_M2_piecewise <- function(f,i) {
  g <- function(x) { 
    if(x < e[[i]]) 0
    else if(between(x, e[[i]], e[[i+1]])) (f$A)(x)
    else if(between(x, e[[i+1]], e[[i+2]])) (f$B)(x)
    else 0
  }
  
  return(Vectorize(g))
}

#create and plot functions
M2_functions <- map(M2_expr, function(expr_list) map(expr_list, yacas_to_R_func)) %>% map2(1:length(M2_expr), create_M2_piecewise)

M2_functions_plot <- plot_function_list(M2_functions)

#M3
############################
##########################
############################
M3 <- function(i,ts,M2_expr) {
  A <- M_ik(i,3,ts,M2_expr[[i]]$A,expression(0))
  B <- M_ik(i,3,ts,M2_expr[[i]]$B,M2_expr[[i+1]]$A)
  C <- M_ik(i,3,ts,expression(0),M2_expr[[i+1]]$B)
  
  return(list(A=A, B=B,C=C))
}

M3_expr <- map(1:(K+2*M-3),M3,ts=e,M2_expr = M2_expr)

create_M3_piecewise <- function(f,i) {
  g <- function(x) { 
    if(x < e[[i]]) 0
    else if(between(x, e[[i]], e[[i+1]])) (f$A)(x)
    else if(between(x, e[[i+1]], e[[i+2]])) (f$B)(x)
    else if(between(x, e[[i+2]], e[[i+3]])) (f$C)(x)
    else 0
  }
  
  return(Vectorize(g))
}

#create and plot functions
M3_functions <- map(M3_expr, function(expr_list) map(expr_list, yacas_to_R_func)) %>% map2(1:length(M3_expr), create_M3_piecewise)

M3_functions_plot <- plot_function_list(M3_functions)

#M4
############################
##########################
############################
M4 <- function(i,ts,M3_expr) {
  A <- M_ik(i,4,ts,M3_expr[[i]]$A,expression(0))
  B <- M_ik(i,4,ts,M3_expr[[i]]$B,M3_expr[[i+1]]$A)
  C <- M_ik(i,4,ts,M3_expr[[i]]$C,M3_expr[[i+1]]$B)
  D <- M_ik(i,4,ts,expression(0),M3_expr[[i+1]]$C)
  
  return(list(A=A, B=B,C=C,D=D))
}

M4_expr <- map(1:(K+2*M-4),M4,ts=e,M3_expr = M3_expr)

create_M4_piecewise <- function(f,i) {
  g <- function(x) { 
    if(x < e[[i]]) 0
    else if(between(x, e[[i]], e[[i+1]])) (f$A)(x)
    else if(between(x, e[[i+1]], e[[i+2]])) (f$B)(x)
    else if(between(x, e[[i+2]], e[[i+3]])) (f$C)(x)
    else if(between(x, e[[i+3]], e[[i+4]])) (f$D)(x)
    else 0
  }
  
  return(Vectorize(g))
}

#create and plot functions
M4_functions <- map(M4_expr, function(expr_list) map(expr_list, yacas_to_R_func)) %>% map2(1:length(M4_expr), create_M4_piecewise)

M4_functions_plot <- plot_function_list(M4_functions)


#INTEGRATE to get I-splines
############################
##########################
############################
#Integrate(Eval(yacas(M4_expr[[1]]$D), env = e),y)

symbolic_Mspline_to_R_Ispline <- function(i, M4_expr) {
  expr_list <- map(M4_expr[[i]], function(expr) as.expression(Integrate(yacas(expr),y)))
  #expr_list <- map(M4_expr[[i]], function(expr) as.expression(Integrate(Eval(yacas(expr), env = e),y)))
  #expr_list <- map(M4_expr[[i]], function(expr) as.expression(Eval(yacas(expr), env = e)))
  
  f <- function(s) {
    ret <- 0
    
    if(s < e[[i]]) return(0)
    
    if(between(s,e[[i]],e[[i+1]])) return(ret + eval(expr_list[[1]], list(y=s)) - eval(expr_list[[1]], list(y=e[[i]])))
    else ret <- ret + eval(expr_list[[1]], list(y=e[[i+1]])) - eval(expr_list[[1]], list(y=e[[i]]))
    
    if(between(s,e[[i+1]],e[[i+2]])) return(ret + eval(expr_list[[2]], list(y=s)) - eval(expr_list[[2]], list(y=e[[i+1]])))
    else ret <- ret + eval(expr_list[[2]], list(y=e[[i+2]])) - eval(expr_list[[2]], list(y=e[[i+1]]))
    
    if(between(s,e[[i+2]],e[[i+3]])) return(ret + eval(expr_list[[3]], list(y=s)) - eval(expr_list[[3]], list(y=e[[i+2]])))
    else ret <- ret + eval(expr_list[[3]], list(y=e[[i+3]])) - eval(expr_list[[3]], list(y=e[[i+2]]))
    
    if(between(s,e[[i+3]],e[[i+4]])) return(ret + eval(expr_list[[4]], list(y=s)) - eval(expr_list[[4]], list(y=e[[i+3]])))
    else ret <- ret + eval(expr_list[[4]], list(y=e[[i+4]])) - eval(expr_list[[4]], list(y=e[[i+3]]))
    
    if(s > e[[i+4]]) ret <- 1
  
    return(ret)
  }
  
  return(Vectorize(f))
}

I_splines <- map(1:length(M4_expr), symbolic_Mspline_to_R_Ispline, M4_expr = M4_expr)

plot_function_list(I_splines)

convex_combination <- function(simplex) {
  functions <- map(1:length(simplex), function(n) symbolic_Mspline_to_R_Ispline(n,M4_expr))
  f <- function(x) reduce(map2(functions, simplex, function(f,s) s*f(x)),sum)
  
  return(Vectorize(f))
}

grid.arrange(M1_functions_plot, M2_functions_plot, M3_functions_plot, M4_functions_plot, plot_function_list(I_splines), random_I_spline_plot, nrow = 6)

#take random draw from convex combination
library(MCMCpack)
s <- rdirichlet(1, rep(1,length(I_splines)));
f <- convex_combination(s);
random_I_spline_plot <- ggplot(data.frame(x = c(-20, 20)), aes(x)) + stat_function(fun = f, geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line") +
  stat_function(fun = convex_combination(rdirichlet(1, rep(1,length(I_splines)))), geom = "line")


#translate to Stan
############################
##########################
############################
library(stringr)

"%+%" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}

expr_to_stan_str <- function(i, M4_expr) {
  expr_list <- map(M4_expr[[i]], function(expr) as.expression(Integrate(yacas(expr),y)))
  
  expr_functions <- map(expr_list, as.expression) %>% map(function(expr) {function(x) eval(expr, list(y = x))})
  
  expr_strings <- expr_list %>%
    map(as.character) %>%
    str_replace("Integrate\\( y \\) ", "") %>%
    str_replace_all("y", "x")

  str <- "real I" %+% as.character(i) %+% "(real x) { \n"
  str <- str %+% "\t real ret = 0;\n"
  str <- str %+% "\t if(x < " %+% as.character(e[[i]]) %+% ") return(0);\n\n"
  
  str <- str %+% "\t if(x >= " %+% as.character(e[[i]]) %+% " && x < " %+% as.character(e[[i+1]]) %+% ") return(ret + " %+% expr_strings[[1]] %+% " - " %+% as.character(expr_functions[[1]](e[[i]])) %+% ");\n"
  str <- str %+% "\t else ret = ret + " %+% as.character(expr_functions[[1]](e[[i+1]]) - expr_functions[[1]](e[[i]])) %+% ";\n\n"
  
  str <- str %+% "\t if(x >= " %+% as.character(e[[i+1]]) %+% " && x < " %+% as.character(e[[i+2]]) %+% ") return(ret + " %+% expr_strings[[2]] %+% " - " %+% as.character(expr_functions[[2]](e[[i+1]])) %+% ");\n"
  str <- str %+% "\t else ret = ret + " %+% as.character(expr_functions[[2]](e[[i+2]]) - expr_functions[[2]](e[[i+1]])) %+% ";\n\n"
  
  str <- str %+% "\t if(x >= " %+% as.character(e[[i+2]]) %+% " && x < " %+% as.character(e[[i+3]]) %+% ") return(ret + " %+% expr_strings[[3]] %+% " - " %+% as.character(expr_functions[[3]](e[[i+2]])) %+% ");\n"
  str <- str %+% "\t else ret = ret + " %+% as.character(expr_functions[[3]](e[[i+3]]) - expr_functions[[3]](e[[i+2]])) %+% ";\n\n"
  
  str <- str %+% "\t if(x >= " %+% as.character(e[[i+3]]) %+% " && x < " %+% as.character(e[[i+4]]) %+% ") return(ret + " %+% expr_strings[[4]] %+% " - " %+% as.character(expr_functions[[4]](e[[i+3]])) %+% ");\n"
  str <- str %+% "\t else ret = ret + " %+% as.character(expr_functions[[4]](e[[i+4]]) - expr_functions[[4]](e[[i+3]])) %+% ";\n\n"
  
  str <- str %+% "\t if(x >= " %+% as.character(e[[i+4]]) %+% ") return(1);\n\n"
  
  str <- str %+% "\t return(ret);\n"
  str <- str %+% "}"
  
  return(str)
}

map(1:length(M4_expr), expr_to_stan_str, M4_expr = M4_expr) %>% paste(collapse = "\n\n") %>% cat

#create convex combination function
str <- "real[] f(real[] x, vector[] c) {\n\n"
str <- str %+% "\t int N;\n"
str <- str %+% "\t real ret[dims(x)[1]];\n"
str <- str %+% "\t N = dims(x)[1];\n\n"
str <- str %+% "\t for(n in 1:N) ret[n] = "
for(i in 1:(length(M4_expr)-1)) str <- str %+% "c[n][" %+% as.character(i) %+% "]*I" %+% as.character(i) %+% "(x[n]) + "
str <- str %+% "c[n][" %+% as.character(length(M4_expr)) %+% "]*I" %+% as.character(length(M4_expr)) %+% "(x[n]);\n\n"
str <- str %+% "\t return(ret);\n}"

############################
##########################
############################
s <- rdirichlet(1, rep(1,length(I_splines)));
f <- convex_combination(s);

N <- 100
sim <- tibble(x = runif(N, 0, 1)) %>% mutate(y = f(x) + rnorm(N, sd = 0.05))

ggplot(sim, aes(x,y)) + geom_point() + stat_function(fun = f, geom = "line")

#run in Stan
dat <- list(N = N, x = sim$x, y = sim$y, sigma_prior = 0.05)
fit <- stan("I_splines_4nodes.stan", data = dat, chains = 1, iter = 1000)

posterior_summary <- (summary(fit, pars = "fhat")$summary)[,c("2.5%", "50%", "97.5%")] %>% as_tibble %>% mutate(x = seq(0, 1, by = 0.01))

ggplot() +
  geom_point(aes(x,y), data = sim) +
  stat_function(fun = f, geom = "line", data = data.frame(x = c(0, 1))) +
  geom_line(aes(x,`50%`),data = posterior_summary, color = "red") +
  geom_ribbon(aes(x, ymin=`2.5%`,ymax=`97.5%`), data = posterior_summary, alpha = 0.2)
