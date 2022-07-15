

# Write a simple vif function
get_vif = function(mymodel){
  myvif <- car::vif(mymodel)
  if(is.matrix(myvif)){
    max(myvif[,3]^2, na.rm = TRUE) %>% return()
  }else{
    max(myvif, na.rm = TRUE) %>% return()
  }
}


# Build a function to make ranges
get_range = function(x, n = NULL, a = NULL, b = NULL){
  if(is.null(a) | is.null(b)){
    myrange <- range(x, na.rm = TRUE)
    seq(from = myrange[1], to = myrange[2], length.out = n) %>%
      return()
  }else{seq(from = a, to = b, length.out = n) %>% return()}
}


get_sim = function(m, i, newdata, dist, simple = TRUE, phi = FALSE){
  #print(i)
  mymodel <- m
  # If phi = TRUE, update Phi with simulated one; but since it's kind of our error term, varying it is weird.
  if(phi == TRUE){mymodel$coefficients$precision <- dist[i,"(phi)"]}
  mymodel$coefficients$mean <- dist[i, 1:(ncol(dist)-1) ]

  thesim <- tibble(
    mu = predict(mymodel, newdata = newdata, type = "response"),
    phi = predict(mymodel, newdata = newdata, type = "precision"),
    newdata)

  if(simple==TRUE){
    thesim %>%
      # Now consolidate the varying predictor values into one column, x
      split(.$var) %>%
      map_dfr(~mutate(., mu = .$mu %>% unlist(), phi = .$phi %>% unlist(), x = .[, unique(.$var)] %>% unlist()) %>%
                select(mu, phi, x, var)) %>%
      return()
  }else{
    return(thesim)
  }
}

get_fun = function(data){
  data %>%
    summarize(pv = rbeta(n = 1, shape1 = mu * phi, shape2 = (1 - mu)*phi),
              ev = rbeta(n = 1000, shape1 = mu * phi, shape2 = (1 - mu)*phi) %>% mean()) %>%
    return()
}


get_bands = function(data, qi, probs = c(0.50, 0.025, 0.975), fd = FALSE){
  # Get name of variable
  name <- qi

  if(fd == TRUE){
    mybands <- data %>%
      rename(qi = qi) %>%
      summarize(
        name = name,
        qmed = quantile(qi, probs = 0.5),
        qmin = quantile(qi, probs = 0.025),
        qmax = quantile(qi, probs = 0.975),
        se = sd(qi),
        p_value = (1 - if_else(qmed > 0, true = sum(qi > 0), false = sum(qi < 0)) / n() ),
        p_other = 2*pt(-abs(   (qmed - mean(qi)  ) / ( se / sqrt(n()) )  ),df=n-1) )
  }else{
    mybands <- data %>%
      rename(qi = qi) %>%
      summarize(
        name = name,
        qmed = quantile(qi, probs = probs[1]),
        qmin = quantile(qi, probs = probs[2]),
        qmax = quantile(qi, probs = probs[3]))
  }
  return(mybands)
}
get_predict = function(m, newdata){
  #print(i)
  tibble(
    mu = predict(m, newdata = newdata, type = "response"),
    phi = predict(m, newdata = newdata, type = "precision"),
    newdata) %>%
    # Now consolidate the varying predictor values into one column, x
    split(.$var) %>%
    map_dfr(~mutate(., mu = .$mu %>% unlist(), phi = .$phi %>% unlist(), x = .[, unique(.$var)] %>% unlist()) %>%
              select(mu, phi, x, var)) %>%
    group_by(var, x) %>%
    summarize(pv = rbeta(n = 1000, shape1 = mu * phi, shape2 = (1 - mu)*phi)) %>%
    summarize(qmid = median(pv),
              qmin = quantile(pv, probs = 0.025),
              qmax = quantile(pv, probs = 0.975)) %>%
    return()
}


get_lr = function(...){
  bind_rows(
    # Compare against each
    lmtest::lrtest(m1,m2,m3,m4) %>%
      broom::tidy() %>%
      select(statistic, p.value),
    # Compare against model 4
    lmtest::lrtest(m4,m5,m6) %>%
      broom::tidy() %>%
      select(statistic, p.value),
    # Compare against model 4
    lmtest::lrtest(m4,m7,m8) %>%
      broom::tidy() %>%
      select(statistic, p.value))  %>%
    filter(!is.na(statistic)) %>%
    mutate(statistic = round(statistic, 1),
           p.value = gtools::stars.pval(p.value),
           label = paste(statistic, p.value, sep = "")) %>%
    with(label) %>% c(" - ", .) %>%
    return()
}

r2 = function(x){
  broom::glance(x)$pseudo.r.squared %>% round(2) %>%
    return()
}




# Get average traits
myconstants = list(
  "group" = "baseline",
  "type" = "baseline",

group_by(setx, type, poi_id, id) %>%
  get_fun() %>%
  ungroup() %>%
