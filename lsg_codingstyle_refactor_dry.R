# Lösung s2

rnorm_sd_mean_conf <- function(quadratic = FALSE) {
  
  # test if eval_half_with is logical
  assert_logical(quadratic)
  
  # sampling 100 NV(0,1) random variable
  sample_rnorm <- rnorm(100)
  
  if (quadratic) {
    sample_rnorm <- sample_rnorm^2
  }
    
  # Calculating mean and sd
  mean_sample <- mean(sample_rnorm); 
  sd_sample <- sd(sample_rnorm);
  
  # Calculating confidence intervals
  n <- length(sample_rnorm)
  
  # Calculate half_width
  half_width <- 1.96 * sd_sample / sqrt(n)
  
  # Calculate confidence
  confidence_sample <- c(mean_sample - half_width, 
                    mean_sample + half_width)
  
  
  return(c(mean_sample, sd_sample, confidence_sample))
  
}


 x <- rnorm_sd_mean_conf(FALSE)
 y <- rnorm_sd_mean_conf(FALSE)
 z <- rnorm_sd_mean_conf(FALSE)
 v <- rnorm_sd_mean_conf(TRUE)
