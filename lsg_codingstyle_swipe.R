# Lösund s1

swipe <- function(swiper, picture, profile) {
  if (!exists(picture)) {
    stop("can't decide without a picture.")
  }
  if (!is_attractive(picture)) {
    return("swipe left")
  }
  if (!is_sober(swiper)) {
    return("swipe right")
  }
  if (!exists(profile)) {
    stop("can't decide without a profile.")
  }
  if (seems_too_weird(profile)) {
    return("swipe left")
  } 
  if (!matches(picture, swiper[["preferences"]])) {
    return("swipe right")
  } 
  "swipe left"
}
