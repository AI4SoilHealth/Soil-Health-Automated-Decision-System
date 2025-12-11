rescale <- function(x, 
                    lower_bound, upper_bound, 
                    lower_target, upper_target, 
                    invert = FALSE) {
  scaled <- (x - lower_bound) / (upper_bound - lower_bound)
  dplyr::case_when(
    x < lower_bound ~ {
      if (invert) upper_target else lower_target
    },
    x > upper_bound ~ {
      if (invert) lower_target else upper_target
    },
    TRUE ~ {
      if (invert) {
        upper_target - scaled * (upper_target - lower_target)
      } else {
        lower_target + scaled * (upper_target - lower_target)
      }
    }
  )
}

