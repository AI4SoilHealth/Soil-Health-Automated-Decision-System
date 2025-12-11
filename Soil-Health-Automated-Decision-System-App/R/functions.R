# ===============================================================
# functions.R
# ---------------------------------------------------------------
# Contains small, reusable helper functions for the Shiny app.
# This file currently defines a single function: `rescale()`.
# ===============================================================
# ----------------------------------------------------------------
# rescale()
# ----------------------------------------------------------------
# Purpose:
#   Rescale a numeric value x from a known source range 
#   [lower_bound, upper_bound] into a target range 
#   [lower_target, upper_target].
#
#   - Values below lower_bound are clamped to lower_target (or upper_target if invert = TRUE)
#   - Values above upper_bound are clamped to upper_target (or lower_target if invert = TRUE)
#   - If invert = TRUE, the scale is reversed, but clamping behavior remains consistent.
#
# Arguments:
#   x             Numeric vector to transform.
#   lower_bound   Minimum input value corresponding to lower_target.
#   upper_bound   Maximum input value corresponding to upper_target.
#   lower_target  Output value when x = lower_bound (unless invert = TRUE).
#   upper_target  Output value when x = upper_bound (unless invert = TRUE).
#   invert        If TRUE, reverses the scale.
#
# Output:
#   A numeric vector of rescaled values.
# ----------------------------------------------------------------

rescale <- function(x, 
                    lower_bound, upper_bound, 
                    lower_target, upper_target, 
                    invert = FALSE) {
  
  # Compute normalized position of x within [lower_bound, upper_bound]
  # Values < lower_bound or > upper_bound are handled by case_when.
  scaled <- (x - lower_bound) / (upper_bound - lower_bound)
  
  dplyr::case_when(
    
    # --- Clamp low end ---
    x < lower_bound ~ {
      if (invert) upper_target else lower_target
    },
    
    # --- Clamp high end ---
    x > upper_bound ~ {
      if (invert) lower_target else upper_target
    },
    
    # --- Within valid range ---
    TRUE ~ {
      if (invert) {
        # Reverse interpolation
        upper_target - scaled * (upper_target - lower_target)
      } else {
        # Normal interpolation
        lower_target + scaled * (upper_target - lower_target)
      }
    }
  )
}

