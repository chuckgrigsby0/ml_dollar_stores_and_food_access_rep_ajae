# # Example factor levels (for illustration)
# levels_vector <- c("0", "1", "2", "3", "(3,5]", "> 5", "4", "> 4", "\u2264 -1")

# Custom sort function for complex level sorting
sort_complex_levels <- function(levels) {
  # Extract numeric values from levels for sorting
  nums <- map_dbl(levels, function(x) {
    if (grepl("^\\(.*\\]$", x)) {
      return(as.numeric(gsub("\\D", "", strsplit(x, ",")[[1]][1])))
    } else if (grepl("^>", x)) { # | grepl("d", x)
      return(as.numeric(gsub("\\D", "", x))+1)
    } else if (grepl("\u2264", x))  {
      return(-1)
    } else { 
      return(as.numeric(x)) }
  })
  
  # # Handle special symbols (e.g., "d -1" becomes very small, "> 5" becomes large)
  # special <- as.numeric(sapply(levels_vector, function(x) {
  #   if (x == "\u2264 -1") {
  #     return(-Inf)
  #   } else if (grepl("^>", x)) {
  #     return(Inf)
  #   } else {
  #     return(0)
  #   }
  # }))
  
  # Order by numeric value first, then handle special cases
  order_levels <- order(nums)
  
  return(levels[order_levels])
}

