######## location of % shares in stacked figures

locate_shares <- function(data,grouping,years){

  shares <- data %>%
    arrange_at(vars(`year`,all_of(grouping))) %>%
    mutate(location=value/2)
  
  z = nrow(unique(shares[,grouping]))
  
  for (j in seq(0,z*(years-1),z)) {
    # for every region
    for (i in 1:z) {
      if (i != z) {
        shares$location[i+j] = shares$location[i+j] + sum(shares$value[i+1+j:(z-i+j-1)])
        #shares$location[i] = shares$location[i] + sum(shares$value[i+1:(z-i))])
        
      }
    }
  }

  return(shares)
}


