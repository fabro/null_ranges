## Fabricio Villalobos

## Function: generate a random presence-absence matrix for n species in the domain. 
## It uses randomMap function (needs to be working/loaded in R)
## arguments as in randomMap, but with a vector of range sizes to be simulated

randomPam <- function(data_file,vector_of_range_sizes){
    
pam_table <- matrix(0, nrow=175*175, ncol=length(vector_of_range_sizes))
    
    for (i in 1:length(vector_of_range_sizes)){
    
    row_index <- 1
    
    one_random_map <- random_map(data_file,vector_of_range_sizes[i])
    
      for(j in 1:175){
      
        for(k in 1:175){
          
          pam_table[row_index,i] <- one_random_map[j,k]
          row_index <- row_index + 1
          }
        }
      }
      
      pam_table
      
    }
    