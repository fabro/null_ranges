## Fabricio Villalobos

## Function to generate n random presence-abscence matrices
## works with randomPam and randomMap functions in R
## arguments as in the randomMap and randomPam functions

randomPamSims <- function (data_file, vector_of_range_sizes, sims){

    for (j in 1:sims){

      temporal_matrix <- randomPam(data_file, vector_of_range_sizes)
      write.table(temporal_matrix, paste('random_pam_', j, '.dat', sep=""), sep="\t")
    }
}  
