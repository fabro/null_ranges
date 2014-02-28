## Carlos López-Natarén (@natorro) & Fabricio Villalobos (@fabro)

## Function: generate n (n = simulations of size 'size_vector') random ranges
## arguments as in rangeMap and rangePam

## NOTE: exclusive to Fabricio's data: a squared matrix of 175 x 175 cells. Change it according to your data 

randomRichness<-function(data_file, vector_of_range_sizes, sims){

  for (i in 1:sims){
  
  simranges <- length (vector_of_range_sizes)
  
  random_richness_matrix <- matrix (0, nrow=175, ncol=175)
  
    for (j in 1:simranges) {                    

    temporary_matrix <- randomMap(data_file,vector_of_range_sizes[j])
    random_richness_matrix <- random_richness_matrix + temporary_matrix
    temporary_matrix <- matrix (0, nrow=175, ncol=175)
    }
  
  write.table(random_richness_matrix, paste('random_matrix_', i, '.txt', sep=""), sep="\t") 
  
  }
  
}
