## Carlos López-Natarén (@natorro) & Fabricio Villalobos(@fabro) (based on VisualBasic code by Héctor T. Arita)
## Villalobos & Arita. 2010. GEB. doi:10.1111/j.1466-8238.2009.00503.x

## Function to generate one random cohesive (contiguous) range
## this function is implemented within 'randomRichness' & 'randomRichnessDisplay' functions
## arguments: data_file = the path to the squared matrix representing the geographic domain of interest (1's on "land" and 0's on "sea"); range_size = the desire range size to simulate

## NOTE: exclusive to Fabricio's data: a squared matrix of 175 x 175 cells. Change it according to your data 

randomMap<-function(data_file,range_size){

# Read the table to create the matrix needed

mexico_matrix<-read.table(data_file,header=T)

# Let's generate a matrix full of zeroes
random_matrix <- matrix(0, nrow=175, ncol=175)

# Size of range (area of distribution)           
# range_size <- 500

# Let's choose initial point

initial_point_passed <- FALSE
initial_point <- sample(175, 2)	

while (initial_point_passed == FALSE){ 
  if (mexico_matrix[initial_point[1], initial_point[2]] == 0)
    initial_point <- sample(175, 2) else {
      random_matrix[initial_point[1], initial_point[2]] <- 1
      initial_point_passed <- TRUE 
    } 
}

# Define vectors x_list and y_list 

x_list <- matrix(0, nrow=range_size, ncol=1)
y_list <- matrix(0, nrow=range_size, ncol=1)

x_list[1] <- initial_point[1]
y_list[1] <- initial_point[2]

# Range counter
range_counter <- 1
flag <- 0

while (sum(random_matrix) < range_size){
	cuadro_aleatorio <- floor((runif(1) * range_counter ) + 1)
	flag <- 0
	while (flag == 0) {
		
		if (mexico_matrix[x_list[cuadro_aleatorio] - 1, y_list[cuadro_aleatorio]] == 1		||
			mexico_matrix[x_list[cuadro_aleatorio] + 1, y_list[cuadro_aleatorio]] == 1		||
			mexico_matrix[x_list[cuadro_aleatorio],		y_list[cuadro_aleatorio] + 1] == 1		||
			mexico_matrix[x_list[cuadro_aleatorio],		y_list[cuadro_aleatorio] - 1] == 1) {
			
			aleatorio <- sample(c(1, 2, 3, 4, 5, 6, 7, 8), 1)
			
			if (aleatorio == 1) {
				if(mexico_matrix[x_list[cuadro_aleatorio] + 1 , y_list[cuadro_aleatorio] ] == 1 && random_matrix[x_list[cuadro_aleatorio] + 1 , y_list[cuadro_aleatorio] ]== 0) {
					random_matrix [x_list[cuadro_aleatorio] + 1 , y_list[cuadro_aleatorio] ] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] + 1
					y_list[range_counter] <- y_list[cuadro_aleatorio] 

					flag <- 1
				}
			}
			
			if (aleatorio == 2) {
				if(mexico_matrix[x_list[cuadro_aleatorio] - 1, y_list[cuadro_aleatorio] ] == 1 && random_matrix[x_list[cuadro_aleatorio] - 1, y_list[cuadro_aleatorio] ]== 0) {
					random_matrix[x_list[cuadro_aleatorio] - 1, y_list[cuadro_aleatorio] ] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] - 1
					y_list[range_counter] <- y_list[cuadro_aleatorio] 

					flag <- 1
				}
			}

			if (aleatorio == 3) {
				if(mexico_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] + 1] == 1 && random_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] + 1]== 0) {
					random_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] + 1] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio]
					y_list[range_counter] <- y_list[cuadro_aleatorio] + 1

					flag <- 1
				}
			}
			
			if (aleatorio == 4) {
				if(mexico_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] - 1] == 1 && random_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] - 1] == 0) {
					random_matrix[x_list[cuadro_aleatorio], y_list[cuadro_aleatorio] - 1] <- 1
					range_counter <- range_counter +1
					x_list[range_counter] <- x_list[cuadro_aleatorio]
					y_list[range_counter] <- y_list[cuadro_aleatorio] - 1

					flag <- 1
				}
			}



			if (aleatorio == 5) {
				if(mexico_matrix[x_list[cuadro_aleatorio] +1, y_list[cuadro_aleatorio]+1 ] == 1 && random_matrix[x_list[cuadro_aleatorio] +1, y_list[cuadro_aleatorio]+1 ] == 0) {
					random_matrix[x_list[cuadro_aleatorio] +1, y_list[cuadro_aleatorio]+1 ] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] +1
					y_list[range_counter] <- y_list[cuadro_aleatorio] +1

					flag <- 1
				}
			}

			if (aleatorio == 6) {
				if(mexico_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio] -1] == 1 && random_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio] -1] == 0) {
					random_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio] -1] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] - 1
					y_list[range_counter] <- y_list[cuadro_aleatorio] - 1

					flag <- 1
				}
			}

			if (aleatorio == 7) {
				if(mexico_matrix[x_list[cuadro_aleatorio]+1, y_list[cuadro_aleatorio] -1] == 1 && random_matrix[x_list[cuadro_aleatorio]+1, y_list[cuadro_aleatorio] -1] == 0) {
					random_matrix[x_list[cuadro_aleatorio]+1, y_list[cuadro_aleatorio] -1] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] + 1
					y_list[range_counter] <- y_list[cuadro_aleatorio] - 1

					flag <- 1
				}
			}
			if (aleatorio == 8) {
				if(mexico_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio]+1 ] == 1 && random_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio]+1 ] == 0) {
					random_matrix[x_list[cuadro_aleatorio]-1, y_list[cuadro_aleatorio]+1 ] <- 1
					range_counter <- range_counter + 1
					x_list[range_counter] <- x_list[cuadro_aleatorio] - 1
					y_list[range_counter] <- y_list[cuadro_aleatorio] + 1

					flag <- 1
				}
			}
			flag <- 1
		}
	}
}


random_matrix 

} 














