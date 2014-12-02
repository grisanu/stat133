#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  
  stopifnot(r > 0 &
            c > 0 &
            p > 0 & p < 1);
  
  # round in case not integer
  r = round(r);
  c = round(c);
  
  if (r==1 & c==1){
    m = matrix(sample(seq(0,2,1), 1, prob = c(1-p,p/2,p/2)),1,1);
    return(m)
  }
  
  # no of cars
  total_cars = round(p*r*c);
  no_of_cars = round(p*r*c/2);
  
  # total no of blocks
  no_of_blocks = r*c;
  
  if (no_of_cars*2 < total_cars) {
    diff_cars = total_cars - (no_of_cars*2);
    print(diff_cars)
    # vector of equal number of car 1 and car 2
    cars = c(rep(1, no_of_cars), rep(2, no_of_cars), sample(1:2, diff_cars, replace = T));
  } else {
    cars = c(rep(1, no_of_cars), rep(2, no_of_cars));
  }
  
  # initialise matrix of plotting, full of zeros
  m = matrix(rep(0, no_of_blocks), nrow = r, ncol = c);
  
  # list of all possible coordinates
  all_coord = matrix(c(rep(1:r, each = c), rep(1:c, r)), nrow = no_of_blocks, ncol = 2)
  
  # sample desired number of coordinates
  coord_idx = sample(1:no_of_blocks, size = total_cars, replace = F);
  
  # put into plotting matrix
  for (i in 1:total_cars) {
    m[all_coord[coord_idx[i],][1], all_coord[coord_idx[i],][2]] = cars[i];
  }
  
  return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  stopifnot(class(m) == "matrix");
  
  # define boundary
  row_max = dim(m)[1];
  col_max = dim(m)[2];

  # grid.new
  grid.new = FALSE;
############################## Using WHICH ##############################

############## RED -> east

  #coordinates of RED cars
  red_coord = which(m == 1, arr.ind = T);
  #coordinated of BLUE cars
  blue_coord = which(m == 2, arr.ind = T);

#   # no of red cars
#   no_of_red_cars = dim(red_coord)[1];
#   # no of blue cars
#   no_of_blue_cars = dim(blue_coord)[1];
#   # total no of cars
#   total_no_of_cars = no_of_red_cars + no_of_blue_cars;

  # next space coord matrix
  next_space = red_coord;
    next_space[,2] = red_coord[,2] +1;

    #replace out of bounds COL -RED
    next_space[,2][next_space[,2] > col_max] = 1;
#     print(next_space);

#       #find out of bounds
#       out_of_bounds_indx = which(next_space[,2] > col_max);
#       #replace out of bounds col with periodic boundary conditions
#       next_space = matrix(c(next_space[, 1], replace(next_space[, 2], out_of_bounds_indx, 1)), nrow = no_of_red_cars, ncol = 2);
  
  # index of car that can move
  moveable_red_index = which((m[next_space] == 0) == T);
#   print(moveable_red_index);

#   # red cars coord that must be removed
#   red_coord = red_coord[moveable_red_index, ];
#     
#   # next red cars coord
#   next_space = next_space[moveable_red_index, ];
  
# # matrix of movalbe RED cars - matrix of next space
#     # vector of matrix of movalbe cars 
#     cont_vector_moveable_red_cars_coord = next_space[m[next_space] == 0];
#       #for making matrix
#       no_rows_movalbe_red_cars_matrix = length(cont_vector_moveable_red_cars_coord)/2;
#     #making matrix
#     movable_red_coords = matrix(cont_vector_moveable_red_cars_coord, nrow = no_rows_movalbe_red_cars_matrix, ncol = 2);
#   
#   # make old red car matrix
#   old_red_coords = matrix(cont_vector_old_red_coord, nrow = no_rows_movalbe_red_cars_matrix, ncol = 2);  
# 
  
# grid.new logical
  if (length(moveable_red_index) > 0) {
    grid.new = TRUE;
  }
#   print(m)

  # move RED cars
  if (length(moveable_red_index) > 1){
    m[next_space[moveable_red_index, ]] = 1;
    # remove old RED cars
    m[red_coord[moveable_red_index, ]] = 0;
  } else {
    m[next_space[moveable_red_index, ][1], next_space[moveable_red_index, ][2]] = 1;
    # remove old RED cars
    m[red_coord[moveable_red_index, ][1], red_coord[moveable_red_index, ][2]] = 0;
  }
#   print(m)

############## BLUE -> north

  # next space coord matrix
  next_space_blue = blue_coord;
    next_space_blue[,1] = blue_coord[,1] -1;
  
  #replace out of bounds ROW - BLUE
  next_space_blue[,1][next_space_blue[,1] < 1] = row_max;
#   print(next_space_blue)  

#   # next space BLUE car matrix
#   next_space_blue = blue_coord + matrix(c(rep(-1, no_of_blue_cars), rep(0, no_of_blue_cars)), nrow = no_of_blue_cars, ncol = 2);
#     #find out of bounds
#     out_of_bounds_indx = which(next_space_blue[,1] < 1);
#     #replace out of bounds ROW with periodic boundary conditions
#     next_space_blue = matrix(c(replace(next_space_blue[, 1], out_of_bounds_indx, row_max), next_space_blue[, 2]), nrow = no_of_blue_cars, ncol = 2);

#   # remove BLUE car from old position
#   cont_vector_old_blue_coord = blue_coord[m[next_space_blue] == 0];  
# 
#   # matrix of movalbe BLUE cars
#     # vector of matrix of movalbe cars 
#     cont_vector_moveable_blue_cars_coord = next_space_blue[m[next_space_blue] == 0];
#       #for making matrix
#       no_rows_movalbe_blue_cars_matrix = length(cont_vector_moveable_blue_cars_coord)/2;
#     #making matrix
#     movable_blue_coords = matrix(cont_vector_moveable_blue_cars_coord, nrow = no_rows_movalbe_blue_cars_matrix, ncol = 2);
# 
#   # make old BLUE car matrix
#   old_blue_coords = matrix(cont_vector_old_blue_coord, nrow = no_rows_movalbe_blue_cars_matrix, ncol = 2);

  # index of car that can move
  moveable_blue_index = which((m[next_space_blue] == 0) == T);
#   print(moveable_blue_index);

  # grid.new logical
  if (length(moveable_blue_index) > 0) {
    grid.new = TRUE;
  }
#   print(m)

  # move BLUE cars
  if (length(moveable_blue_index) > 1){
    m[next_space_blue[moveable_blue_index, ]] = 2;
    # remove old BLUE cars
    m[blue_coord[moveable_blue_index, ]] = 0;
  } else {
    m[next_space_blue[moveable_blue_index, ][1], next_space_blue[moveable_blue_index, ][2]] = 2;
    # remove old BLUE cars
    m[blue_coord[moveable_blue_index, ][1], blue_coord[moveable_blue_index, ][2]] = 0;
  }
#   print(m)

#   # loop to place red car in space => SEPERATE for red and blue cars
#   for (i in 1:total_no_of_cars) {
#     if (m[next_space_blue[i, 1], next_space_blue[i, 2]] == 0 & i <= no_of_red_cars) { #unoccupied and RED
#       mred[next_space_blue[i, 1], next_space_blue[i, 2]] = 1; 
#     } else if (m[next_space_blue[i, 1], next_space_blue[i, 2]] != 0 & i <= no_of_red_cars){ #occupied and RED
#       mred[red_coord[i, 1], red_coord[i, 2]] = 1;
#     } else if (i > no_of_red_cars) {
#       mblue[blue_coord[i-no_of_red_cars, 1], blue_coord[i-no_of_red_cars, 2]] = 2;
#     }
#   }
############################## Using WHICH ##############################

############################## For LOOP ##############################
#   # move RED -> EAST
#   for (row in 1:row_max) {
#     for (colm in 1:col_max) {
#       
#       # periodic boundary conditons
#       if (colm+1 > col_max) {
#         colnew = 1;
#       } else {
#         colnew = colm+1;
#       }
#       
#       # move RED east
#       if (m[row, colm] == 1) {
#         if (m[row, colnew] == 0) { # unoccupied
#           mnew[row, colnew] = 1;
#           
#           has_changed = has_changed + 1;
#         } else { # occupied
#           mnew[row, colm] = 1;
#         } 
#       } else if (m[row, colm] == 2) {
#         mnew[row, colm] = 2;
#       }
#     }
#   }
#  
  
#   # update current matrix
#   m = mnew;
#   mnew = mnew*0;

#   # then move BLUE -> NORTH
#   for (row in 1:row_max) {
#     for (colm in 1:col_max) {
#       
#       # periodic boundary conditons
#       if (row-1 <= 0) {
#         rownew = row_max;
#       } else {
#         rownew = row-1;
#       }
#       
#       # move BLUE north
#       if (m[row, colm] == 2) {
#         if (m[rownew, colm] == 0) { # unoccupied
#           mnew[rownew, colm] = 2;
#           
#           has_changed = has_changed + 1;
#         } else { # occupied
#           mnew[row, colm] = 2;
#         }
#       } else if (m[row, colm] == 1) {
#         mnew[row, colm] = 1;
#       }
#     }
#   }
############################## For LOOP ##############################
  return(list(m, grid.new)) #grid.new = T if systenm has CHANGED (ie. NOT gridlock)
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p, max_steps = 15000, print_every = 2000, should_print_step = TRUE){
#timer
start_time = proc.time();
  
  m = bml.init(r,c,p);

  #imaging
  img1 = image(t(apply(m,2,rev)), col = c('white','red', 'blue'));
        
  # initialise variables
  curr_step = 0;
  gridlock = FALSE;
  hit_max_step = FALSE;
  
  # run until either gridlock or hit max_steps
  while ((gridlock == FALSE) & (curr_step < max_steps)) {
      #record output of bml.step
        step_output = bml.step(m);
    
    m = step_output[[1]];
    #update gridlock status
    gridlock = !step_output[[2]];
    
    # update step count
    curr_step = curr_step +1;
    
    # periodic printing
    if ((curr_step%%print_every == 0) & (should_print_step == TRUE)) {
      print(c("Currently, step number =", curr_step), quote = FALSE);
    }
  }
  
  if (curr_step >= max_steps) {
    hit_max_step = TRUE;
  }

#timer
end_time = (proc.time() - start_time)[3];
  
  #imaging
  img2 = image(t(apply(m,2,rev)), col = c('white','red', 'blue'));
        
  return(list(m, curr_step, hit_max_step, gridlock, end_time, img1,img2))
}
