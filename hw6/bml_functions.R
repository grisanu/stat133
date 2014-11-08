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
  
  # no of cars
  total_cars = round(p*r*c);
  no_of_cars = round(total_cars/2);
  
  # total no of blocks
  no_of_blocks = r*c;
  
  # vector of equal number of car 1 and car 2
  cars = c(rep(1, no_of_cars), rep(2, no_of_cars));
  
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

  # changed variable
  has_changed = 0;
  
  # make mnew
  mnew = m*0;
  
  # define boundary
  row_max = dim(m)[1];
  col_max = dim(m)[2];

############################## Using WHICH ##############################

# RED -> east

  #coordinates of red cars
  red_coord = which(m == 1, arr.ind = T);
  # no of red cars
  no_of_red_cars = dim(red_coord)[1];
  # next space matrix
  next_space = red_coord + matrix(c(rep(0, no_of_red_cars), rep(1, no_of_red_cars)), nrow = no_of_red_cars, ncol = 2);
  
  #find out of bounds
  out_of_bounds_indx = which(next_space[,2] > col_max);

  #replace out of bounds with periodic boundary conditions
  replace(next_space[])
# update current matrix
m = mnew;
mnew = mnew*0;

  # BLUE -> north

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

  # check if matrix has changed
  if (has_changed > 0){ # changed
    grid.new = TRUE;
  } else { #gridlock
    grid.new = FALSE;
  }
  
  return(list(mnew, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p, max_steps = 10000, print_every = 1000, should_print_step = TRUE){
  m = bml.init(r,c,p);
  
  # initialise variables
  curr_step = 0;
  gridlock = FALSE;
  
  # run until either gridlock or hit max_steps
  while ((gridlock = FALSE) | (curr_step < max_steps)) {
    step_output = bml.step(m);
    
    mnew = step_output[[1]];
    gridlock = !step_output[[2]];
    
    # update step count
    curr_step = curr_step +1;
    
    # update input matrix
    m = mnew;
    
    # periodic print
    if ((curr_step%%print_every == 0) & (should_print_step == TRUE)) {
      print(c("Currently, step number =", curr_step), quote = FALSE);
    }
    
#     #imaging
#     image(mnew, col = c("white", "red", "blue"));
  }
  
  if (curr_step > max_steps) {
    hit_max_step = TRUE;
  }
  return(list(mnew, curr_step, hit_max_step))
}
