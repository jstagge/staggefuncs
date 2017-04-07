# *------------------------------------------------------------------
# | FUNCTION NAME: rot90_times
# | FILE NAME: rot90_times.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        A - Matrix or array with data to be rotated
# |                times - the number of times to rotate data (clockwise)
# |                
# |     Out:       A - The rotated array
# | 
# |     Desc:      Accepts an array or matrix and rotates it 90 degrees, multiple times.
# |					The default is clockwise, but, you can use times=-1 to rotate counter-clockwise
# |                
# *------------------------------------------------------------------


rot90_times <- function(A, times = 1) {
	### Calculate the number of times to rotate.  Find the remainder after dividing by 4
	base_rotation <- 4 * sign(times)
	times <- times%%base_rotation
	### Catch for negative values.  If negative, subtract from 4
	if (times < 0) {times <- 4 + times}
	
	### If the rotations are divisible by 4, return original
	if (times == 0) {
	 A <- A 
	### Otherwise, rotate the number of times requested
	} else {
		for (j in seq(1,times)){
		A <- rot90(A)
		}
	}
	
return(A)
}



# *------------------------------------------------------------------
# | FUNCTION NAME: rot90
# | FILE NAME: rot90_times.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        A - Matrix or array with data to be rotated
# |                
# |     Out:       A - The rotated array
# | 
# |     Desc:      Accepts an array or matrix and rotates it 90 degrees.
# |                
# *------------------------------------------------------------------

rot90 <- function(A) {
if (length(dim(A))==3) {
		A <- aperm(A, c(2,1,3))
		A <- A[,rev(seq_len(ncol(A))),]
	} else if (length(dim(A))==2) {
		A <- aperm(A, c(2,1))
		A <- A[,rev(seq_len(ncol(A)))]
	}
return(A)
}


