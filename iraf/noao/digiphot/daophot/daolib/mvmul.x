# MVMUL -- Multply a matrix (left-hand side) by a one dimensional vector
# (right-hand side) and return the resultant vector.

procedure mvmul (matrix, maxdim, dim, vector, result)

real	matrix [maxdim, maxdim]	# input matrix
int	maxdim			# maximum size of input matrix
int	dim			# dimension of matrix and vectors
real	vector[maxdim]		# input vector
real	result[maxdim]		# iutput vector

double	sum
int	i, j

begin
	do i = 1, dim {
	    sum = 0.0
	    do j = 1, dim
		sum = sum + double (matrix[j,i]) * double(vector[j])
	    result[i] = sum
	}

end


# DMVMUL -- Multply a matrix (left-hand side) by a one dimensional vector
# (right-hand side) and return the resultant vector.

procedure dmvmul (matrix, maxdim, dim, vector, result)

double	matrix [maxdim, maxdim]	# input matrix
int	maxdim			# maximum size of input matrix
int	dim			# dimension of matrix and vectors
double	vector[maxdim]		# input vector
double	result[maxdim]		# iutput vector

double	sum
int	i, j

begin
	do i = 1, dim {
	    sum = 0.0d0
	    do j = 1, dim
		sum = sum + (matrix[j,i] * vector[j])
	    result[i] = sum
	}

end
