# MVMUL -- Multply a matrix (left-hand side) by a one dimensional vector
# (right-hand side) and return the resultant vector.

procedure mvmul (matrix, maxdim, dim, vector, result)

real	matrix [maxdim, maxdim]	# Input matirx
int	maxdim			# Maximum size of input matrix
int	dim			# Dimension of matrix and vectors
real	vector[maxdim]		# Input vector
real	result[maxdim]		# Output vector

double	sum
int	i, j

begin

	do i = 1, dim {

	    sum = 0.0
	    do j = 1, dim
		sum = sum + dble (matrix[i,j]) * dble(vector[j])

	    result (i) = sum
	}

end
