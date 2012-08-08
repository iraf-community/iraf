# SETINDEX -- Initialize an index array

procedure setindex (index, len)

int	index[ARB]	# o: index rray
int	len		# i: array length
#--
int	i

begin
	do i = 1, len
	    index[i] = i
end
