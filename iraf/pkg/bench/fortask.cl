# FORTASK -- Execute a foreign task repeatedly.

procedure fortask (nreps)

int	nreps		{ prompt = "number of repetitions" }
int	i

begin
	time; print ("======= begin ========")

	for (i=nreps;  i > 0;  i-=1)
	    !rmbin

	print ("=======  end  ========"); time
end
