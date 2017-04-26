# SUBPROC -- Benchmark the process control facilities.

procedure subproc (nreps)

int	nreps		{ prompt = "number of repetitions" }
int	i

begin
	time; print ("======= begin ========")

	for (i=nreps;  i > 0;  i-=1) {
	    prcache ("imheader")
	    flprcache ("imheader")
	    time()
	}

	print ("=======  end  ========"); time
end
