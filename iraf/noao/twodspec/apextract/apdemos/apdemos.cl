procedure apdemos (demo)

int	demo		{prompt="Demo number"}

begin
	int	demonum
	file	demofile

	if ($nargs == 0)
	    type ("apdemos$apdemos.men")
	demonum = demo
	demofile = "apdemos$apdemo" // demonum // ".cl"
	if (access (demofile))
	    cl (< demofile)
	else
	    error (1, "Invalid demo number " // demonum)
end
