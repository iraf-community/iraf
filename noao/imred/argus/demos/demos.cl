# DEMOS -- Run specified demo provided a demo file exists.

procedure demos (demoname)

file	demoname	{prompt="Demo name"}

begin
	file	demo, demofile

	if ($nargs == 0 && mode != "h")
	    type ("demos$demos.men")
	demo = demoname
	demofile = "demos$" // demo // ".cl"
	if (access (demofile))
	    cl (< demofile)
	else
	    error (1, "Unknown demo " // demo)
end
