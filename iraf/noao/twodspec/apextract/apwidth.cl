# APWIDTH -- Script to report widths from APALL database files.
# The input is the image name and database directory.
# The output is image name, aperture number, x center, y center, and width.
#
# To install this script copy it to a directory, such as your IRAF login
# directory "home$" in this example.  Define the task in your loginuser.cl
# or login.cl with
#
#    task apwidth = home$apwidth.cl
#
# Note that you can substitute some other path to the script if desired.

procedure apwidth (image)

file	image			{prompt="Image name"}
file	database = "database"	{prompt="Database"}

begin
	file	dbfile
	string	im
	int	ap, axis
	real	xc, yc, aplow1, aphigh1, aplow2, aphigh2, width

	# Form database name from the database and image names.
	dbfile = database // "/ap" // image

	# Check that the database file actually exists.
	if (!access(dbfile))
	    error (1, "Databse file not found (" // dbfile // ")")

	# Loop through each line of the database file.  Extract information
	# and print the output line when the axis keyword is found.  This
	# assumes the aperture limits are read before the axis.

	axis = INDEF
	list = dbfile
	while (fscan (list, line) != EOF) {
	    if (fscan (line, s1) < 1)
		next
	    if (s1 == "begin")
		i = fscan (line, s1, s1, im, ap, xc, yc)
	    else if (s1 == "low")
		i = fscan (line, s1, aplow1, aplow2)
	    else if (s1 == "high")
		i = fscan (line, s1, aphigh1, aphigh2)
	    else if (s1 == "axis")
		i = fscan (line, s1, axis)

	    if (axis != INDEF) {
		if (axis == 1)
		    width = aphigh1 - aplow1
		else
		    width = aphigh2 - aplow2
		printf ("%s %2d %8.4g %8.4g %8.4g\n", im, ap, xc, yc, width)
		axis = INDEF
	    }
	}
	list = ""
end
