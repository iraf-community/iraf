# SGETSPEC -- Get spectra which are not multispec and strip ".imh" extension

procedure sgetspec (objects, arcs, standards, obj, arc, std)

string	objects			{prompt="List of object images"}
string	arcs			{prompt="List of arc images"}
string	standards		{prompt="List of standard images"}
file	obj			{prompt="File object images"}
file	arc			{prompt="File of arc images"}
file	std			{prompt="File of standard images"}
struct	*fd

begin
	string	temp, image, ctype, itype
	int	n

	temp = mktemp ("tmp$iraf")

	# Initialize files
	sleep (> obj)
	sleep (> arc)
	sleep (> std)

	# Do arcs
	hselect (arcs, "$I,naxis,ctype1,imagetyp", yes, > temp)
	fd = temp
	ctype = ""
	itype = ""
	while (fscan (fd, image, n, ctype,itype) != EOF) {
	    if (n==1)
		next
	    n = nscan()
	    if (n > 2 && ctype == "MULTISPE")
		next
	    if (n < 4)
		itype = "comp"
	    n = strlen (image)
	    if (n > 4 && substr (image, n-3, n) == ".imh")
		image = substr (image, 1, n-4)
	    if (itype == "comp" || itype == "COMPARISON")
		print (image, >> arc)
	}
	fd = ""; delete (temp, verify=no)

	# Do standards
	hselect (standards, "$I,naxis,ctype1", yes, > temp)
	fd = temp
	ctype = ""
	itype = ""
	while (fscan (fd, image, n, ctype) != EOF) {
	    if (n==1)
		next
	    n = nscan()
	    if (n > 2 && ctype == "MULTISPE")
		next
	    n = strlen (image)
	    if (n > 4 && substr (image, n-3, n) == ".imh")
		image = substr (image, 1, n-4)
	    print (image, >> std)
	}
	fd = ""; delete (temp, verify=no)

	# Do objects
	hselect (objects, "$I,naxis,ctype1,imagetyp", yes, > temp)
	fd = temp
	while (fscan (fd, image, n, ctype, itype) != EOF) {
	    if (n == 1)
		next
	    n = nscan()
	    if (n > 2 && ctype == "MULTISPE")
		    next
	    if (n < 4)
		itype = "object"
	    n = strlen (image)
	    if (n > 4 && substr (image, n-3, n) == ".imh")
		image = substr (image, 1, n-4)
	    if (itype == "object" || itype == "OBJECT")
		print (image, >> obj)
	    else if (itype == "comp" || itype == "COMPARISON")
		print (image, >> arc)
	}
	fd = ""; delete (temp, verify=no)
end
