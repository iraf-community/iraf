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
	string	temp1, temp2, image, itype
	int	n

	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")

	# Initialize files
	set clobber=yes
	sleep (> obj)
	sleep (> arc)
	sleep (> std)
	set clobber=no

	# Do arcs
	hselect (arcs, "$I,naxis,ctype1", yes, > temp1)
	fd = temp1
	itype = ""
	while (fscan (fd, image, n, itype) != EOF) {
	    if (n==1)
		next
	    n = nscan()
	    if (n > 2 && itype == "MULTISPE")
		next
	    n = strlen (image)
	    if (n > 4 && substr (image, n-3, n) == ".imh")
		image = substr (image, 1, n-4)
	    print (image, >> temp2)
	}
	fd = ""; delete (temp1, verify=no)

	hselect ("@"//temp2, "$I,imagetyp", yes, > temp1)
	delete (temp2, verify=no)
	fd = temp1
	while (fscan (fd, image, itype) != EOF) {
	    if (nscan() < 2)
		itype = "comp"
	    if (itype == "comp" || itype == "COMPARISON" ||
		itype == "comparison" || itype == "COMP")
		print (image, >> arc)
	}
	fd = ""; delete (temp1, verify=no)

	# Do standards
	hselect (standards, "$I,naxis,ctype1", yes, > temp1)
	fd = temp1
	while (fscan (fd, image, n, itype) != EOF) {
	    if (n==1)
		next
	    n = nscan()
	    if (n > 2 && itype == "MULTISPE")
		next
	    n = strlen (image)
	    if (n > 4 && substr (image, n-3, n) == ".imh")
		image = substr (image, 1, n-4)
	    print (image, >> std)
	}
	fd = ""; delete (temp1, verify=no)

	# Do objects
	hselect (objects, "$I,naxis,ctype1", yes, > temp1)
	fd = temp1
	while (fscan (fd, image, n, itype) != EOF) {
	    if (n==1)
		next
	    n = nscan()
	    if (n > 2 && itype == "MULTISPE")
		next
	    n = strlen (image)
	    if (n > 4 && substr (image, n-3, n) == ".imh")
		image = substr (image, 1, n-4)
	    print (image, >> temp2)
	}
	fd = ""; delete (temp1, verify=no)

	hselect ("@"//temp2, "$I,imagetyp", yes, > temp1)
	delete (temp2, verify=no)
	fd = temp1
	while (fscan (fd, image, itype) != EOF) {
	    if (nscan() < 2)
		itype = "object"
	    if (itype == "object" || itype == "OBJECT")
		print (image, >> obj)
	    else if (itype == "comp" || itype == "COMPARISON" ||
		itype == "comparison" || itype == "COMP")
		print (image, >> arc)
	}
	fd = ""; delete (temp1, verify=no)

	rename (arc, temp1)
	sort (temp1, column=0, ignore=yes, numeric=no, reverse=no) |
	unique ("STDIN", > arc)
end
