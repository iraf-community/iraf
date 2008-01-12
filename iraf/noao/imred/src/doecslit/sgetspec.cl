# SGETSPEC -- Get spectra which are CCD processed and not extracted.
# This task also recognizes the arc spectra in the object list and arc table.
# This task also strips the image type extension.

procedure sgetspec (objects, arcs, arctable, standards, obj, arc, std)

string	objects			{prompt="List of object images"}
string	arcs			{prompt="List of arc images"}
file	arctable		{prompt="Arc table"}
string	standards		{prompt="List of standard images"}
file	obj			{prompt="File of object images"}
file	arc			{prompt="File of arc images"}
file	std			{prompt="File of standard images"}
bool	ccdproc			{prompt="Add CCDPROC keyword and continue?",
				 mode="q"}
struct	*fd1, *fd2

begin
	string	imtype, temp, image, itype
	int	n, n1, narcs

	imtype = "." // envget ("imtype")
	n = stridx (",", imtype)
	if (n > 0)
	    imtype = substr (imtype, 1, n-1)
	n1 = strlen (imtype)

	temp = mktemp ("tmp$iraf")

	# Initialize files
	set clobber=yes
	sleep (> obj)
	sleep (> arc)
	sleep (> std)
	set clobber=no

	# Do arcs
	narcs = 0
	sections (arcs, option="fullname", > temp)
	fd1 = temp
	while (fscan (fd1, image) != EOF) {
	    hselect (image, "ccdproc", yes) | scan (itype)
	    if (nscan() == 0) {
		printf ("%s: CCDPROC keyword not found.\n", image)
		printf ("  Either run CCDPROC or add CCDPROC keyword with HEDIT.\n")
		if (!ccdproc)
		    error (1, "Exit")
		hedit (image, "ccdproc", "DOSLIT", add=yes, update=yes,
		    verify=no, show=no)
	    }
	    hselect (image, "wat0_001", yes) | scanf ("system=%s", itype)
	    if (itype == "equispec" || itype == "multispec")
		next
	    hselect (image, "imagetyp", yes) | scan (itype)
	    if (nscan() == 0)
		itype = "comp"
	    if (itype != "comp" && itype != "COMPARISON" &&
		itype != "comparison" && itype != "COMP")
		next
	    n = strlen (image)
	    if (n > n1 && substr (image, n-n1+1, n) == imtype)
		image = substr (image, 1, n-n1)
	    narcs = narcs + 1
	    printf ("%s %02d\n", image, narcs, >> arc)
	}
	fd1 = ""; delete (temp, verify=no)

	# Do arc table.
	if (arctable != "") {
	    fd2 = arctable
	    while (fscan (fd2, image, image) != EOF) {
		if (nscan() != 2)
		    next
		sections (image, option="fullname", > temp)
		fd1 = temp
		while (fscan (fd1, image) != EOF) {
		    hselect (image, "ccdproc", yes) | scan (itype)
		    if (nscan() == 0) {
			printf ("%s: CCDPROC keyword not found.\n", image)
			printf ("  Either run CCDPROC or add CCDPROC keyword with HEDIT.\n")
			if (!ccdproc)
			    error (1, "Exit")
			hedit (image, "ccdproc", "DOSLIT", add=yes, update=yes,
			    verify=no, show=no)
		    }
		    hselect (image, "wat0_001", yes) | scanf ("system=%s", itype)
		    if (itype == "equispec" || itype == "multispec")
			next
		    hselect (image, "imagetyp", yes) | scan (itype)
		    if (nscan() == 0)
			itype = "comp"
		    if (itype != "comp" && itype != "COMPARISON" &&
			itype != "comparison" && itype != "COMP")
			next
		    n = strlen (image)
		    if (n > n1 && substr (image, n-n1+1, n) == imtype)
			image = substr (image, 1, n-n1)
		    narcs = narcs + 1
		    printf ("%s %02d\n", image, narcs, >> arc)
		}
		fd1 = ""; delete (temp, verify=no)
	    }
	}

	# Do standards
	sections (standards, option="fullname", > temp)
	fd1 = temp
	while (fscan (fd1, image) != EOF) {
	    hselect (image, "ccdproc", yes) | scan (itype)
	    if (nscan() == 0) {
		printf ("%s: CCDPROC keyword not found.\n", image)
		printf ("  Either run CCDPROC or add CCDPROC keyword with HEDIT.\n")
		if (!ccdproc)
		    error (1, "Exit")
		hedit (image, "ccdproc", "DOSLIT", add=yes, update=yes,
		    verify=no, show=no)
	    }
	    hselect (image, "wat0_001", yes) | scanf ("system=%s", itype)
	    if (itype == "equispec" || itype == "multispec")
		next
	    n = strlen (image)
	    if (n > n1 && substr (image, n-n1+1, n) == imtype)
		image = substr (image, 1, n-n1)
	    print (image, >> std)
	}
	fd1 = ""; delete (temp, verify=no)

	# Do objects
	sections (objects, option="fullname", > temp)
	fd1 = temp
	while (fscan (fd1, image) != EOF) {
	    hselect (image, "ccdproc", yes) | scan (itype)
	    if (nscan() == 0) {
		printf ("%s: CCDPROC keyword not found.\n", image)
		printf ("  Either run CCDPROC or add CCDPROC keyword with HEDIT.\n")
		if (!ccdproc)
		    error (1, "Exit")
		hedit (image, "ccdproc", "DOSLIT", add=yes, update=yes,
		    verify=no, show=no)
	    }
	    hselect (image, "wat0_001", yes) | scanf ("system=%s", itype)
	    if (itype == "equispec" || itype == "multispec")
		next
	    hselect (image, "imagetyp", yes) | scan (itype)
	    if (nscan() == 0)
		itype = "object"

	    n = strlen (image)
	    if (n > n1 && substr (image, n-n1+1, n) == imtype)
		image = substr (image, 1, n-n1)
	    if (itype == "object" || itype == "OBJECT")
		print (image, >> obj)
	    else if (itype == "comp" || itype == "COMPARISON" ||
		itype == "comparison" || itype == "COMP") {
		narcs = narcs + 1
		printf ("%s %02d\n", image, narcs, >> arc)
	    }
	}
	fd1 = ""; delete (temp, verify=no)

	if (narcs > 0) {
	    sort (arc, column=0, ignore=yes, numeric=no, reverse=no, > temp)
	    delete (arc, verify=no)
	    rename (temp, arc, field="all")
	    itype = ""
	    fd1 = arc
	    while (fscan (fd1, image, narcs) != EOF) {
		if (image != itype)
		    printf ("%s %02d\n", image, narcs, >> temp)
		itype = image
	    }
	    delete (arc, verify=no)
	    sort (temp, column=2, ignore=yes, numeric=yes, reverse=no) |
	    fields ("STDIN", "1", lines="1-99", > arc)
	    delete (temp, verify=no)
	}
end
