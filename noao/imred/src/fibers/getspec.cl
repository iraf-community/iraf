# GETSPEC -- Get spectra which are processed but are not extracted.
# Strip the imtype extension.

procedure getspec (images, output)

string	images			{prompt="List of images"}
file	output			{prompt="Output file of images"}
bool	ccdproc			{prompt="Add CCDPROC keyword and continue?",
				 mode="q"}
struct	*fd

begin
	string	imtype, temp, image, system=""
	int	n, n1

	imtype = "." // envget ("imtype")
	n = stridx (",", imtype)
	if (n > 0)
	    imtype = substr (imtype, 1, n-1)
	n1 = strlen (imtype)

	# Initialize files
	set clobber=yes
	sleep (> output)
	set clobber=no

	temp = mktemp ("tmp$iraf")
	sections (images, option="fullname", > temp)
	fd = temp
	while (fscan (fd, image) != EOF) {
	    hselect (image, "ccdproc", yes) | scan (system)
	    if (nscan() == 0) {
		printf ("%s: CCDPROC keyword not found.\n", image)
		printf ("  Either run CCDPROC or add CCDPROC keyword with HEDIT.\n")
		if (!ccdproc)
		    error (1, "Exit")
		hedit (image, "ccdproc", "DONE", add=yes, update=yes,
		    verify=no, show=no)
	    }
	    hselect (image, "wat0_001", yes) | scanf ("system=%s", system)
	    if (system=="equispec" || system=="multispec")
		next
	    n = strlen (image)
	    if (n > n1 && substr (image, n-n1+1, n) == imtype)
		image = substr (image, 1, n-n1)
	    print (image, >> output)
	}
	fd = ""; delete (temp, verify=no)
end
