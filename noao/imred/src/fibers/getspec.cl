# GETSPEC -- Get spectra which are not multispec and strip ".imh" extension

procedure getspec (images)

string	images			{prompt="List of images"}
struct	*fd

begin
	string	temp, image, ctype
	int	n

	temp = mktemp ("tmp$iraf")
	hselect (images, "$I,naxis,ctype1", yes, > temp)
	fd = temp
	ctype = ""
	while (fscan (fd, image, n, ctype) != EOF) {
	    if (n==1 || (nscan() == 3 && ctype == "MULTISPE"))
		next
	    n = strlen (image)
	    if (n > 4 && substr (image, n-3, n) == ".imh")
		image = substr (image, 1, n-4)
	    print (image)
	}
	fd = ""
	delete (temp, verify=no)
end
