# MSREIDENTIFY -- Reidentify features in multispec format spectra
 
procedure msreidentify (reference, images)
 
file	reference		{prompt="Reference image"}
string	images			{prompt="Images to be reidentified"}
 
real	cradius=5.		{prompt="Centering radius"}
real	threshold=10.		{prompt="Feature threshold for centering"}
file	database="database"	{prompt="Database"}
struct	*list
 
begin
	int	i, nspec
	file	temp, root, image, ref, im
 
	# Determine the number of spectra in the image.
	temp = mktemp ("temp")
	root = reference
	i = strlen (root)
	if (i > 4 && substr (root, i-3, i) == ".imh")
	    root = substr (root, 1, i-4)
	hselect (root, "naxis2", yes, > temp)
	list = temp
	i = fscan (list, nspec)
	list = ""
	delete (temp, verify=no)
 
	# For each input image use identify to match reference to image.
	sections (images, option="fullname", > temp)
	list = temp
	while (fscan (list, image) != EOF) {
	    i = strlen (image)
	    if (i > 4 && substr (image, i-3, i) == ".imh")
	    image = substr (image, 1, i-4)
	    for (i=1; i<=nspec; i+=1) {
		ref = root // "[*," // i // "]"
		im = image // "[*," // i // "]"
 
	        print (":r ", ref,"\na\nc\nf\nq\n:w\nq\n") |
	        identify (im, database=database,
		    cradius=cradius, threshold=threshold,
		    cursor="STDIN", >G "dev$null", > "dev$null")
	    }
	}
 
	list = ""
	delete (temp, verify=no)
end

