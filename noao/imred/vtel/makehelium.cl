#{ MAKEHELIUM --

# getinroot,s,a,,,,Input root file name
# getoutroot,s,a,,,,Root filename for output images
# inroot,s,h
# outroot,s,h

{
	inroot = getinroot
	outroot = getoutroot

	if (access(inroot//"1.imh")) {
	    rmap (inroot//"1", outroot//"a1", outroot//"a3", outroot//"a2",
		"H"//outroot//"a")
	    imdelete (inroot//"1")
	} else {
	    print (inroot//"1 not accessable")
	}

	if (access(inroot//"2.imh")) {
	    rmap (inroot//"2", outroot//"b1", outroot//"b3", outroot//"b2",
		"H"//outroot//"b")
	    imdelete (inroot//"2")
	} else {
	    print (inroot//"2 not accessable")
	}

	if (access(inroot//"3.imh")) {
	    rmap (inroot//"3", outroot//"c1", outroot//"c3", outroot//"c2",
		"H"//outroot//"c")
	    imdelete (inroot//"3")
	} else {
	    print (inroot//"3 not accessable")
	}

	if (access(inroot//"4.imh")) {
	    rmap (inroot//"4", outroot//"d1", outroot//"d3", outroot//"d2",
		"H"//outroot//"d")
	    imdelete (inroot//"4")
	} else {
	    print (inroot//"4 not accessable")
	}

	if (access(inroot//"5.imh")) {
	    rmap (inroot//"5", outroot//"e1", outroot//"e3", outroot//"e2",
		"H"//outroot//"e")
	    imdelete (inroot//"5")
	} else {
	    print (inroot//"5 not accessable")
	}
}
