#{ MAKEIMAGES --

# getinroot,s,a,,,,Input root file name
# getoutroot,s,a,,,,Root filename for output images
# inroot,s,h
# outroot,s,h

{
	inroot = getinroot
	outroot = getoutroot

	if (access("scratch$"//inroot//"001")) {
	    readvt ("scratch$"//inroot//"001", inroot//"tmp1")
	    quickfit (inroot//"tmp1001",verbose=yes)
	    rmap (inroot//"tmp1001",outroot//"a1",outroot//"a3",
		outroot//"a2","H"//outroot//"a")
	    delete ("scratch$"//inroot//"001")
	    imdelete (inroot//"tmp1001")
	} else {
	    print ("scratch$"//inroot//"001 not accessable")
	}

	if (access("scratch$"//inroot//"002")) {
	    readvt ("scratch$"//inroot//"002", inroot//"tmp2")
	    quickfit (inroot//"tmp2001",verbose=yes)
	    rmap (inroot//"tmp2001",outroot//"b1",outroot//"b3",
		outroot//"b2","H"//outroot//"b")
	    delete ("scratch$"//inroot//"002")
	    imdelete (inroot//"tmp2001")
	} else {
	    print ("scratch$"//inroot//"002 not accessable")
	}

	if (access("scratch$"//inroot//"003")) {
	    readvt ("scratch$"//inroot//"003", inroot//"tmp3")
	    quickfit (inroot//"tmp3001",verbose=yes)
	    rmap (inroot//"tmp3001",outroot//"c1",outroot//"c3",
		outroot//"c2","H"//outroot//"c")
	    delete ("scratch$"//inroot//"003")
	    imdelete (inroot//"tmp3001")
	} else {
	    print ("scratch$"//inroot//"003 not accessable")
	}

	if (access("scratch$"//inroot//"004")) {
	    readvt ("scratch$"//inroot//"004", inroot//"tmp4")
	    quickfit (inroot//"tmp4001",verbose=yes)
	    rmap (inroot//"tmp4001",outroot//"d1",outroot//"d3",
		outroot//"d2","H"//outroot//"d")
	    delete ("scratch$"//inroot//"004")
	    imdelete (inroot//"tmp4001")
	} else {
	    print ("scratch$"//inroot//"004 not accessable")
	}

	if (access("scratch$"//inroot//"005")) {
	    readvt ("scratch$"//inroot//"005", inroot//"tmp5")
	    quickfit (inroot//"tmp5001",verbose=yes)
	    rmap (inroot//"tmp5001",outroot//"e1",outroot//"e3",
		outroot//"e2","H"//outroot//"e")
	    delete ("scratch$"//inroot//"005")
	    imdelete (inroot//"tmp5001")
	} else {
	    print ("scratch$"//inroot//"005 not accessable")
	}
}
