#{ DESTREAK5 -- Destreak all five images from a vacuum telescope tape.  The
# script accepts the general input image filename and the general output
# image filename from the user (and now the scratch disk).  Destreak5
# appends a digit [1-5] to the file name for each file read and each
# corresponding file  written.

# getinput,s,a,,,,General input filename for the 5 images
# getoutput,s,a,,,,General output filename for the 5 images
# inim,s,h
# outim,s,h

{

	inim = getinput
	outim = getoutput

	if (access("vtelscr$"//inim//"001")) {
	    readvt ("vtelscr$"//inim//"001", inim//"tmp1")
	    quickfit (inim//"tmp1001",verbose=yes)
	    delete ("vtelscr$"//inim//"001")
	    getsqib (inim//"tmp1001", inim//"sqib1")
	    destreak (inim//"tmp1001", inim//"temp1", inim//"tmpr1")
	    imdelete (inim//"tmp1001")
	    imdelete (inim//"tmpr1")
	    putsqib (inim//"temp1", inim//"sqib1", outim//"1")
	    imdelete (inim//"temp1")
	    imdelete (inim//"sqib1")
	} else {
	    print ("vtelscr$"//inim//"001 not accessable")
	}

	if (access("vtelscr$"//inim//"002")) {
	    readvt ("vtelscr$"//inim//"002", inim//"tmp2")
	    quickfit (inim//"tmp2001",verbose=yes)
	    delete ("vtelscr$"//inim//"002")
	    getsqib (inim//"tmp2001", inim//"sqib2")
	    destreak (inim//"tmp2001", inim//"temp2", inim//"tmpr2")
	    imdelete (inim//"tmp2001")
	    imdelete (inim//"tmpr2")
	    putsqib (inim//"temp2", inim//"sqib2", outim//"2")
	    imdelete (inim//"temp2")
	    imdelete (inim//"sqib2")
	} else {
	    print ("vtelscr$"//inim//"002 not accessable")
	}

	if (access("vtelscr$"//inim//"003")) {
	    readvt ("vtelscr$"//inim//"003", inim//"tmp3")
	    quickfit (inim//"tmp3001",verbose=yes)
	    delete ("vtelscr$"//inim//"003")
	    getsqib (inim//"tmp3001", inim//"sqib3")
	    destreak (inim//"tmp3001", inim//"temp3", inim//"tmpr3")
	    imdelete (inim//"tmp3001")
	    imdelete (inim//"tmpr3")
	    putsqib (inim//"temp3", inim//"sqib3", outim//"3")
	    imdelete (inim//"temp3")
	    imdelete (inim//"sqib3")
	} else {
	    print ("vtelscr$"//inim//"003 not accessable")
	}

	if (access("vtelscr$"//inim//"004")) {
	    readvt ("vtelscr$"//inim//"004", inim//"tmp4")
	    quickfit (inim//"tmp4001",verbose=yes)
	    delete ("vtelscr$"//inim//"004")
	    getsqib (inim//"tmp4001", inim//"sqib4")
	    destreak (inim//"tmp4001", inim//"temp4", inim//"tmpr4")
	    imdelete (inim//"tmp4001")
	    imdelete (inim//"tmpr4")
	    putsqib (inim//"temp4", inim//"sqib4", outim//"4")
	    imdelete (inim//"temp4")
	    imdelete (inim//"sqib4")
	} else {
	    print ("vtelscr$"//inim//"004 not accessable")
	}

	if (access("vtelscr$"//inim//"005")) {
	    readvt ("vtelscr$"//inim//"005", inim//"tmp5")
	    quickfit (inim//"tmp5001",verbose=yes)
	    delete ("vtelscr$"//inim//"005")
	    getsqib (inim//"tmp5001", inim//"sqib5")
	    destreak (inim//"tmp5001", inim//"temp5", inim//"tmpr5")
	    imdelete (inim//"tmp5001")
	    imdelete (inim//"tmpr5")
	    putsqib (inim//"temp5", inim//"sqib5", outim//"5")
	    imdelete (inim//"temp5")
	    imdelete (inim//"sqib5")
	} else {
	    print ("vtelscr$"//inim//"004 not accessable")
	}
}
