#{ WRITETAPE -- Write five images to a vacuum telescope tape.  The
# script accepts the name of the mag tape device and the general input
# image filename from the user.  Writetape appends a digit [1-5] to the
# file name for each file to be written.

# getmtape,s,a,,,,Mag tape device to write to
# getname,s,a,,,,Root filename for the 5 images
# magtape,s,h
# imname,s,h

{

	imname = getname
	magtape = getmtape

	if (access(imname//"1.imh")) {
	    writevt (imname//"1", magtape//"1600[1]")
	} else {
	    print (imname//"1 not accessable")
	}

	if (access(imname//"2.imh")) {
	    writevt (imname//"2", magtape//"1600[2]")
	} else {
	    print (imname//"2 not accessable")
	}

	if (access(imname//"3.imh")) {
	    writevt (imname//"3", magtape//"1600[3]")
	} else {
	    print (imname//"3 not accessable")
	}

	if (access(imname//"4.imh")) {
	    writevt (imname//"4", magtape//"1600[4]")
	} else {
	    print (imname//"4 not accessable")
	}

	if (access(imname//"5.imh")) {
	    writevt (imname//"5", magtape//"1600[5]")
	} else {
	    print (imname//"5 not accessable")
	}
}
