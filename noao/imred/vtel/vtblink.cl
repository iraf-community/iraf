#{ VTBLINK -- Blink successive frames of daily grams to check registration.

# imname1,s,a,,,,Name of first image
# imname2,s,a,,,,Name of next image
# z1,r,h,-3000.0,,,Minimum graylevel to be displayed.
# z2,r,h,3000.0,,,Minimum graylevel to be displayed.

{
	real	zz1, zz2, offset, currentoffset
	char	im1name, im2name, framelog[4]
	int	im1long, im2long, currentframe, offscreenflag

	# initialize
	print ("  ")
	print ("  ")
	print ("vtblink vtblink vtblink vtblink vtblink vtblink vtblink")
	print ("  ")
	currentframe = 1
	offscreenflag = 0
	currentoffset = .72      # Start at the right side of the screen.
	framelog[1] = "none"
	framelog[2] = "none"
	framelog[3] = "none"
	framelog[4] = "none"

	# Get the gray scale.
	zz1 = z1
	zz2 = z2

	# Get the first frame from the user, display it, allow user to window.
	im1name = imname1
	if (im1name == "end") {
	    bye
	}
	while (!access(im1name//".imh") && im1name != "end") {
	    print (im1name, "not accessable, try again")
	    im1name = imname1
	    if (im1name == "end") {
	        bye
	    }
	}
	imgets (im1name, "L_ZERO")
	im1long = real(imgets.value)
	print ("Longitude of first image is ", im1long)
	print ("Displaying frame.")
	display (im1name, currentframe, xcenter=currentoffset, zrange=no,
	    zscale=no, z1=zz1, z2=zz2)
	framelog[currentframe] = im1name
	frame (currentframe)
	print ("Now, please window this frame for the desired color table.")
	window

	# Make all the color tables of the other 3 frames the same as this.
	print ("Equalizing color tables of 4 frames, Please wait.")
	lumatch (2, currentframe)
	lumatch (3, currentframe)
	lumatch (4, currentframe)

	# Get the next frame from the user.
	im2name = imname2
	while (im2name == "stat") {
	    print ("Frame 1 contains image ", framelog[1])
	    print ("Frame 2 contains image ", framelog[2])
	    print ("Frame 3 contains image ", framelog[3])
	    print ("Frame 4 contains image ", framelog[4])
	    im2name = imname2
	}
	if (im2name == "end") {
	    bye
	}
	while (!access(im2name//".imh") && im2name != "end") {
	    print (im2name, "not accessable, try again")
	    im2name = imname2
	    if (im2name == "end") {
	        bye
	    }
	}
	imgets (im2name, "L_ZERO")
	im2long = real(imgets.value)
	print ("Longitude of this image is ", im2long)

	# While the user does not enter 'end' for the image name, keep going.
	# also check the offscreenflag and exit it it becomes set.
	while (im2name != 'end' && offscreenflag != 1) {
	
	    # Calculate offset. subsequent images in general have smaller
	    # longitudes, that is, longitude decreases with time.
	    # If the new image has a larger longitude then fix up offset.
	    if (im1long < im2long) {
	        offset = real((im2long - 360) - im1long)/512.
	    } else {
	        offset = real(im2long - im1long)/512.
	    }

	    # If we are getting too close to the left side, restart program.
	    if ((currentoffset+offset) <= .18) {
		print("*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*")
		print("* The next image would overlap the edge of the      *")
		print("* screen. Please restart the program with the last  *")
		print("* image.                                            *")
		print("*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*")
		offscreenflag = 1
	    }

	    # Display the next image and blink it with the previously displayed
	    # image.
	    if (offscreenflag != 1) {
	        print ("Displaying frame.")
	        display (im2name, mod(currentframe,4)+1,
		    xcenter=currentoffset+offset, zscale=no, zrange=no,
		    z1=zz1, z2=zz2)
		framelog[mod(currentframe,4)+1] = im2name

	        # Return the user to the cl so s/he can do whatever s/he wants.
	        print("  ")
	        print("You are now in the cl, type 'bye' to return to vtlbink")
	        cl()
	        print("  ")

	        # Update currentframe and print it out, update the offset.
	        currentframe = mod(currentframe,4)+1
		print ("The next frame to be used for display is frame ", 
		    mod(currentframe,4)+1)
	        currentoffset += offset

	        # Move image2 to image1 and then get a new image2 and loop back.
	        im1name = im2name
	        im1long = im2long
	        im2name = imname2
		while (im2name == "stat") {
	    	    print ("Frame 1 contains image ", framelog[1])
	    	    print ("Frame 2 contains image ", framelog[2])
	    	    print ("Frame 3 contains image ", framelog[3])
	    	    print ("Frame 4 contains image ", framelog[4])
	    	    im2name = imname2
		}
	        while (!access(im2name//".imh") && im2name != "end") {
	            print (im2name, "not accessable, try again")
	            im2name = imname2
	            if (im2name == "end") {
	                bye
	            }
	        }
		if (im2name != "end") {
		    imgets (im2name, "L_ZERO")
		    im2long = real(imgets.value)
		}
	    }
	}
}
