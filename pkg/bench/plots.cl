# PLOTS -- Measure the time required to make a number of row plots of an image.

procedure plots (image, nlines)

string	image		{ prompt = "image to be plotted" }
int	nlines		{ prompt = "number of line plots to be made" }

string	imname
int	nleft

begin
	cache ("prow")
	imname = image
	time(); print ("======== start ========")

	for (nleft=nlines;  nleft > 0;  nleft-=1)
	    $prow (imname, 50, >G "dev$null")

	print ("========  end  ========"); time()
end
