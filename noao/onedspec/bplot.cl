# BPLOT -- Batch plotting of spectra
#
# Count the number of images in the list and create a temporary
# cursor command file the users cursor commands repeated for each
# image.  Call SPLOT with the cursor file to do the plotting.
# Finally delete the temporary cursor file.
#
# WARNING: Error recovery is non-existant!
#          An abort during this process leaves the
#          stdgraph device assigned to the batch plotter
#          and the CL list variable GCUR assigned a string

procedure bplot (images)

string	images {prompt="List of images to plot"}
string	graphics="stdgraph" {prompt="Graphics output device"}
file	cursor="onedspec$gcurval" {prompt="Cursor input file"}

struct	*list

begin
	file	tmpfile
	string	image

	tmpfile = mktemp ("tmp")
	sections (images, > tmpfile)
	list = tmpfile
	while (fscan (list, image) != EOF)
	    splot (image, line=1, graphics=graphics, cursor=cursor)
	delete (tmpfile, verify=no)
end
