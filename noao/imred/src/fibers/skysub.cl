# SKYSUB -- Sky subtract fiber spectra.
#   Subtract the selected sky apertures from the selected object apertures.
#   The object apertures may include the sky apertures if desired.
#   Unless the skymedian option is used the object and sky apertures should
#   match.  The subtracted sky may be saved in an image with the prefix "sky"
#   and the same output name.  Note that existing output images are clobbered.

procedure skysub (input)

string	input = ""		{prompt="Input spectra to sky subtract"}

string	output = ""		{prompt="Output sky subtracted spectra"}
string	objaps = ""		{prompt="Object apertures"}
string	skyaps = ""		{prompt="Sky apertures"}
string	objbeams = ""		{prompt="Object beam numbers"}
string	skybeams = ""		{prompt="Sky beam numbers"}
bool	skyedit	= yes		{prompt="Edit the sky spectra?"}
string	combine = "average"	{prompt="Combining option",
				 enum="average|median"}
string	reject	= "avsigclip"	{prompt="Sky rejection option",
				 enum="none|minmax|avsigclip"}
string	scale = "none"		{prompt="Sky scaling option",
				 enum="none|mode|median|mean"}
bool	saveskys = yes		{prompt="Save sky spectra?"}
file	logfile = ""		{prompt="Logfile"}

struct	*fd1
struct	*fd2
struct	*fd3

begin
	string	in, out, out1, sky, log, aps, str
	file	temp1, temp2, temp3, temp4
	int	i, j

	temp1 = mktemp ("tmp$iraf")
	temp2 = mktemp ("tmp$iraf")
	temp3 = mktemp ("tmp$iraf")
	temp4 = mktemp ("tmp$iraf")

	if (logfile == "")
	    log = "dev$null"
	else
	    log = logfile

	sections (input, option="fullname", > temp1)
	sections (output, option="fullname", > temp2)
	fd1 = temp1
	fd2 = temp2
	while (fscan (fd1, in) != EOF) {
	    i = strlen (in)
	    if (i > 4 && substr (in, i-3, i) == ".imh")
		in = substr (in, 1, i-4)
	    if (fscan (fd2, out) < 1)
		out = in
	    out1 = out
	    i = strlen (out1)
	    if (i > 4 && substr (out1, i-2, i) == ".ms")
		out1 = substr (out1, 1, i-3)

	    aps = skyaps
	    sky = "sky" // out1
	    if (access (sky // ".imh"))
		imdelete (sky, verify=no)
	    if (skyedit) {
		scopy (in, sky, w1=INDEF, w2=INDEF, apertures=aps,
		    beams=skybeams, apmodulus=0, offset=0, clobber=yes,
		    format="multispec", merge=no, renumber=no,
		    verbose=yes, >> "dev$null")
		specplot (sky, apertures="", autolayout=no, autoscale=yes,
		    fraction=1., scale=1., offset=0., step=0., ptype="1",
		    labels="user", ulabels="", sysid=yes, yscale=yes,
		    xlpos=1.02, ylpos=0., title="Edit sky spectra from "//in,
		    xlabel="", ylabel="", xmin=INDEF, xmax=INDEF,
		    ymin=INDEF, ymax=INDEF, logfile=temp4, graphics="stdgraph")
		imdelete (sky, verify=no)
		match (sky, temp4, stop=no) |
		fields (fields="2", lines="1-9999") |
		sort (column=0, ignore=yes, numeric=no, reverse_sort=no) |
		unique (> temp3)
		delete (temp4, verify=no)
		aps = ""
		fd3 = temp3
		while (fscan (fd3, str) != EOF) {
		    i = stridx ("[", str) + 1
		    j = stridx ("]", str) - 1
		    aps = aps // substr (str, i, j) // ","
		}
		fd3 = ""; delete (temp3, verify=no)

		reject.p_mode="q"
		str = reject
		reject.p_mode="h"
	    }

	    if (skybeams == "") {
		scombine (in, sky, noutput="", logfile=logfile,
		    apertures=aps, group="all", combine="median",
		    reject=reject, first=yes, scale=scale, zero="none",
		    weight="none", sample="", lthreshold=INDEF,
		    hthreshold=INDEF, nlow=1, nhigh=1, mclip=yes,
		    lsigma=2., hsigma=3., rdnoise="0.", gain="1.",
		    sigscale=0., pclip=-0.5, grow=0, blank=0.)
	    } else {
		temp3 = mktemp ("sky")
		scopy (in, sky, w1=INDEF, w2=INDEF, apertures=aps,
		    beams=skybeams, apmodulus=0, offset=0, clobber=yes,
		    format="multispec", merge=no, renumber=no,
		    verbose=yes, >> log)
		scombine (sky, temp3, noutput="", logfile=logfile,
		    apertures=aps, group="all", combine="median",
		    reject=reject, first=yes, scale=scale, zero="none",
		    weight="none", sample="", lthreshold=INDEF,
		    hthreshold=INDEF, nlow=1, nhigh=1, mclip=yes,
		    lsigma=2., hsigma=3., rdnoise="0.", gain="1.",
		    sigscale=0., pclip=-0.5, grow=0, blank=0.)
		imdelete (sky, verify=no)
		imrename (temp3, sky, verbose=yes, >> log)
	    }
	    sarith (in, "-", sky, out, w1=INDEF, w2=INDEF, apertures=objaps,
		beams=objbeams, reverse=no, ignoreaps=yes, format="multispec",
		renumber=no, offset=0, clobber=yes, merge=no, errval=0.,
		verbose=yes, >> log)
	    if (!saveskys)
		imdelete (sky, verify=no)
	}
	fd1 = ""; delete (temp1, verify=no)
	fd2 = ""; delete (temp2, verify=no)
end
