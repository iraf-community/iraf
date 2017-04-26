# BPMEDIT -- Edit BPM masks.

procedure bpmedit (images)

string	images		{prompt="List of images"}
string	bpmkey = "BPM"	{prompt="Keyword with mask name"}
int	frame = 1	{prompt="Display frame with mask overlay"}
int	refframe = 2	{prompt="Display frame without mask overlay"}
string	command = "display $image $frame over=$mask erase=$erase ocol='1-10=red,green' fill-"		{prompt="Display command"}
bool	display = yes	{prompt="Interactive display?"}
string	cursor = ""	{prompt="Cursor input"}

struct	*fd

begin
	int	i1
	file	im, bpm, temp
	struct	dispcmd

	set imedit_help = "tv$imedit/bpmedit.key"

	temp = mktemp ("tmp$iraf")

	sections (images, option="fullname", > temp)

	fd = temp
	while (fscan (fd, im) != EOF) {
	    bpm = ""; hselect (im, bpmkey, yes) | scan (bpm)
	    if (bpm == "") {
		printf ("WARNING: No %s keyword (%s)\n", bpmkey, im)
		next
	    }
	    if (imaccess(bpm)==NO) {
		printf ("WARNING: Can't access mask (%s)\n", bpm)
		next
	    }

	    if (display) {
	        # Override certain display parameters.
		display.bpdisplay="none"
		display.fill = no

		# Set display command.
		dispcmd = command
		i1 = strstr ("$image", dispcmd)
		if (i1 > 0)
		    dispcmd = substr (dispcmd, 1, i1-1) // im //
			substr (dispcmd, i1+6, 1000)
		i1 = strstr ("$frame", dispcmd)
		if (i1 > 0)
		    dispcmd = substr (dispcmd, 1, i1-1) // frame //
			substr (dispcmd, i1+6, 1000)
		i1 = strstr ("$mask", dispcmd)
		if (i1 > 0)
		    dispcmd = substr (dispcmd, 1, i1-1) // "$image" //
			substr (dispcmd, i1+5, 1000)
		i1 = strstr (">", dispcmd)
		if (i1 == 0)
		    dispcmd += " >& dev$null"

		display (im, refframe, over="", >& "dev$null")
		imedit (bpm, "", command=dispcmd, display=display,
		    cursor=cursor, search=0)
	    } else
		imedit (bpm, "", command=dispcmd, display=display,
		    cursor=cursor, search=0)
	}
	fd = ""; delete (temp, verify-)
end
