#{ OVERHANDLER -- VOTable SAMP message handler to overlay a catalog on the
#  image display.

procedure overhandler (url)

string	url 			{ prompt = "VOTable URL"		}
int	fnum  = 0		{ prompt = "Test number"		}

begin
	string  tcat

	# Get the currently displayed image.
	dispname (1)
	if (dispname.status != 0)
	    error (0, "No image displayed")
	else
	    wcsinfo (dispname.name)


	tcat = mktemp ("/tmp/cat")

	votpos (url, out="STDOUT", verb-, number+, header-, >& tcat)
	taboverlay (dispname.name, tcat, lab=1, ra=2, dec=3)

	delete (tcat, verify-, >& "dev$null")
end
