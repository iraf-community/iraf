#{  SPECTAB -- Download all the SDSS spectra found in an SSQUERY table.

procedure spectab (intab)

string	intab 			{ prompt = "Query table name"  		   }
string	imroot 	= "" 		{ prompt = "Root output image"		   }

bool	verbose = no		{ prompt = "Verbose output?"		   }
int	status 	= 0		{ prompt = "Service status code"	   }

string  *flist

begin
    string  in, root, tfile, name, sname, sid
    bool    verb


    in   = intab
    root = imroot
    verb = verbose

    # Set the environment.
    reset clobber = yes
    reset imclobber = yes

    tfile = osfn (mktemp ("tmp$tfile"))

    # Unpack the ivorns from the output table.
    fields (intab, "1,7", > tfile)

    flist = tfile
    i = 0
    while (fscan (flist, sname, sid) != EOF) {
        if (root != "")
	    printf ("%s%03d.fits\n", root, i) | scan (name)
	else
            name = substr (sname, 1, 10) // ".fits"
        if (imaccess(name) == no)
            ssget (sid, name, verbose=verb)

        i = i + 1
    }
    flist = ""

    # Clean up.
    delete (tfile, verify-, >& "dev$null")
end
