#{ TBLHANDLER -- Utility VOTable SAMP message handler

procedure tblhandler (url)

string	url 			{ prompt = "VOTable URL"		}

string	task1 = "imexam %s 1"   { prompt = "Single-row task to execute"	}
string	taskn = "" 		{ prompt = "Multi--row task to execute"	}

begin
	string  inurl, sname, tname, tab, tsk, t1, tn, root
	int     sia

	inurl   = url
	t1      = task1
	tn      = taskn


	iferr {
	    tab = mktemp ("tmp$tbl")
	    # copy (inurl, tab)		FIXME
	    urlget (inurl, tab, use_cache-, extn="", verb-)
	    unlearn ("tinfo")
	    tinfo (inurl, >& "dev$null")

	    # Check for an SIA result table.
	    match ("Image_AccessReference", tab) | count ("STDIN") | scan (sia)

	    if (tinfo.nrows == 1 && sia == 1)		# get task to execute
		tsk = t1
	    else
		tsk = tn

	    # Execute the command depending on the number of rows in the table.

	    sections ("@" // osfn (tab)) | scan (tname)
	    root = tname
	    if (imaccess (tname // "[1]") == yes) {
	        if (imaccess (tname // "[SCI]") == yes)
		    tname = tname // "[SCI]"
	        else
		    tname = tname // "[1]"
	    }

	    # Execute the command.
	    printf (tsk // "\n", tname) | clbye()

	    printf ("Save As (or <cr> to quit)? ")	# Save displayed image?
	    scan (sname)
	    if (nscan () == 1) {
		print (root // " --> " // sname)
		copy (root, sname)
	    }

	} then {
	    delete (tab, ver-, >& "dev$null")
	    logout
	}

	delete (tab, ver-, >& "dev$null")
end
