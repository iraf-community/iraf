#{  VODATA -- Call a DAL service.

procedure vodata (svc, obj)

string	svc			{ prompt = "Service Name"		}
string	obj			{ prompt = "Object Name"		}

real	sz	 = 0.1		{ prompt = "Search size"		}
string	output   = "samp"	{ prompt = "Output filename"		}
string	otype    = "ascii"	{ prompt = "Output format",
				    min="ascii|csv|votable|fits"	} 
bool    verbose  = yes          { prompt = "Verbose output?"            }
int	status   = 0		{ prompt = "Service status code"	}

begin
    string  lsvc, lobj, tout, out, cmd
    string  dum1, dum2, t1, t2

    lsvc = svc
    lobj = obj
    if (output == "samp")
        tout = osfn (mktemp ("tmp$" // lsvc))
    else
        tout = osfn (lsvc)
    out  = tout // ".xml"


    time() | scan (dum1, t1, dum2)		# start time

    if (verbose)
        printf ("Querying data at '"// lsvc //"' for Object '"// lobj //"' ...")
    cmd = "!vodata -q -V -O " // tout // " " // lsvc //" "// lobj // " " // sz

    print (cmd) | cl(,>& "dev$null")

    time() | scan (dum1, t2, dum2)		# end time
    if (verbose)
        printf ("done.\n")

    tinfo (out, >& "dev$null")
    if (verbose) {
	if (tinfo.nrows > 0) {
            printf ("Results: " // tinfo.nrows // " rows  ")
            printf (tinfo.ncols // " columns  ")
            printf ("(%5.1f sec)", ((real(t2) - real(t1)) * 3600.))
	    
            printf ("\n")
	} else {
            printf ("No results found\n")
	}
    }


    if (output == "samp") {
	samp ("quiet")
	samp ("loadVOTable", out)
	samp ("noquiet")
    }
end
