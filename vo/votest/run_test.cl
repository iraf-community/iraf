#{ RUN_TEST -- Run a test script.


procedure run_test (name, script, output)

string	name				{ prompt = "Test name"		      }
string	script				{ prompt = "Script to execute"	      }
string	output				{ prompt = "Filename of test output"  }

bool	pass = no			{ prompt = "Test result"	      }
bool	cl_err = no			{ prompt = "CL script error?"	      }

begin
    string lname, lscript, lresult, loutput, lres, lout, descr
    int    ndiff


    # Get parameters to local script variables.
    lname	= name
    lscript	= "tests$/" // script // ".cl"
    lresult	= "tests$/" // script // ".out"
    loutput	= output
    descr	= " "

    # Run the test.
    votest.has_err = no
    iferr {
        cl ( < lscript, >& loutput )
        # print ("cl < " // lscript) |& clbye(, >& loutput)
        # type (lscript) |& clbye(, >& loutput)

    } then {
        printf ("\t%-50.50s\t", votest.descr)
	if (votest.has_err == yes) {
	    pass = yes
	    cl_err = no
	} else {
	    pass = no
	    cl_err = yes
	}

    } else {
        printf ("\t%-50.50s\t", votest.descr)

	lres = osfn (lresult)
	lout = osfn (loutput)
        diff ("-bitw", lres, lout) | count("STDIN") | scan(ndiff)

        if (ndiff == 0) {
	    pass = yes
        } else {
	    pass = no
            diff ("-bitw", lres, lout)
	    #print ("!diff -bitw " // lres // " " // lout) | cl()
        }
    }
end
