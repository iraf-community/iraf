#{  TEST_IFERR -- Test various iferr constructs.

procedure test_iferr (type)

int	type			{ prompt = "Error test code: " 	}

begin
	printf ("Testing iferr....\n\t")
	for (i=1; i <= 5; i=i+1) {
	    iferr { errif (i) } then {
	        print ("    error from test #"//i)
	    } else {
	        print ("    NO error from test #"//i)
	    }
	}

	printf ("\n\n")
	printf ("Testing divzero error....\n\t")
	iferr { i = 1 / 0 } then {
	    print ("    error from divzero test")
	} else {
	    print ("    NO error from divzero test")
	}
	;

	printf ("\n\n")
	printf ("Testing fdivzero error....\n\t")
	iferr { x = 1.0 / 0.0 } then {
	    print ("    error from fdivzero test")
	} else {
	    print ("    NO error from fdivzero test")
	}
end
