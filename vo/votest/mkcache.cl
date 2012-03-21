#{ MKCACHE.CL -- Execute queries that will load our cache for demos when
#                we're not online.

procedure mkcache ()

bool	list 		= no		{ prompt = "List available tests"    }
bool	verbose 	= yes		{ prompt = "Verbose output?"         }
real	size		= 0.15		{ prompt = "search size"             }
bool	do_sesame	= no		{ prompt = "Sesame cache?"	     }
bool	do_dss  	= no		{ prompt = "DSS cache?"	     }
bool	do_2mass	= yes		{ prompt = "2MASS cache?"	     }


begin
    real   sz
    int    N

    sz    = size



    # Cache positions from Sesame.
    # ----------------------------
    if (do_sesame) {
        for (N=1; N <= 110; N=N+1) { 		# 110 Messier Objects
	    print ("Sesame: m" // N)
	    sesame ("m"//N, verb+)
	}
        for (N=1; N <= 7840; N=N+1) { 		# 7840 NGC Objects
	    print ("Sesame: NGC" // N)
	    sesame ("ngc"//N, verb+)
	}
    }

    # Cache images of Messier Catalog.
    if (do_dss) {
        for (N=1; N <= 110; N=N+1) {
	    print ("DSS: m" // N)
	    dss ("m" // N, size=sz)
        }
    }


    # Cache images of Messier Catalog.
    if (do_2mass) {
	getcat.otype = "votable"
	getcat.size  = sz
        for (i=1; i <= 110; i=i+1) {
	    print ("2MASS: m" // i)
	    iferr {
	        getcat ("2mass-psc", "m"//i, out="m"//i//"_psc")
	    } then {
		print ("Error")
	    }
	    #votpos ("m"//i//"_psc", out="m"//i//"_pos.txt")
	    #taboverlay ("m"//i, "m"//i//"_pos.txt")
        }
    }
end
