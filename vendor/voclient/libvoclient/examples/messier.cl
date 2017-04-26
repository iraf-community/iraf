#  MESSIER -- Loop through the Messier catalog, displaying a DSS image of each.

procedure messier ()

begin
    int    qr, stat
    string svc, coords = "/tmp/coords"

    reset clobber = yes				    # Allow file overwrites

    svc = regResolver ("dss","sia")		    # Find the DSS SIA service

    for (i=1; i < 111; i=i+1) {			    # Loop over Messier objs
        sesame ("M"//i,verb+) | scan (x,y)	    # Resolve name to coords
        print ("30 30 0 :text M"//i, > coords)
    
        qr = dalSiapSvc (svc, x, y, 0.25)	      # Query for data
        stat = dalGetData (qr, 0, "foo.fits")	      # Download 1st image
        display ("foo.fits[0]", 1)		      # Display it
        tvmark (1,"",commands=coords,txsiz=5,col=205) # Label it
    }
end
