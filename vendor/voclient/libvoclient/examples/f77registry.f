C
C  F77REGISTRY -- Fortran example task of VOClient Registry interface
C
C  M.Fitzpatrick, NOAO, July 2006

	program f77registry

	character 	svc*10, term*60
	character 	title*70, type*20, sname*20
	integer		result, ier, count

 	svc = "cone"
 	term = "cool stars"

c  Initialize the VOClient interface
        call vfinitvoclient ("", ier)

c  Do a keyword Registry search of Cone services.
	call vfregsearchbysvc (svc, term, 0, result, ier)

C  Summarize the results
	call vfresgetcount (result, count)
	print *, "#"
	print *, "# Service Type: ", svc
	print *, "# Search Term:  ", term
	print *, "#"
	print *, "# Found ", count, " matching records"
	print *, "#"

	do 10 i = 1, count

	    call vfresgetstr (result, "Title", i, title, len)
	    call vfresgetstr (result, "ShortName", i, sname, len)
	    call vfresgetstr (result, "Type", i, type, len)

	    print *, "----------------------------------------------------"
	    print *, "Title: ", title
	    print *, "Type:  ", type, "  ShortName:  ", sname

10	continue

c  Shut down the VO Client
        call vfclosevoclient (0)
	stop
	end
