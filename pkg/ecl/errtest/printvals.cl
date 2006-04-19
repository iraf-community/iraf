procedure printvals ()

begin

time
return
	printf ("PRINTVALS:\n");
	printf ("\t$errno = %d\n", $errno)
	printf ("\t$errmsg = %d\n", $errmsg)
	printf ("\t$errtask = %d\n", $errtask)

	i  =  cl.$errno
	s1 =  cl.$errmsg
	s2 =  cl.$errtask

	=i
	=s1
	=s2
	keep
end
