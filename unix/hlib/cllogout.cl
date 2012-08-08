#{ System logout file; executed when logging off from the CL.  Perform any
# cleanup functions you want executed at logout time.

if (deftask ("_logout") && access ("home$logout.cl"))
    _logout();;
