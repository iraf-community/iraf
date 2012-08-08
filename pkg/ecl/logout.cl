# LOGOUT.CL -- Executed when you log out of the CL.  Keep this around in the CL
# directory just to make sure this feature continues to work.

history (100, >> "uparm$history.cl")
time
