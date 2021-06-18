#  Dynamic Package Loading Script
#
#  This script is "sourced" by the standard hlib$extern.pkg file when the
#  CL is loaded to automatically define any packages installed in the 
#  iraf$extern directory.  Both the package definition and helpdb strings
#  are defined to include the package, negating the previous manual
#  declarations required in hlib$extern.pkg
#
#

string	curdir, extdir, dpkg

extdir = osfn ("iraf$extern")

# Go to the dynamic package directory, but save the current directory so
# we can return when we're done.  At this stage of the login we need to 
# use host commands since the system package isn't available.
printf ("!!/bin/pwd\n") | cl () | scan (curdir)
chdir (extdir)

#  Create a file list to process.
dpkg = mktemp ("tmp$dpkg")
if (access (dpkg) == yes)
  printf ("!!/bin/rm -f %s\n", osfn(dpkg))                             	| cl ()
;
printf ("!!/bin/ls -1ad [a-y]* 2> /dev/null\n") | cl (,> dpkg)

list = dpkg
while (fscan (list, s1) != EOF) {

  # We define an environment variable for installed directories, e.g. a
  # package support directory of data might require it's own definition.
  # This also works to define the variable for actual package code, but 
  # we don't declare the package just yet.
  if (access (s1) == yes && 
       (access (s1//"/.installed") == yes ||
        access (s1//"/"//s1//".cl") == yes)) {
    printf ("reset %s = %s/%s/\nkeep\n", s1, osfn("iraf$extern"), s1)   | cl ()
  }
  ;

  # We assume we can dynamically load a package if there is a "foo.cl"
  # script file in the 'foo' subdirectory.
  if (access (s1//"/"//s1//".cl") == yes) {
    printf ("task  %s.pkg = %s$%s.cl\nkeep\n", s1, s1, s1)              | cl ()

    # Add to the helpdb string.
    printf ("reset helpdb=%s,%s$lib/helpdb.mip\nkeep\n", 
	envget("helpdb"), s1) 						| cl ()
  }
  ;
}

# Clean up and go back to the login directory.
printf  ("!!/bin/rm -f %s\n", osfn(dpkg))                               | cl ()
if (curdir != "") {
    chdir (curdir)
} else {
    back ()
}
;

keep
