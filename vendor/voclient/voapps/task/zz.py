import sys
import os
import re
import glob


vopkg_path	= ['.', "/tmp"]

pkg_list	= []
pkg_struct	= {}


def pkgList (pattern=None):
    ''' Get the list of available packages.
        @param pattern      the package name pattern to match

        Returns:    A list of available package names.
    '''

    pat = pattern
    if (pattern == None):
        pat = "*"

    for dir in vopkg_path:
        if os.path.isdir(dir):
            pfiles = glob.glob(dir + "/" + pat + ".c")
            print pfiles
            for f in pfiles:
				root,ext = os.path.splitext(os.path.basename(f))
				pkg_struct[root] = [ f ]
				print f
        else:
            root,ext = os.path.splitext(os.path.basename(dir))
            if (re.search(pattern,root)):
				pkg_struct[root] = [ dir ]
				print dir

	# Save just the package names as a valid list.
    pkg_list =  pkg_struct.keys()

    print pkg_list
    print pkg_struct


print "vo*"
pkgList("vo*")
print " "

print "z*"
pkgList("z*")
print " "

print "None"
pkgList()
print " "

print "zz"
pkgList("zz")
print " "

