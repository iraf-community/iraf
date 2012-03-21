#
#  Save data-query results in formats other than VOTable.

# Set the test description string.
votest.descr = "Save data-query results in formats other than VOTable."

print ("------------------------------------------------------------------")
print ("Req  4.4:  Users shall be able to save the results of a query in a")
print ("	   tabular format other than VOTable.")
print ("------------------------------------------------------------------")


fcache init

# Execute the test commands.

string qstr, ofn

# Generate a query string.
qstring ("gsc2.3", "m101", size=0.005, type="catalog") | scan (qstr)
print (qstr)

votcopy (qstr, "STDOUT", format="ascii")
votcopy (qstr, "STDOUT", format="asv")
votcopy (qstr, "STDOUT", format="bsv")
votcopy (qstr, "STDOUT", format="csv")
votcopy (qstr, "STDOUT", format="tsv")
votcopy (qstr, "STDOUT", format="html")
votcopy (qstr, "STDOUT", format="shtml")
votcopy (qstr, "STDOUT", format="xml")
votcopy (qstr, "STDOUT", format="raw")
votcopy (qstr, "STDOUT", format="votable")

# The FITS file is a special case where we want to create a file.
ofn = "tmp$req44.fits"
delete (ofn, verify-, >& "dev$null")
votcopy (qstr, ofn, format="fits")
tinfo (ofn)
delete (ofn, verify-, >& "dev$null")
