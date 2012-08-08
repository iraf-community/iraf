#
#  Select DAL results for download.

# Set the test description string.
votest.descr = "Select DAL results for download"

print ("------------------------------------------------------------------")
print ("Req  4.5:  Users shall be able to select which data returned by a")
print ("	   DAL service query is to be downloaded to local disk.")
print ("------------------------------------------------------------------")


fcache init

# Execute the test commands.

string  ofn1, ofn2

ofn1 = mktemp ("tmp$req45")
ofn2 = mktemp ("tmp$req45") // ".fits"

# Get an SIA service result.
getimg ("dss", "m83", size=0.1, output=ofn1, format="raw")

# Select only the FITS data.  Note the use of the column name here
# implies some knowledge of the table schema.
tselect (ofn1, ofn2, expr="Format ?= 'fits'")

# Print the URLs we would like to download.
tdump (ofn2, columns="URL", cdfile="",  pfile="",  datafile="STDOUT")

# Clean up.
delete (ofn1, verify-, >& "dev$null")
delete (ofn2, verify-, >& "dev$null")


