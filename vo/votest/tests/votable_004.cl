#
#  Copy VOTable columns

# Set the test description string.
votest.descr = "TABLES support: Copy VOTable columns"

# Convert the data$logical to a local path.
s1 = data_url // "/usno-b.xml"
s2 = "file://" // data_path // "/usno-b.xml"
s3 = "file:///localhost" // data_path // "/usno-b.xml"


fcache init

# Create the output table name.
string out
out = "tmp$out_004"
if (access (out) == yes) {
    delete (out, verify-, >& "dev$null")
}
;


# Execute the test commands.
print ("\nLogical Path:  data$usno-b.xml")		# logical path
tproject ("data$usno-b.xml", out, "id sRa sDec")
tdump (out, cdfile="", pfile="")
delete (out, verify-, >& "dev$null")

print ("\nHTTP URI:  " // s1)				# remote http URI
tproject (s1, out, "id sRa sDec")
tdump (out, cdfile="", pfile="")
delete (out, verify-, >& "dev$null")

print ("\nFile URI:  " // s2)				# file URI
tproject (s2, out, "id sRa sDec")
tdump (out, cdfile="", pfile="")
delete (out, verify-, >& "dev$null")

print ("\nFile URI:  " // s3)				# file URI
tproject (s3, out, "id sRa sDec")
tdump (out, cdfile="", pfile="")
delete (out, verify-, >& "dev$null")
