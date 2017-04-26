#
#  Print VOTable column information

# Set the test description string.
votest.descr = "TABLES support: Print VOTable column info"

# Convert the data$logical to a local path.
s1 = data_url // "/usno-b.xml"
s2 = "file://" // data_path // "/usno-b.xml"
s3 = "file:///localhost" // data_path // "/usno-b.xml"


fcache init
reset clobber = yes

# Create the output table name.
string out
out = "/tmp/out_006"
if (access (out) == yes) {
    delete (out, verify-, >& "dev$null")
}
;

# Execute the test commands.
print ("\nLogical Path:  data$usno-b.xml")		# logical path
tcopy ("data$usno-b.xml", out, verb-)
tinfo ("data$usno-b.xml,"//out)
delete (out)

print ("\nHTTP URI:  " // s1)				# remote http URI
tcopy (s1, out, verb-)
tinfo (s1//","//out)
delete (out)

print ("\nFile URI:  " // s2)				# file URI
tcopy (s2, out, verb-)
tinfo (s2//","//out)
delete (out)

print ("\nFile URI:  " // s3)				# file URI
tcopy (s3, out, verb-)
tinfo (s3//","//out)
delete (out)

