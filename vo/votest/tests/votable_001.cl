#
#  Print VOTable information

# Set the test description string.
votest.descr = "TABLES support: Print VOTable info"

# Convert the data$logical to a local path.
s1 = data_url // "/usno-b.xml"
s2 = "file://" // data_path // "/usno-b.xml"
s3 = "file:///localhost" // data_path // "/usno-b.xml"


fcache init

# Execute the test commands.
print ("\nLogical Path:  data$usno-b.xml")		# logical path
tinfo ("data$usno-b.xml")

print ("\nHTTP URI:  " // s1)				# remote http URI
tinfo (s1)

print ("\nFile URI:  " // s2)				# file URI
tinfo (s2)

print ("\nFile URI:  " // s3)				# file URI
tinfo (s3)
