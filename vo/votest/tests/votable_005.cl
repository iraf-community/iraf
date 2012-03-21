#
#  Print VOTable column statistics

# Set the test description string.
votest.descr = "TABLES support: Print VOTable column statistics"

# Convert the data$logical to a local path.
s1 = data_url // "/usno-b.xml"
s2 = "file://" // data_path // "/usno-b.xml"
s3 = "file:///localhost" // data_path // "/usno-b.xml"


fcache init

# Execute the test commands.
print ("\nLogical Path:  data$usno-b.xml")		# logical path
tstat ("data$usno-b.xml", "B1")

print ("\nHTTP URI:  " // s1)				# remote http URI
tstat (s1, "B1")

print ("\nFile URI:  " // s2)				# file URI
tstat (s2, "B1")

print ("\nFile URI:  " // s3)				# file URI
tstat (s3, "B1")
