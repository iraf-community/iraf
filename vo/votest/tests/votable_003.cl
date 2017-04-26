#
#  Get a value from a VOTable cell

# Set the test description string.
votest.descr = "TABLES support: Get a value from a VOTable cell"

# Convert the data$logical to a local path.
s1 = data_url // "/usno-b.xml"
s2 = "file://" // data_path // "/usno-b.xml"
s3 = "file:///localhost" // data_path // "/usno-b.xml"


fcache init

# Execute the test commands.
print ("\nLogical Path:  data$usno-b.xml")		# logical path
tabpar ("data$usno-b.xml", "I2", 10)
print ("cell value at (I2,10) = ", tabpar.value)

print ("\nHTTP URI:  " // s1)				# remote http URI
tabpar (s1, "I2", 10)
print ("cell value at (I2,10) = ", tabpar.value)

print ("\nFile URI:  " // s2)				# file URI
tabpar (s2, "I2", 10)
print ("cell value at (I2,10) = ", tabpar.value)

print ("\nFile URI:  " // s3)				# file URI
tabpar (s3, "I2", 10)
print ("cell value at (I2,10) = ", tabpar.value)
