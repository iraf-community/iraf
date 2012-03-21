#
#  Access to <PARAM> and <INFO> table elements.

# Set the test description string.
votest.descr = "Access to <PARAM> and <INFO> table elements"

# Convert the data$logical to a local path.
s1 = data_url // "/usno-b.xml"
s2 = "file://" // data_path // "/usno-b.xml"
s3 = "file:///localhost" // data_path // "/usno-b.xml"

print ("------------------------------------------------------------------")
print ("Req  1.1:  Users shall be able to access the <PARAM> and <INFO>")
print ("           elements of a VOTable as standard table header information.")
print ("------------------------------------------------------------------")


fcache init

# Execute the test commands.
print ("\nLogical Path:  data$usno-b.xml")		# logical path
thedit ("data$usno-b.xml", "parnam*,parval*,infnam*,infval*",".",del-,show+)

print ("\nHTTP URI:  " // s1)				# remote http URI
thedit (s1, "parnam*,parval*,infnam*,infval*",".",del-,show+)


print ("\nFile URI:  " // s2)				# file URI
thedit (s2, "parnam*,parval*,infnam*,infval*",".",del-,show+)


print ("\nFile URI:  " // s3)				# file URI
thedit (s3, "parnam*,parval*,infnam*,infval*",".",del-,show+)

