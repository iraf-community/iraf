#
#  Demonstrate support for latest VOTable standard spec.

# Set the test description string.
votest.descr = "Demonstrate support for latest VOTable standard spec."

# Convert the data$logical to a local path.
s1 = data_url // "/votable_1.2.xml"
s2 = "file://" // data_path // "/votable_1.2.xml"
s3 = "file:///localhost" // data_path // "/votable_1.2.xml"

print ("------------------------------------------------------------------")
print ("Req  1.4:  VOTable Interface code will support the IVOA Standard")
print ("           specification of the VOTable format at time of release.")
print ("------------------------------------------------------------------")


fcache init

print (" ")
print ("    The requirement is demonstrated using a VOTable v1.2 document")
print ("retrieved from the CADC and modified to add v1.2-specific features:")
print (" ")
print ("    - INFO elements before </TABLE>, </RESOURCE> and </VOTABLE>")
print ("    - INFO elements w/ 'utype', 'ucd', 'ref' and 'unit' attributes")
print ("    - FIELDref and PARAMref elements w/ 'ucd' and 'utype' attributes")
print (" ")
print ("The parse will print an error if these v1.2-specifics aren't supported")
print (" ")
print (" ")


# Execute the test commands.
print ("\nLogical Path:  data$votable_1.2.xml")		# logical path
tlcol ("data$votable_1.2.xml")

print ("\nHTTP URI:  " // s1)				# remote http URI
tlcol (s1)

print ("\nFile URI:  " // s2)				# file URI
tlcol (s2)

print ("\nFile URI:  " // s3)				# file URI
tlcol (s3)

