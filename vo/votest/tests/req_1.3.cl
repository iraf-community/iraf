#
#  Demonstrate selection of column for tasks expecting a list.

# Set the test description string.
votest.descr = "Demonstrate selection of column for tasks expecting a list"

# Convert the data$logical to a local path.
s1 = data_url // "/sia.xml"
s2 = "file://" // data_path // "/sia.xml"
s3 = "file:///localhost" // data_path // "/sia.xml"

print ("------------------------------------------------------------------")
print ("Req  1.3:  Users shall be able to select a column in a VOTable")
print ("           for use in tasks that expect a list of values.")
print ("------------------------------------------------------------------")


fcache init
flpr 0

print (" ")
print ("    This test is demonstrated by using the @-file syntax to auto-")
print ("matically extract the image access reference  column from  an SIA")
print ("result table.")
print (" ")


# Suppress the image name to avoid a false failure because of checksum
# algorithm differences between platforms.
imstat.fields = "npix,mean,stddev,min,max"


# Execute the test commands.
print ("\nLogical Path:  data$sia.xml")			# logical path
imstat ("@data$sia.xml")

print ("\nHTTP URI:  " // s1)				# remote http URI
imstat ("@" // s1)

print ("\nFile URI:  " // s2)				# file URI
imstat ("@" // s2)

print ("\nFile URI:  " // s3)				# file URI
imstat ("@" // s3)



