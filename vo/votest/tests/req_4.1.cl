#
#  Query via the footprint of a desktop image.

# Set the test description string.
votest.descr = "Query via the footprint of a desktop image."

print ("------------------------------------------------------------------")
print ("Req  4.1:  Users shall be able to use the WCS footprint of a 2-D")
print ("           image as the basis of a data access query.")
print ("------------------------------------------------------------------")


fcache init

# Execute the test commands.
print ("")
print ("Query the GSC2.3 catalog for the footprint of the dev$ypix image")
print ("")

getcat ("gsc2.3", "dev$ypix", format="ascii", output="STDOUT")
