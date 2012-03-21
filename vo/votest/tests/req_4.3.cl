#
#  ServiceURLs and resource names are interchangeable.

# Set the test description string.
votest.descr = "ServiceURLs and resource names are interchangeable."

print ("------------------------------------------------------------------")
print ("Req  4.3:  Users shall be able to specify a base ServiceURL to")
print (" 	   data-access tasks expecting a Resource parameter.")
print ("------------------------------------------------------------------")


fcache init

# Execute the test commands.

#  Get the base NED ServiceURL, use that to query a catalog service rather
#  than the name of the service.

regdb ("resolve", "veron")			
getcat (regdb.url, field="3c273", size=0.005, output="STDOUT", format="raw")

