#
#  Refer to a Resource by the IVOA identifier.

# Set the test description string.
votest.descr = "Refer to a Resource by the IVOA identifier."

print ("------------------------------------------------------------------")
print ("Req  3.3.1:  Users shall be able to refer to a VO Resource by the")
print ("	     IVOA identifier.")
print ("------------------------------------------------------------------")


fcache init


# Execute the test commands.

print ("")
print ("This is demonstrated with the GETCAT task by passing in an IVORN")
print ("as the resource parameter. ")
print ("")

getcat ("ivo://archive.stsci.edu/gsc/gsc2.3", "dev$ypix", size=0.005, 
		display-, overplot-)
