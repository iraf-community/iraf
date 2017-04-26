#
#  Refer to a Resource by an 'anonymous' query.

# Set the test description string.
votest.descr = "Refer to a Resource by an 'anonymous' query."

print ("------------------------------------------------------------------")
print ("Req  3.3.3:  Users shall be able to refer to a VO Resource by the")
print ("	     'anonymous' result of a Registry query.")
print ("------------------------------------------------------------------")

fcache init


# Execute the test commands.
print ("")
print ("Process requests for all x-ray resources in our personal database. ")
print ("")
print ("")

# Search for the X-Ray resource.
regdb ("resolve", bandpass="x-ray")

if (regdb.svctype == "I") {
    getimg (regdb.alias, "dev$ypix", size=0.02)
} else if (regdb.svctype == "C") {
    getcat (regdb.alias, "dev$ypix", size=0.02)
} else {
    printf ("Unknown service type for '%s'\n", regdb.alias)
}
