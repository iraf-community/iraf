#
#  Refer to a Resource by the Resource ShortName.

# Set the test description string.
votest.descr = "Refer to a Resource by the Resource ShortName"

print ("------------------------------------------------------------------")
print ("Req  3.3.2:  Users shall be able to refer to a VO Resource by the")
print ("	     Resource ShortName.")
print ("------------------------------------------------------------------")


fcache init


# Execute the test commands.

print ("")
print ("This is demonstrated with the GETCAT task by passing in a ShortName")
print ("as the resource parameter. ")
print ("")

getcat ("gsc2.3", "m12", size=0.01, display-, overplot-)
