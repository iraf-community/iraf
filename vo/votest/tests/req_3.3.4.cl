#
#  Refer to a Resource by a personal alias.

# Set the test description string.
votest.descr = "Refer to a Resource by a personal alias."

print ("------------------------------------------------------------------")
print ("Req  3.3.4:  Users shall be able to refer to a VO Resource by a")
print ("	     personal alias.")
print ("------------------------------------------------------------------")


fcache init


# Execute the test commands.

print ("")
print ("This is demonstrated with the GETCAT task by passing in a personal")
print ("name as the resource parameter. ")
print ("")

getcat ("gsc2.3", "dev$ypix", size=0.02, display-, overplot-)
