#
#  Query Registry with constraints.

# Set the test description string.
votest.descr = "Query Registry with constraints."

print ("------------------------------------------------------------------")
print ("Req  3.1:  Users shall be able to constrain the results of a search")
print ("           by supplying additional parameters to a query (e.g. ")
print ("	   bandpass, service type, etc).")
print ("------------------------------------------------------------------")


# Execute the test commands.
registry ("chandra", type="image", bandpass="x-ray", interactive=no)
