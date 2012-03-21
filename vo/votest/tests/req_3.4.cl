#
#  Cache Registry query results.

# Set the test description string.
votest.descr = "Cache Registry query results."

print ("------------------------------------------------------------------")
print ("Req  3.4:  Users shall have the option to cache query search")
print ("	   results to avoid redundant service calls.")
print ("------------------------------------------------------------------")


# Execute the test commands.
print ("This is demonstrated by using the REGDB task to retrieve record")
print ("information from the local registry cache.\n\n")

regdb ("url", alias="dss", verb+)
regdb ("url", alias="nvss", verb+)

