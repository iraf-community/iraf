#
#  Query multiple resource at one position, or one resource for multiple 
#  positions.

# Set the test description string.
votest.descr = "Query many svcs at one pos, or one svc for multiple positions."


print ("------------------------------------------------------------------")
print ("Req  4.2:  Users shall be able to query multiple resources at a")
print ("	   single position on the sky, OR a single resource for")
print ("	   multiple positions.")
print ("------------------------------------------------------------------")


fcache init

reset	clobber = yes

# Execute the test commands.

# Multiple resources at a single position
vodata ("gsc2.3,dss", objects="3c273", size=0.01, 
		output="STDOUT", format="csv", quiet+)

# Multiple position from a single position
vodata ("gsc2.3", objects="m82,m83,m84", size=0.01, 
		output="STDOUT", format="raw", quiet+)

