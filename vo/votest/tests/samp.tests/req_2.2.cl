#
#  Demonstrate ability to execute remote IRAF cmds via SAMP

# Set the test description string.
votest.descr = "Demonstrate ability to execute remote IRAF cmds via SAMP"

print ("------------------------------------------------------------------")
print ("Req  2.2:  Users shall be able to execute IRAF tasks and set or")
print ("           retrieve information in/from the IRAF environment via")
print ("           SAMP messaging from clients that implement the required")
print ("           message types.")
print ("------------------------------------------------------------------")


string  res

# Execute the test commands.
print ("")
print ("")
print ("")


###########################################
#  Broadcast to all clients.
###########################################

# Remote command execution
res = sampCmdExec ("imstat dev$pix")

# Environment Set/Get
res = sampEnvSet ("foo", "bar")
res = sampEnvGet ("foo")
print ("Result = " // res)

# Task Parameter Set/Get
res = sampParamSet ("imstat.images", "dev$pix")
res = sampParamGet ("imstat.images")
print ("Parameter = " // res)



###########################################
#  Targeted message to specific client.
###########################################


# Remote command execution
res = sampCmdExec ("imstat dev$pix", 		"iraf2")

# Environment Set/Get
res = sampEnvSet ("foo", "bar", 		"iraf2")
res = sampEnvGet ("foo", 			"iraf2")
print ("Result = " // res)

# Task Parameter Set/Get
res = sampParamSet ("imstat.images", "dev$pix", "iraf2")
res = sampParamGet ("imstat.images", 		"iraf2")
print ("Parameter = " // res)



