#
#  Demonstrate ability to send targeted or broadcast SAMP messages.

# Set the test description string.
votest.descr = "Demonstrate ability to send targeted or broadcast messages"

print ("------------------------------------------------------------------")
print ("Req  2.1:  Users shall be able to send messages to specific")
print ("           clients or broadcast to all available clients.")
print ("------------------------------------------------------------------")


fcache init

string	res

# Execute the test commands.
print ("")
print ("")
print ("")


# Convert the data$logical to a local path.
s1 = data_url // "/usno-b.xml"
s2 = "file://" // data_path // "/usno-b.xml"
s3 = "file:///localhost" // data_path // "/usno-b.xml"


# Startup topcat as a recipient application.
if (sampAccess ("topcat") == yes) {

  ###########################################
  #  Broadcast to all clients.
  ###########################################

  # Command mode
  samp ("loadVOTable", "data$usno-b.xml")
  samp ("loadVOTable", s1)
  samp ("loadVOTable", s2)
  samp ("loadVOTable", s3)

  # Program mode
  res = sampLoadVOTable ("data$usno-b.xml")
  res = sampLoadVOTable (s1)
  res = sampLoadVOTable (s2)
  res = sampLoadVOTable (s3)


  ###########################################
  #  Targeted message to specific client.
  ###########################################

  # Command mode
  samp ("loadVOTable", "data$usno-b.xml", to="topcat")
  samp ("loadVOTable", s1,		  to="topcat")
  samp ("loadVOTable", s2,		  to="topcat")
  samp ("loadVOTable", s3,		  to="topcat")

  # Program mode
  res = sampLoadVOTable ("data$usno-b.xml", "topcat")
  res = sampLoadVOTable (s1, 		    "topcat")
  res = sampLoadVOTable (s2, 		    "topcat")
  res = sampLoadVOTable (s3, 		    "topcat")

} else {
    print ("Topcat is not running... skipping this test.")
    return
}

