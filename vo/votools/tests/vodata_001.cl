#
#  VODATA task unit tests

# Set the test description string.
votest.descr = "VODATA task unit tests"

vodata.size = 0.05
vodata.output = "STDOUT"

delete ("/tmp/svcs.txt", verify-, >& "dev$null")
print ("hst\nchandra\ngsc2.3",  > "/tmp/svcs.txt")
delete ("/tmp/pos.txt", verify-, >& "dev$null")
print ("m31\nm32\nm33\n",	> "/tmp/pos.txt")

printf ("Example 1)\n")
vodata ("gsc2.3", "ngc1234")
vodata ("gsc2.3", "@/tmp/pos.txt")
vodata ("gsc2.3", "m31,m51,m93")
vodata ("@/tmp/svcs.txt", "@/tmp/pos.txt")
vodata ("hst,chandra,gsc2.3", "@/tmp/pos.txt")
vodata ("2mass-psc", "dev$ypix")

printf ("Example 2)\n")
vodata ("any", "IC10", type="image")

printf ("Example 3)\n")
vodata ("any", "abell2712", type="image", bandpass="x-ray")
