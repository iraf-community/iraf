#
# Test for filename errors

# Set the test description string.
votest.descr = "Test for invalid table URL"
votest.has_err = yes

set cache = "/tmp/cache/"
fcache init

iferr {
    tlcol ("http://iraf.noao.edu/votest/usno-z.xml", >& "dev$null")
} then {
    print ("error in test")
}
fcache list
