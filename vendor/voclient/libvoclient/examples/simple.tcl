#!/usr/bin/tclsh

load ./voclient.so voclient
puts [voc_coneCaller "http://www.nofs.navy.mil/cgi-bin/vo_cone.cgi?CAT=USNO-B1&" 12. 12. 0.1 1]

