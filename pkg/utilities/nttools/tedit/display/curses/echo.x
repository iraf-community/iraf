include "window.h"

# ECHO -- Turn character echoing on
#
# B.Simon	02-Oct-90	Original

procedure echo ()

#--
include "window.com"

begin
	echoed = YES
end

procedure noecho ()

#--
include "window.com"

begin
	echoed = NO
end
