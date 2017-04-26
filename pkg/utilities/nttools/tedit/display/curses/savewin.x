include "window.h"

# SAVEWIN -- Save characters under window when a window is created
#
# B.Simon	18-Oct-90	Original

procedure savewin ()

#--
include "window.com"

begin
	saved = YES
end

procedure nosavewin ()

#--
include "window.com"

begin
	saved = NO
end
