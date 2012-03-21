#!/bin/csh -f

set n = 0
echo > /tmp/z/zzlist
foreach i (`cat /tmp/_im.s`)
   echo $n "  " $i

   echo "b xerror_"		 		 > /tmp/c
   echo "b zpanic_"				>> /tmp/c
   echo "run vodata resources='"$i"' @zz.dpar"	>> /tmp/c
   echo "thread apply all bt"			>> /tmp/c
   echo "quit"					>> /tmp/c

   echo $n "  " $i				>> /tmp/z/zzlist
   gdb -x /tmp/c ./xx_votools.e			>& /tmp/z/$n.out

   set n = `expr $n + 1`
end
