#-h-  outgo			  239  local   12/01/80  15:54:31
# outgo - output "goto	n"
   include  defs

subroutine outgo (n)

integer n
include COMMON_BLOCKS

       if (xfer == YES)
	   return
       call ogotos (n, NO)
end
