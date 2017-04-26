#-h-  push			  249  local   12/01/80  15:54:34
# push - push ep onto argstk, return new pointer ap
   include  defs

   integer function push (ep, argstk, ap)
   integer ap, argstk (ARGSIZE), ep

   if (ap > ARGSIZE)
      call baderr ('arg stack overflow.')
   argstk (ap) = ep
   push = ap + 1
   return
   end
