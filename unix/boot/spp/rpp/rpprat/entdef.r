#-h-  entdef			  387  local   12/01/80  15:53:51
# entdef - enter a new symbol definition, discarding any old one
   include  defs

   subroutine entdef (name, defn, table)
   character name (MAXTOK), defn (ARB)
   pointer table

   integer lookup

   pointer text
   pointer sdupl

   if (lookup (name, text, table) == YES)
      call dsfree (text)   # this is how to do UNDEFINE, by the way
   call enter (name, sdupl (defn), table)

   return
   end
