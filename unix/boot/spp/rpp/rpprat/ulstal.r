#-h-  ulstal			  268  local   12/01/80  15:55:09
# ulstal - install lower and upper case versions of symbol
   include  defs

   subroutine ulstal (name, defn)
   character name (ARB), defn (ARB)

   include COMMON_BLOCKS

   call entdef (name, defn, deftbl)
   call upper (name)
   call entdef (name, defn, deftbl)

   return
   end
