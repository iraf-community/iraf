# TOKDATA.INC -- Character classes for ctotok.x.
# identifier=1, number=2, operator=3, punctuation=4, string=5, unknown=8

#				  !   "   #   $   %   &   '   (   )   *
data	(class(i),i= 1,10)	/ 3,  5,  3,  1,  3,  3,  5,  4,  4,  3/

#				  +   ,   -   .   /   0   1   2   3   4
data	(class(i),i=11,20)	/ 3,  4,  3,  1,  3,  1,  1,  1,  1,  1/

#				  5   6   7   8   9   :   ;   <   =   >
data	(class(i),i=21,30)	/ 1,  1,  1,  1,  1,  4,  4,  3,  3,  3/

#				  ?   @   A   B   C   D   E   F   G   H
data	(class(i),i=31,40)	/ 3,  3,  1,  1,  1,  1,  1,  1,  1,  1/

#				  I   J   K   L   M   N   O   P   Q   R
data	(class(i),i=41,50)	/ 1,  1,  1,  1,  1,  1,  1,  1,  1,  1/

#				  S   T   U   V   W   X   Y   Z   [   \
data	(class(i),i=51,60)	/ 1,  1,  1,  1,  1,  1,  1,  1,  4,  4/

#				  ]   ^   _   `   a   b   c   d   e   f
data	(class(i),i=61,70)	/ 4,  3,  1,  3,  1,  1,  1,  1,  1,  1/

#				  g   h   i   j   k   l   m   n   o   p
data	(class(i),i=71,80)	/ 1,  1,  1,  1,  1,  1,  1,  1,  1,  1/

#				  q   r   s   t   u   v   w   x   y   z
data	(class(i),i=81,90)	/ 1,  1,  1,  1,  1,  1,  1,  1,  1,  1/

#				  {   |   }   ~  DEL
data	(class(i),i=91,96)	/ 4,  3,  4,  3,  8, EOS/
