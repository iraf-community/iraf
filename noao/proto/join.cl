# JOIN -- Join two lists line by line.

procedure join (list1, list2)

file	list1		{prompt="First list to be joined"}
file	list2		{prompt="Second list to be joined"}

struct	*l1
struct	*l2

begin
	
	struct	line1
	struct	line2

	l1 = list1
	l2 = list2

	while ( fscan(l1, line1) != EOF && fscan(l2, line2) != EOF)
	    print (line1, " ", line2)
end
