# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# XORI -- Boolean exclusive or of two integer operands.

int procedure xori (a, b)

int	a, b
int	not(), and(), or()

begin
	return (or (and(a,not(b)), and(not(a),b)))
end


# XORS -- Boolean exclusive or of two short integer operands.

short procedure xors (a, b)

short	a, b
short	nots(), ands(), ors()

begin
	return (ors (ands(a,nots(b)), ands(nots(a),b)))
end


# XORL -- Boolean exclusive or of two long integer operands.

long procedure xorl (a, b)

long	a, b
long	notl(), andl(), orl()

begin
	return (orl (andl(a,notl(b)), andl(notl(a),b)))
end
