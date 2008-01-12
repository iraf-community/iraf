# PARSER.COM -- Common block containing pointer to temporary tables

common / stkptr / ptgt, pact, pwrk

pointer	ptgt, pact, pwrk

