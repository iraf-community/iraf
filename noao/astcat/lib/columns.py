#!/usr/bin/env python3
#
#  COLUMNS.PY -- Utility script to help in creating new catdb.dat service
#  entried.
#
#  The script assumes the input is tabular output such as a query result from
#  a Postgres (or other) database.  The ASTCAT services rely on the Simple
#  Cone Search services from NOIRLab that permit a 'text' FORMAT option that
#  directly returns the database query text, e.g. a table of the form:
#
#       +---------------+------------+-----------+
#       | id            | raj2000    | dej2000   |
#       +---------------+------------+-----------+
#       | 1350-18887518 | 359.424648 | 45.007714 |
#       | 1350-18888231 | 359.434314 | 45.006792 |
#       | 1350-18885607 | 359.400464 | 45.022678 |
#       | 1350-18886666 | 359.413564 | 45.018995 |
#
#  The second line is the header containing the column names. The script
#  outputs the name of each column, the character offset in the line to the
#  start of that column and its width. Since the NOIRLab search services
#  return the entire table (which may be hundreds of columns), finding the
#  formatting information necessary for the catdb configuration can be
#  challenging.


import sys

def parse_table_structure(filename):
    with open(filename, "r") as f:
        lines = f.readlines()

    if len(lines) < 2:
        raise ValueError("File must contain at least 2 lines (hdr + 1 row).")

    # Line 2 contains the column names
    header_line = lines[0].rstrip("\n")

    # Split but keep spacing by calculating indexes directly
    col_starts = []
    col_widths = []
    col_names = []

    # Split on '|', strip only outer whitespace for names
    parts = header_line.split("|")

    # Clean leading/trailing spaces and compute offsets
    offset = 0
    for i, part in enumerate(parts):
        # First element (before first '|') may have trailing space
        text = part.strip()
        if text == "" and i == 0:
            # skip empty leftmost if it happens
            offset += len(part) + 1
            continue

        col_names.append(text)
        col_starts.append(offset)
        col_widths.append(len(part))

        # Move offset: part length + 1 for the '|' char
        offset += len(part) + 1

    # Print results.  Note the format will need to be modified and units
    # added by hand.
    for name, start, width in zip(col_names, col_starts, col_widths):
        print(f"    {name} {start+1}  {width}  INDEF %s")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <filename>")
        sys.exit(1)

    parse_table_structure(sys.argv[1])

