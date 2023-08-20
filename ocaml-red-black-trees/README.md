# Red-Black Trees in OCaml

This is code for a presention regarding Red-Black Trees for Computer Science Club.
The code is intended to be run from the command line.  It should only output something
on error.  The test cases are not intended to be complete, and are insufficient to 
test all code paths.  It would be better to have functions checking invariants than 
using these corny static tests.  Some optimizations, such as splitting left and 
right insert and delete cases, have been performed.  There are likely improvements
that could be made to the matching, as the point of this code is clarity over 
efficiency.  The functor case is provided since it is more realistic in practice to
support arbitrary types, though the bulk of the code uses naive ordering of 
intrinsically ordered types using <, > and =, despite the use of a comparison function
yielding more expressive code.


Here is a cool visualization tool for the normal operations on Red Black trees:

     https://www.cs.usfca.edu/~galles/visualization/RedBlack.html
