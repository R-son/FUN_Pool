# FUN_PushSwap Checker

This project is an Epitech project realised in 2nd year of Epitech that checks if a sequence of push_swap operations effectively sorts a list of integers.
It takes a list of signed integers on the command line, and reads a single line of operations separated by spaces from the standard input.
It will display “OK” on the standard output (followed by a newline) if the final result is a sorted list (and the second empty), or “KO” otherwise.

Here are the different possible operations:

• sa

    swap the first two elements of l_a (nothing will happen if there aren’t enough elements).

• sb

    swap the first two elements of l_b (nothing will happen if there aren’t enough elements).

• sc

      sa and sb at the same time.

• pa

    take the first element from l_b and move it to the first position on the l_a list (nothing will happen if l_b is empty).
  
• pb

    take the first element from l_a and move it to the first position on the l_b list (nothing will happen if l_a is empty).

• ra

    rotate l_a toward the beginning, the first element will become the last.

• rb

    rotate l_b toward the beginning, the first element will become the last.

• rr
  ra and rb at the same time.

• rra
  rotate l_a toward the end, the last element will become the first.

• rrb
  rotate l_b toward the end, the last element will become the first.

• rrr
  rra and rrb at the same time.

The subject is available in the Subjects/ folder
