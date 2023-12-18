# Adventure4i

Colossal Cave Adventure for the IBM i operating system

This is an attempt to get the original Fortran IV source for the PDP-10 to run on IBM i.

Fortran IV is pretty funky and the PDP-10 version is a bit more so.  [Here](https://www.math-cs.gordon.edu/courses/cs323/FORTRAN/fortran.html) is a basic overview. 

Overview of parts:
- bldadvent.rpgle & system_calls.rpgle does a basic pass over the code translating it to RPGLE.  
- advent_a.rpgle and avent_b.rpgle contain manually translated code around input/output.  
- advent_d.rpgle and adventfm.dspf contain a the screen logic. 

### Installation

1. `git clone` this repository
2. Run `gmake` (available from yum)

### Usage instructions

After building, do CALL ADVENT.  

### License

MIT License. See file `LICENSE` in root of this repository.
