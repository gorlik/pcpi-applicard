# pcpi-applicard
Commented disassebmly of the Z80 PCPI Appli-card for apple II computers.
The included makefile will generate an exact match of the original ROM.
Z80asm and srecord are required to build it.

modified images can be tested using mame:
mame apple2gs -sl7 applicard -flop1 starcpm.po
