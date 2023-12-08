NAME=Adventure4i
BIN_LIB=ADVENT4I
DBGVIEW=*SOURCE
TGTRLS=V7R4M0
SHELL=/QOpenSys/usr/bin/qsh

#----------

#all: adventfm.dspf bldadvent.rpgle
all: adventfm.dspf advent.pgm
	@echo "Built all"

advent.pgm: bldadvent.rpgle adventfm.dspf advent_d.rpgle advent.rpgle
advent.rpgle: bldadvent.rpgle
#advent_d.rpgle: adventfm.dspf

#----------

%.dspf:
	system -s "CHGATR OBJ('./qddssrc/$*.dspf') ATR(*CCSID) VALUE(819)"
	-system -qi "CRTSRCPF FILE($(BIN_LIB)/QDDSSRC) MBR($*) RCDLEN(112)"
	system "CPYFRMSTMF FROMSTMF('./qddssrc/$*.dspf') TOMBR('/QSYS.lib/$(BIN_LIB).lib/QDDSSRC.file/$*.mbr') MBROPT(*REPLACE)"
	system "CRTDSPF FILE($(BIN_LIB)/$*) SRCFILE($(BIN_LIB)/QDDSSRC) SRCMBR($*) TEXT('$(NAME)')"

bldadvent.rpgle:
	system -s "CHGATR OBJ('./qrpglesrc/bldadvent.rpgle') ATR(*CCSID) VALUE(819)"
	liblist -a $(BIN_LIB);\
	system "CRTBNDRPG PGM($(BIN_LIB)/BLDADVENT) SRCSTMF('./qrpglesrc/bldadvent.rpgle') TEXT('$(NAME)') REPLACE(*YES) DBGVIEW($(DBGVIEW)) TGTRLS($(TGTRLS)) TGTCCSID(*SRC)"

advent.rpgle:
	liblist -a $(BIN_LIB);\
	system "CALL PGM($(BIN_LIB)/BLDADVENT)"
	system "CRTRPGMOD MODULE($(BIN_LIB)/ADVENT) SRCSTMF('./qrpglesrc/advent.rpgle') TEXT('$(NAME)') REPLACE(*YES) DBGVIEW($(DBGVIEW)) TGTRLS($(TGTRLS)) TGTCCSID(*SRC)"
	
%.rpgle:
	system -s "CHGATR OBJ('./qrpglesrc/$*.rpgle') ATR(*CCSID) VALUE(819)"
	liblist -a $(BIN_LIB);\
	system "CRTRPGMOD MODULE($(BIN_LIB)/$*) SRCSTMF('./qrpglesrc/$*.rpgle') TEXT('$(NAME)') REPLACE(*YES) DBGVIEW($(DBGVIEW)) TGTRLS($(TGTRLS)) TGTCCSID(*SRC)"

advent.pgm:
	liblist -a $(BIN_LIB);\
	system "CRTPGM PGM($(BIN_LIB)/ADVENT) ENTMOD(ADVENT) MODULE(ADVENT ADVENT_D) TEXT('$(NAME)') REPLACE(*YES) ACTGRP(*NEW) TGTRLS($(TGTRLS))"
