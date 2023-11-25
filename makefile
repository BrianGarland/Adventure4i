NAME=Adventure4i
BIN_LIB=ADVENT4I
DBGVIEW=*SOURCE
TGTRLS=V7R3M0
SHELL=/QOpenSys/usr/bin/qsh

#----------

all: bldadvent.rpgle
	@echo "Built all"

%.rpgle:
	liblist -a $(BIN_LIB);\
	system "CRTBNDRPG PGM($(BIN_LIB)/$*) SRCSTMF('QSOURCE/$*.rpgle') TEXT('$(NAME)') REPLACE(*YES) DBGVIEW($(DBGVIEW)) TGTRLS($(TGTRLS))"

%.dspf:
	-system -qi "CRTSRCPF FILE($(BIN_LIB)/QDDSSRC) MBR($*) RCDLEN(112)"
	system "CPYFRMSTMF FROMSTMF('qddssrc/$*.dspf') TOMBR('/QSYS.lib/$(BIN_LIB).lib/QDDSSRC.file/$*.mbr') MBROPT(*REPLACE)"
	system "CRTDSPF FILE($(BIN_LIB)/$*) SRCFILE($(BIN_LIB)/QSOURCE) SRCMBR($*) TEXT('$(NAME)')"
	
#----------

clean:
	system "CLRLIB LIB($(BIN_LIB))"
