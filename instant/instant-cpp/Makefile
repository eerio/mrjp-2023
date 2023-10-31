## bnfc 2.9.4.1

CC = gcc -g -O2
CPP = g++ -std=c++17 -g -O2
CCFLAGS = --ansi -W -Wall -Wsign-conversion -Wno-unused-parameter -Wno-unused-function -Wno-unneeded-internal-declaration ${CC_OPTS}

BNFC = bnfc
BNFC_OPTS = --c

FLEX = flex
FLEX_OPTS = -Pinstant_

BISON = bison
BISON_OPTS = -t -pinstant_ -Wno-deprecated

OBJS = Absyn.o Buffer.o Lexer.o Parser.o Printer.o

.PHONY: clean all distclean
 
all : insc_jvm insc_llvm

clean:
	rm -f ${OBJS} Absyn.h Absyn.c Bison.h Buffer.h Buffer.c insc_jvm.o insc_llvm.o Instant.l Instant.y Lexer.c Parser.h Parser.c Printer.c Printer.h Skeleton.c Skeleton.h Test.c

distclean: clean
	rm -f insc_jvm insc_llvm

insc_jvm: ${OBJS} insc_jvm.o
	${CPP} ${OBJS} insc_jvm.o -o insc_jvm

insc_llvm: ${OBJS} insc_llvm.o
	${CPP} ${OBJS} insc_llvm.o -o insc_llvm

insc_jvm.o: src/insc_jvm.cpp Parser.h Printer.h Absyn.h
	g++ -g -Wall -Wsign-conversion -c src/insc_jvm.cpp -I .

insc_llvm.o: src/insc_llvm.cpp Parser.h Printer.h Absyn.h
	g++ -g -Wall -Wsign-conversion -c src/insc_llvm.cpp -I .

Absyn.h Absyn.c Buffer.h Buffer.c Instant.l Instant.y Parser.h Printer.c Printer.h Skeleton.c Skeleton.h: Instant.cf
	${BNFC} ${BNFC_OPTS} Instant.cf

Absyn.o : Absyn.c Absyn.h
	${CC} ${CCFLAGS} -c Absyn.c -Wno-int-conversion -Wno-implicit-function-declaration

Buffer.o : Buffer.c Buffer.h
	${CC} ${CCFLAGS} -c Buffer.c

Lexer.c : Instant.l
	${FLEX} ${FLEX_OPTS} -oLexer.c Instant.l

Parser.c Bison.h : Instant.y
	${BISON} ${BISON_OPTS} Instant.y -o Parser.c

Lexer.o : CCFLAGS+=-Wno-sign-conversion

Lexer.o : Lexer.c Bison.h
	${CC} ${CCFLAGS} -c Lexer.c

Parser.o : Parser.c Absyn.h Bison.h
	${CC} ${CCFLAGS} -c Parser.c

Printer.o : Printer.c Printer.h Absyn.h
	${CC} ${CCFLAGS} -c Printer.c
