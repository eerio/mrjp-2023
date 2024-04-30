## bnfc 2.9.4.1

CC = gcc -g -O2
CPP = g++ -std=c++17 -g -O2
CCFLAGS = --ansi -W -Wall -Wsign-conversion -Wno-unused-parameter -Wno-unused-function -Wno-unneeded-internal-declaration ${CC_OPTS}

BNFC = bnfc
BNFC_OPTS = --c

FLEX = flex
FLEX_OPTS = -Platte_

BISON = bison
BISON_OPTS = -t -platte_ -Wno-deprecated

OBJS = Absyn.o Buffer.o Lexer.o Parser.o Printer.o

.PHONY: clean all distclean
 
all : latc

clean:
	rm -f ${OBJS} Absyn.h Absyn.c Bison.h Buffer.h Buffer.c Latte.l Latte.y Lexer.c Parser.h Parser.c Printer.c Printer.h Skeleton.c Skeleton.h Test.c

distclean: clean
	rm -f latc

latc: ${OBJS} latc.o
	${CPP} ${OBJS} latc.o -o latc

latc.o: src/latc.cpp Parser.h Printer.h Absyn.h
	g++ -g -Wall -Wsign-conversion -c src/latc.cpp -I .

Absyn.h Absyn.c Buffer.h Buffer.c Latte.l Latte.y Parser.h Printer.c Printer.h Skeleton.c Skeleton.h: Latte.cf
	${BNFC} ${BNFC_OPTS} Latte.cf

Absyn.o : Absyn.c Absyn.h
	${CC} ${CCFLAGS} -c Absyn.c -Wno-int-conversion -Wno-implicit-function-declaration

Buffer.o : Buffer.c Buffer.h
	${CC} ${CCFLAGS} -c Buffer.c

Lexer.c : Latte.l
	${FLEX} ${FLEX_OPTS} -oLexer.c Latte.l

Parser.c Bison.h : Latte.y
	${BISON} ${BISON_OPTS} Latte.y -o Parser.c

Lexer.o : CCFLAGS+=-Wno-sign-conversion

Lexer.o : Lexer.c Bison.h
	${CC} ${CCFLAGS} -c Lexer.c

Parser.o : Parser.c Absyn.h Bison.h
	${CC} ${CCFLAGS} -c Parser.c

Printer.o : Printer.c Printer.h Absyn.h
	${CC} ${CCFLAGS} -c Printer.c
