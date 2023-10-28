/* File generated by the BNF Converter (bnfc 2.9.4.1). */

/************************* Compiler Front-End Test *************************/
/*                                                                         */
/*  This test will parse a file, print the abstract syntax tree, and then  */
/*  pretty-print the result.                                               */
/*                                                                         */
/***************************************************************************/

#include <map>
#include <string>

extern "C" {
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "Parser.h"
#include "Printer.h"
#include "Absyn.h"
}

void usage(void) {
  printf("usage: Call with one of the following argument combinations:\n");
  printf("\t--help\t\tDisplay this help message.\n");
  printf("\t(no arguments)\tParse stdin verbosely.\n");
  printf("\t(files)\t\tParse content of files verbosely.\n");
  printf("\t-s (files)\tSilent mode. Parse content of files silently.\n");
}

using namespace std;
using state_t = map<string, Integer>;

int eval(Exp e, const state_t& state) {
  state_t::const_iterator it;

  switch (e->kind) {
    case Exp_::is_ExpLit:
      return e->u.explit_.integer_;
    case Exp_::is_ExpAdd:
      return eval(e->u.expadd_.exp_1, state) + eval(e->u.expadd_.exp_2, state);
    case Exp_::is_ExpSub:
      return eval(e->u.expadd_.exp_1, state) - eval(e->u.expadd_.exp_2, state);
    case Exp_::is_ExpMul:
      return eval(e->u.expmul_.exp_1, state) * eval(e->u.expmul_.exp_2, state);
    case Exp_::is_ExpDiv:
      return eval(e->u.expdiv_.exp_1, state) / eval(e->u.expdiv_.exp_2, state);
    case Exp_::is_ExpVar:
      it = state.find(e->u.expvar_.ident_);
      if (it == state.end()) {
        printf("Unbound variable: %s!");
        exit(1);
      }
      return it->second;
      break;
    default:
    printf("Unimplemented!\n");
    exit(1);
    break;
  }
  printf("Unimplemented!\n");
  exit(1);
  return 0;
}

void execute(Stmt, state_t&);

void execute(Program p) {
  state_t state;
  ListStmt stmt = p->u.prog_.liststmt_;
  
  while (stmt) {
    execute(stmt->stmt_, state);
    stmt = stmt->liststmt_; 
  }
}

void execute(Stmt s, state_t& state) {
  switch (s->kind) {
    case Stmt_::is_SAss:
      state[s->u.sass_.ident_] = eval(s->u.sass_.exp_, state);
      break;
    case Stmt_::is_SExp:
      printf("%d\n", eval(s->u.sexp_.exp_, state));
      break;
  }
}

int main(int argc, char ** argv)
{
  FILE *input;
  Program parse_tree;
  int quiet = 0;
  char *filename = NULL;

  if (argc > 1) {
    if (strcmp(argv[1], "-s") == 0) {
      quiet = 1;
      if (argc > 2) {
        filename = argv[2];
      } else {
        input = stdin;
      }
    } else {
      filename = argv[1];
    }
  }

  if (filename) {
    input = fopen(filename, "r");
    if (!input) {
      usage();
      exit(1);
    }
  }
  else input = stdin;
  /* The default entry point is used. For other options see Parser.h */
  parse_tree = pProgram(input);
  if (parse_tree)
  {
    printf("\nParse Successful!\n");
    if (!quiet) {
      printf("\n[Abstract Syntax]\n");
      printf("%s\n\n", showProgram(parse_tree));
      printf("[Linearized Tree]\n");
      printf("%s\n\n", printProgram(parse_tree));
      printf("Executing...\n");
      execute(parse_tree);
    }
    free_Program(parse_tree);
    printerQuit();
    return 0;
  }

  printerQuit();
  return 1;
}
