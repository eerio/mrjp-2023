/* This module contains a compiler from the Instant language to JVM */

extern "C" {
#include "Parser.h"
#include "Printer.h"
#include "Absyn.h"
}

#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include <sstream>

using namespace std;

string compile(Exp e, map<string, int>& state) {
  switch (e->kind) {
    case Exp_::is_ExpLit:
      return "ldc " + to_string(e->u.explit_.integer_) + "\n";
    case Exp_::is_ExpAdd:
      return compile(e->u.expadd_.exp_1, state) + compile(e->u.expadd_.exp_2, state) + "iadd\n";
    case Exp_::is_ExpSub:
      return compile(e->u.expsub_.exp_1, state) + compile(e->u.expsub_.exp_2, state) + "isub\n";
    case Exp_::is_ExpMul:
      return compile(e->u.expmul_.exp_1, state) + compile(e->u.expmul_.exp_2, state) + "imul\n";
    case Exp_::is_ExpDiv:
      return compile(e->u.expdiv_.exp_1, state) + compile(e->u.expdiv_.exp_2, state) + "idiv\n";
    case Exp_::is_ExpVar:
      return "iload " + to_string(state[e->u.expvar_.ident_]) + "\n";
    default:
      printf("Unimplemented!\n");
      exit(1);
      break;
  }
}

string compile(Stmt s, map<string, int>& state) {
  map<string, int>::iterator it;
  switch (s->kind) {
    case Stmt_::is_SAss:
      it = state.find(s->u.sass_.ident_);
      if (it == state.end()) {
        state[s->u.sass_.ident_] = state.size();
      }
      return compile(s->u.sass_.exp_, state) + "istore " + to_string(state[s->u.sass_.ident_]) + "\n";
    case Stmt_::is_SExp:
      return compile(s->u.sexp_.exp_, state) + "getstatic java/lang/System/out Ljava/io/PrintStream;\n" + "swap\n" + "invokevirtual java/io/PrintStream/println(I)V\n";
    default:
      printf("Unimplemented!\n");
      exit(1);
      break;
  }
}

string compile(Program p) {
  map<string, int> state;
  vector<string> stmts;
  ListStmt stmt = p->u.prog_.liststmt_;
  while (stmt) {
    stmts.push_back(compile(stmt->stmt_, state));
    stmt = stmt->liststmt_;
  }
  string result = "";
  for (string s : stmts) {
    result += s;
  }
  return result;
}

string compile(Program p, string classname) {
  string result = ".class public " + classname + "\n.super java/lang/Object\n\n.method public <init>()V\n\taload_0\n\tinvokespecial java/lang/Object/<init>()V\n\treturn\n.end method\n\n.method public static main([Ljava/lang/String;)V\n";
  // result += "\t.limit stack 1000\n\t.limit locals 1000\n\n";
  result += compile(p);
  result += "\n\treturn\n.end method\n";
  return result;
}

int main(int argc, char ** argv) {
  FILE *input;
  Program parse_tree;
  char *filename = NULL;

  if (argc > 1) {
    filename = argv[1];
  }

  if (filename) {
    input = fopen(filename, "r");
    if (!input) {
      printf("File not found!\n");
      exit(1);
    }
  }
  else input = stdin;
  /* The default entry point is used. For other options see Parser.h */
  parse_tree = pProgram(input);
  if (parse_tree)
  {
    printf("\nParse Successful!\n");
    printf("\n[Abstract Syntax]\n");
    printf("%s\n\n", showProgram(parse_tree));
    printf("[Linearized Tree]\n");
    printf("%s\n\n", printProgram(parse_tree));
    printf("Compiling...\n");
    string classname = "Test";
    string bytecode = compile(parse_tree, classname);
    FILE *output = fopen((classname + ".j").c_str(), "w");
    fprintf(output, "%s", bytecode.c_str());
    fclose(output);
    system(("java -jar ../jasmin.jar " + classname + ".j").c_str());
    system(("java " + classname).c_str());
    free_Program(parse_tree);
    printerQuit();
    return 0;
  }

  return 1;
}



// /* File generated by the BNF Converter (bnfc 2.9.4.1). */

// /************************* Compiler Front-End Test *************************/
// /*                                                                         */
// /*  This test will parse a file, print the abstract syntax tree, and then  */
// /*  pretty-print the result.                                               */
// /*                                                                         */
// /***************************************************************************/

// #include <map>
// #include <string>


// void usage(void) {
//   printf("usage: Call with one of the following argument combinations:\n");
//   printf("\t--help\t\tDisplay this help message.\n");
//   printf("\t(no arguments)\tParse stdin verbosely.\n");
//   printf("\t(files)\t\tParse content of files verbosely.\n");
//   printf("\t-s (files)\tSilent mode. Parse content of files silently.\n");
// }

// using namespace std;
// using state_t = map<string, Integer>;

// void compile(Stmt s, FILE * out, state_t& state) {
//   switch (s->kind) {
//     case Stmt_::is_SAss:
//       compile(s->u.sass_.exp_, out, state);
//       fprintf(out, "istore %d\n", state[s->u.sass_.ident_]);
//       break;
//     case Stmt_::is_SExp:
//       compile(s->u.sexp_.exp_, out, state);
//       fprintf(out, "getstatic java/lang/System/out Ljava/io/PrintStream;\n");
//       fprintf(out, "swap\n");
//       fprintf(out, "invokevirtual java/io/PrintStream/println(I)V\n");
//       break;
//   }
// }


// void compile(Program p, FILE *out) {
//   ListStmt stmt = p->u.prog_.liststmt_;
//   while (stmt) {
//     compile(stmt->stmt_, out);
//     stmt = stmt->liststmt_;
//   }
// }

// // Function to compile an expression to JVM bytecode
// void compile(Exp e, FILE *out) {
//   switch (e->kind) {
//     case Exp_::is_ExpLit:
//       fprintf(out, "ldc %d\n", e->u.explit_.integer_);
//       break;
//     case Exp_::is_ExpAdd:
//       compile(e->u.expadd_.exp_1, out);
//       compile(e->u.expadd_.exp_2, out);
//       fprintf(out, "iadd\n");
//       break;
//     case Exp_::is_ExpSub:
//       compile(e->u.expadd_.exp_1, out);
//       compile(e->u.expadd_.exp_2, out);
//       fprintf(out, "isub\n");
//       break;
//     case Exp_::is_ExpMul:
//       compile(e->u.expmul_.exp_1, out);
//       compile(e->u.expmul_.exp_2, out);
//       fprintf(out, "imul\n");
//       break;
//     case Exp_::is_ExpDiv:
//       compile(e->u.expdiv_.exp_1, out);
//       compile(e->u.expdiv_.exp_2, out);
//       fprintf(out, "id\n");
//       break;
//     case Exp_::is_ExpVar:
//       fprintf(out, "iload %d\n", e->u.expvar_.integer_);
//       break;
//     default:
//       printf("Unimplemented!\n");
//       exit(1);
//       break;
// }

// int eval(Exp e, const state_t& state) {
//   state_t::const_iterator it;

//   switch (e->kind) {
//     case Exp_::is_ExpLit:
//       return e->u.explit_.integer_;
//     case Exp_::is_ExpAdd:
//       return eval(e->u.expadd_.exp_1, state) + eval(e->u.expadd_.exp_2, state);
//     case Exp_::is_ExpSub:
//       return eval(e->u.expadd_.exp_1, state) - eval(e->u.expadd_.exp_2, state);
//     case Exp_::is_ExpMul:
//       return eval(e->u.expmul_.exp_1, state) * eval(e->u.expmul_.exp_2, state);
//     case Exp_::is_ExpDiv:
//       return eval(e->u.expdiv_.exp_1, state) / eval(e->u.expdiv_.exp_2, state);
//     case Exp_::is_ExpVar:
//       it = state.find(e->u.expvar_.ident_);
//       if (it == state.end()) {
//         printf("Unbound variable: %s!");
//         exit(1);
//       }
//       return it->second;
//       break;
//     default:
//     printf("Unimplemented!\n");
//     exit(1);
//     break;
//   }
//   printf("Unimplemented!\n");
//   exit(1);
//   return 0;
// }

// void execute(Stmt, state_t&);

// void execute(Program p) {
//   state_t state;
//   ListStmt stmt = p->u.prog_.liststmt_;
  
//   while (stmt) {
//     execute(stmt->stmt_, state);
//     stmt = stmt->liststmt_; 
//   }
// }

// void execute(Stmt s, state_t& state) {
//   switch (s->kind) {
//     case Stmt_::is_SAss:
//       state[s->u.sass_.ident_] = eval(s->u.sass_.exp_, state);
//       break;
//     case Stmt_::is_SExp:
//       printf("%d\n", eval(s->u.sexp_.exp_, state));
//       break;
//   }
// }

// int main(int argc, char ** argv)
// {
//   FILE *input;
//   Program parse_tree;
//   int quiet = 0;
//   char *filename = NULL;

//   if (argc > 1) {
//     if (strcmp(argv[1], "-s") == 0) {
//       quiet = 1;
//       if (argc > 2) {
//         filename = argv[2];
//       } else {
//         input = stdin;
//       }
//     } else {
//       filename = argv[1];
//     }
//   }

//   if (filename) {
//     input = fopen(filename, "r");
//     if (!input) {
//       usage();
//       exit(1);
//     }
//   }
//   else input = stdin;
//   /* The default entry point is used. For other options see Parser.h */
//   parse_tree = pProgram(input);
//   if (parse_tree)
//   {
//     printf("\nParse Successful!\n");
//     if (!quiet) {
//       printf("\n[Abstract Syntax]\n");
//       printf("%s\n\n", showProgram(parse_tree));
//       printf("[Linearized Tree]\n");
//       printf("%s\n\n", printProgram(parse_tree));
//       printf("Executing...\n");
//       execute(parse_tree);
//     }
//     free_Program(parse_tree);
//     printerQuit();
//     return 0;
//   }

//   printerQuit();
//   return 1;
// }
