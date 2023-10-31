/* This module contains a compiler from the Instant language to JVM */

extern "C" {
#include "Parser.h"
#include "Printer.h"
#include "Absyn.h"
}

#include <fstream>
#include <filesystem>
#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include <sstream>
#include <stack>

using namespace std;

struct State {
  map<string, size_t> var_of_ident;
  size_t last_intermediate;
};

Exp get_exp2(Exp e) {
  if (e->kind == Exp_::is_ExpAdd) { return e->u.expadd_.exp_2; }
  if (e->kind == Exp_::is_ExpSub) { return e->u.expsub_.exp_2; }
  if (e->kind == Exp_::is_ExpMul) { return e->u.expmul_.exp_2; }
  if (e->kind == Exp_::is_ExpDiv) { return e->u.expdiv_.exp_2; }
  printf("Unimplemented!\n");
  exit(1);
}

Exp get_exp1(Exp e) {
  if (e->kind == Exp_::is_ExpAdd) { return e->u.expadd_.exp_1; }
  if (e->kind == Exp_::is_ExpSub) { return e->u.expsub_.exp_1; }
  if (e->kind == Exp_::is_ExpMul) { return e->u.expmul_.exp_1; }
  if (e->kind == Exp_::is_ExpDiv) { return e->u.expdiv_.exp_1; }
  printf("Unimplemented!\n");
  exit(1);
}



string compile(Exp e, State& state) {
  map<Exp, size_t> intermediate_of_exp;
  string result;
  stack<pair<Exp, bool>> todo;
  todo.push({e, false});
  while (!todo.empty()) {
    auto [e, is_visiting] = todo.top();
    todo.pop();

    if (!e) { continue; }

    if (e->kind == Exp_::is_ExpLit) {
      state.last_intermediate++;
      intermediate_of_exp[e] = state.last_intermediate;
      result += "%t" + to_string(state.last_intermediate) + " = add i32 0, " + to_string(e->u.explit_.integer_) + "\n";
      continue;
    } else if (e->kind == Exp_::is_ExpVar) {
      state.last_intermediate++;
      intermediate_of_exp[e] = state.last_intermediate;
      auto it = state.var_of_ident.find(e->u.expvar_.ident_);
      if (it == state.var_of_ident.end()) {
        cout << "Unbound variable: " << e->u.expvar_.ident_ << endl;
        exit(1);
      }
      result += "%t" + to_string(state.last_intermediate) + " = load i32, i32* %v" + to_string(it->second) + "\n";
      continue;
    }
    
    if (is_visiting) {
      state.last_intermediate++;
      intermediate_of_exp[e] = state.last_intermediate;

      auto op = e->kind == Exp_::is_ExpAdd ? "add" : e->kind == Exp_::is_ExpSub ? "sub" : e->kind == Exp_::is_ExpMul ? "mul" : "sdiv";
      auto res1_reg = "%t" + to_string(intermediate_of_exp[get_exp1(e)]);
      auto res2_reg = "%t" + to_string(intermediate_of_exp[get_exp2(e)]);
      result += "%t" + to_string(state.last_intermediate) + " = " + op + " " + "i32 " + res1_reg + ", " + res2_reg + "\n";
      // auto res1 = compile(get_exp1(e), state);
      
      // auto res2 = compile(get_exp2(e), state);
      // auto res2_reg = "%t" + to_string(state.last_intermediate);
      
      // return res1 + res2 + 
    } else {
      todo.push({e, true});
      todo.push({get_exp1(e), false});
      todo.push({get_exp2(e), false});
    }
  }
  return result;
}

string compile(Stmt s, State& state) {
  if (s->kind == Stmt_::is_SAss) {
    auto res = compile(s->u.sass_.exp_, state);
    auto it = state.var_of_ident.find(s->u.sass_.ident_);
    string var;
    bool alloca = false;
    if (it == state.var_of_ident.end()) {
      state.var_of_ident[s->u.sass_.ident_] = state.var_of_ident.size();
      alloca = true;
    }
    var = "%v" + to_string(state.var_of_ident[s->u.sass_.ident_]);
    if (alloca) {
      res += var + " = alloca i32\n";
    }
    return res + "store i32 " + "%t" + to_string(state.last_intermediate) + ", i32* " + var + "\n";
  } else {
    auto res = compile(s->u.sexp_.exp_, state);
    return res + "call void @printInt(i32 %t" + to_string(state.last_intermediate) + ")\n";
  }
}

string compile(Program p) {
  stringstream ss;
  ss << "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n";
  ss << "declare i32 @printf(i8*, ...)\n";
  ss << "define void @printInt(i32 %x) {\n";
  ss << "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n";
  ss << "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n";
  ss << "ret void\n";
  ss << "}\n";

  ss << "define i32 @main() {\n";
  ss << "entry:\n";
  State state;
  ListStmt stmt = p->u.prog_.liststmt_;
  while (stmt) {
    ss << compile(stmt->stmt_, state);
    stmt = stmt->liststmt_;
  }
  ss << "  ret i32 0\n";
  ss << "}\n";
  return ss.str();
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
    // get base of filename (without extension)
    // filename = argc > 1 ? argv[1] : stdin;
    string classname = argc > 1 ? filename : "out";
    string bytecode = compile(parse_tree);
    ofstream outfile;
    outfile.open((filesystem::path(classname).replace_extension("ll").string()).c_str());
    if (!outfile.is_open()) {
      cout << "Unable to open file " << classname << ".ll" << endl;
      exit(1);
    }
    outfile << bytecode;
    outfile.close();
    system(("llvm-as " + filesystem::path(classname).replace_extension("ll").string() + " -o " + filesystem::path(classname).replace_extension("bc").string()).c_str());
    // system(("lli " + classname + ".ll").c_str());
    free_Program(parse_tree);
    // printerQuit();
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

