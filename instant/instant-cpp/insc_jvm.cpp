/* This module contains a compiler from the Instant language to JVM */

extern "C" {
#include "Parser.h"
#include "Printer.h"
#include "Absyn.h"
}

#include <filesystem>
#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include <sstream>
#include <stack>

using namespace std;

map<Exp, size_t> height_of;

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

void set_height(Exp e) {
  // iterative post-order
  stack<pair<Exp, bool>> stack;
  stack.push({e, false});
  while (!stack.empty()) {
    auto [node, is_visiting] = stack.top();
    stack.pop();

    if (!node) { continue; }
    if (node->kind == Exp_::is_ExpLit || node->kind == Exp_::is_ExpVar) {
      height_of[node] = 1;
      continue;
    }

    if (is_visiting) {
      auto h1 = height_of[get_exp1(node)];
      auto h2 = height_of[get_exp2(node)];
      height_of[node] = min(max(h1, h2 + 1), max(h1 + 1, h2));
    } else {
      stack.push({node, true});
      stack.push({get_exp1(node), false});
      stack.push({get_exp2(node), false});
    }
  }
}


string compile(Exp e, map<string, int>& state) {
  string result;
  stack<tuple<Exp, bool, bool>> todo;
  todo.push({e, false, false});
  while (!todo.empty()) {
    auto [node, is_visiting, was_swapped] = todo.top();
    todo.pop();

    if (!node) { continue; }
    if (node->kind == Exp_::is_ExpLit) {
      // use optimal jvm isntruction based on value of literal
      string int_expr = to_string(node->u.explit_.integer_);
      if (node->u.explit_.integer_ <= 5) {
        result += "iconst_" + int_expr + "\n";
      }
      else if (node->u.explit_.integer_ <= 127) {
        result += "bipush " + int_expr + "\n";
      }
      else if (node->u.explit_.integer_ <= 32767) {
        result += "sipush " + int_expr + "\n";
      }
      else {
        result += "ldc "    + int_expr + "\n";
      }
      continue;
    } else if (node->kind == Exp_::is_ExpVar) {
      if (state[node->u.expvar_.ident_] <= 3) {
        result += "iload_" + to_string(state[node->u.expvar_.ident_]) + "\n";
      }
      else {
        result += "iload " + to_string(state[node->u.expvar_.ident_]) + "\n";
      }
      continue;
    }

    if (is_visiting) {
      if (node->kind == Exp_::is_ExpAdd) {
        result += "iadd\n";
      }
      else if (node->kind == Exp_::is_ExpSub) {
        if (!was_swapped) {
          result += "isub\n";
        } else {
          result += "swap\nisub\n";
        }
      }
      else if (node->kind == Exp_::is_ExpMul) {
        result += "imul\n";
      }
      else if (node->kind == Exp_::is_ExpDiv) {
        if (!was_swapped) {
          result += "idiv\n";
        } else {
          result += "swap\nidiv\n";
        }
      }
    } else {
      auto h1 = height_of[get_exp1(node)];
      auto h2 = height_of[get_exp2(node)];
      if (h1 >= h2) {
        todo.push({node, true, false});
        todo.push({get_exp2(node), false, false});
        todo.push({get_exp1(node), false, false});
      } else {
        todo.push({node, true, true});
        todo.push({get_exp1(node), false, false});
        todo.push({get_exp2(node), false, false});
      }
    }
  }

  return result;

  // string result;
  switch (e->kind) {
    case Exp_::is_ExpLit:
      // use optimal jvm isntruction based on value of literal
      if (e->u.explit_.integer_ == 0) {
        return "iconst_0\n";
      }
      else if (e->u.explit_.integer_ == 1) {
        return "iconst_1\n";
      }
      else if (e->u.explit_.integer_ == 2) {
        return "iconst_2\n";
      }
      else if (e->u.explit_.integer_ == 3) {
        return "iconst_3\n";
      }
      else if (e->u.explit_.integer_ == 4) {
        return "iconst_4\n";
      }
      else if (e->u.explit_.integer_ == 5) {
        return "iconst_5\n";
      }
      else if (e->u.explit_.integer_ >= -128 && e->u.explit_.integer_ <= 127) {
        return "bipush " + to_string(e->u.explit_.integer_) + "\n";
      }
      else if (e->u.explit_.integer_ >= -32768 && e->u.explit_.integer_ <= 32767) {
        return "sipush " + to_string(e->u.explit_.integer_) + "\n";
      }
      else {
        return "ldc " + to_string(e->u.explit_.integer_) + "\n";
      }
    case Exp_::is_ExpVar:
      if (state[e->u.expvar_.ident_] <= 3) {
        return "iload_" + to_string(state[e->u.expvar_.ident_]) + "\n";
      }
      else {
        return "iload " + to_string(state[e->u.expvar_.ident_]) + "\n";
      }
    default:
      if (height_of[get_exp2(e)] > height_of[get_exp1(e)]) {
        result += compile(get_exp2(e), state) + compile(get_exp1(e), state);
        if (e->kind == Exp_::is_ExpAdd) {
          result += "iadd\n";
        }
        else if (e->kind == Exp_::is_ExpSub) {
          result += "swap\nisub\n";
        }
        else if (e->kind == Exp_::is_ExpMul) {
          result += "imul\n";
        }
        else if (e->kind == Exp_::is_ExpDiv) {
          result += "swap\nidiv\n";
        }
      }
      else {
        result += compile(get_exp1(e), state) + compile(get_exp2(e), state);
        if (e->kind == Exp_::is_ExpAdd) {
          result += "iadd\n";
        }
        else if (e->kind == Exp_::is_ExpSub) {
          result += "isub\n";
        }
        else if (e->kind == Exp_::is_ExpMul) {
          result += "imul\n";
        }
        else if (e->kind == Exp_::is_ExpDiv) {
          result += "idiv\n";
        }
        else {
          printf("Unimplemented!\n");
          exit(1);
        }
        // return compile(e->u.expadd_.exp_1, state) + compile(e->u.expadd_.exp_2, state) + "iadd\n";
      }
      return result;
    //     return compile(e->u.expadd_.exp_2, state) + compile(e->u.expadd_.exp_1, state) + "iadd\n";
    //   // return compile(e->u.expadd_.exp_1, state) + compile(e->u.expadd_.exp_2, state) + "iadd\n";
    // case Exp_::is_ExpSub:
    //   return compile(e->u.expsub_.exp_1, state) + compile(e->u.expsub_.exp_2, state) + "isub\n";
    // case Exp_::is_ExpMul:
    //   return compile(e->u.expmul_.exp_1, state) + compile(e->u.expmul_.exp_2, state) + "imul\n";
    // case Exp_::is_ExpDiv:
    //   return compile(e->u.expdiv_.exp_1, state) + compile(e->u.expdiv_.exp_2, state) + "idiv\n";
    
    // default:
    //   printf("Unimplemented!\n");
    //   exit(1);
    //   break;
  }
}

string compile(Stmt s, map<string, int>& state) {
  map<string, int>::iterator it;
  string result;
  switch (s->kind) {
    case Stmt_::is_SAss:
      it = state.find(s->u.sass_.ident_);
      if (it == state.end()) {
        state[s->u.sass_.ident_] = state.size();
      }
      set_height(s->u.sass_.exp_);
      result = compile(s->u.sass_.exp_, state);
      if (state[s->u.sass_.ident_] <= 3) {
        result += "istore_" + to_string(state[s->u.sass_.ident_]) + "\n";
      } else {
        result += "istore " + to_string(state[s->u.sass_.ident_]) + "\n";
      }
      return result;
    case Stmt_::is_SExp:
      set_height(s->u.sexp_.exp_);
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
  // calculate and declare stack size and number of local variables
  map<string, int> state;
  vector<string> stmts;
  ListStmt stmt = p->u.prog_.liststmt_;
  while (stmt) {
    stmts.push_back(compile(stmt->stmt_, state));
    stmt = stmt->liststmt_;
  }
  int max_stack = 0, stack = 0, locals = state.size() + 1;
  for (string s : stmts) {
    stringstream ss(s);
    string line;
    while (getline(ss, line)) {
      if (line.find("iadd") != string::npos || line.find("isub") != string::npos || line.find("imul") != string::npos || line.find("idiv") != string::npos) {
        stack--;
      }
      else if (line.find("iconst") != string::npos) {
        stack++;
      }
      else if (line.find("bipush") != string::npos) {
        stack++;
      }
      else if (line.find("sipush") != string::npos) {
        stack++;  
      }
      else if (line.find("istore") != string::npos) {
        stack--;
      }
      else if (line.find("iload") != string::npos) {
        stack++;
      }
      else if (line.find("swap") != string::npos) {
       
      }
      else if (line.find("getstatic") != string::npos) {
        stack++;
      }
      else if (line.find("invokevirtual java/io/PrintStream/println(I)V") != string::npos) {
        stack -= 2;
      }
      else if (line.find("ldc") != string::npos) {
        stack++;
      }
      else {
        printf("Unimplemented!\n");
        exit(1);
      }
      max_stack = max(max_stack, stack);
    }
  }
  result += "\t.limit stack " + to_string(max_stack) + "\n\t.limit locals " + to_string(locals) + "\n\n";
  for (string s : stmts) {
    result += s;
  }
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
    string classname = argc > 1 ? filename : "out";
    string bytecode = compile(parse_tree, filesystem::path(classname).stem());
    FILE *output = fopen((filesystem::path(classname).replace_extension("j").string()).c_str(), "w");
    fprintf(output, "%s", bytecode.c_str());
    fclose(output);
    cout << "executing: " << ("java -jar ../jasmin.jar " + filesystem::path(classname).replace_extension("j").string() + " -d " + filesystem::path(classname).parent_path().string()) << '\n';
    if (!filesystem::path(classname).parent_path().string().empty()) {
      system(("java -jar lib/jasmin.jar " + filesystem::path(classname).replace_extension("j").string() + " -d " + filesystem::path(classname).parent_path().string()).c_str());
    } else {
      system(("java -jar lib/jasmin.jar " + filesystem::path(classname).replace_extension("j").string() + " -d .").c_str());
    }
    // system(("java -jar ../jasmin.jar " + filesystem::path(classname).replace_extension("j").string() + "-d ").c_str());
    // system(("java " + classname).c_str());
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

