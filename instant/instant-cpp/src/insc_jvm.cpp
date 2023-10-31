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
#include <fstream>

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

// get optimal height of stack for each node
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

string compile(Program p, string classname) {
  string result = ".class public " + classname + "\n.super java/lang/Object\n\n.method public <init>()V\n\taload_0\n\tinvokespecial java/lang/Object/<init>()V\n\treturn\n.end method\n\n.method public static main([Ljava/lang/String;)V\n";
  map<string, int> state;
  vector<string> stmts;
  ListStmt stmt = p->u.prog_.liststmt_;
  while (stmt) {
    stmts.push_back(compile(stmt->stmt_, state));
    stmt = stmt->liststmt_;
  }
  int max_stack = 0, stack = 0, locals = state.size() + 1; // +1 for *this
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
  Program parse_tree;
  string classname;
  if (argc > 1) {
    FILE *input = fopen(argv[1], "r");
    if (!input) {
      printf("File not found!\n");
      exit(1);
    }
    parse_tree = pProgram(input);
    fclose(input);
    classname = argv[1];
  }
  else {
    parse_tree = pProgram(stdin);
    classname = "out";
  };

  if (!parse_tree) { return 1; }

  string bytecode = compile(parse_tree, filesystem::path(classname).stem());
  ofstream outfile;
  outfile.open(filesystem::path(classname).replace_extension("j").c_str());
  if (!outfile.is_open()) {
    return 1;
  }
  outfile << bytecode;
  outfile.close();

  auto parent_path = filesystem::path(classname).parent_path().string();
  string out_folder = parent_path.empty() ? "." : parent_path;
  system(
    ("java -jar lib/jasmin.jar " 
    + filesystem::path(classname).replace_extension("j").string() 
    + " -d " + out_folder
    ).c_str()
  );
  free_Program(parse_tree);
  return 0;
}