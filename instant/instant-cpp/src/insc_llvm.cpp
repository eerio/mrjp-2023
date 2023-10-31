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
  // number of register of intermediate result
  map<Exp, size_t> intermediate_of_exp;
  string result;
  // iterative post-order setup
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
    bool alloca = false;
    auto it = state.var_of_ident.find(s->u.sass_.ident_);
    if (it == state.var_of_ident.end()) {
      state.var_of_ident[s->u.sass_.ident_] = state.var_of_ident.size();
      alloca = true;
    }
    string var = "%v" + to_string(state.var_of_ident[s->u.sass_.ident_]);
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

  string bytecode = compile(parse_tree);
  ofstream outfile;
  outfile.open(filesystem::path(classname).replace_extension("ll").c_str());
  if (!outfile.is_open()) {
    return 1;
  }
  outfile << bytecode;
  outfile.close();
  system(
    ("llvm-as " 
    + filesystem::path(classname).replace_extension("ll").string() 
    + " -o " + filesystem::path(classname).replace_extension("bc").string()
    ).c_str()
  );

  free_Program(parse_tree);
  return 0;
}