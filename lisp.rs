use std::io::Write;
use std::io;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Clone, PartialEq, Eq, Hash)]
struct LRef(i64);

struct Arena { last: i64, data: HashMap<LRef, LObj> }

impl Arena {
  fn new() -> Arena { Arena { last: 0, data: HashMap::new() } }

  fn get(&self, key: &LRef) -> LObj
      { match self.data.get(&key) { Some(val) => val.clone(), _ => LObj::Nil } }
  fn set(&mut self, key: LRef, val: LObj) { self.data.insert(key, val); }

  fn make(&mut self, obj: LObj) -> LRef {
    self.last += 1;
    self.data.insert(LRef(self.last), obj);
    LRef(self.last)
  }

  fn to_string(&self, key: &LRef) -> String {
    match self.get(key) {
      LObj::Nil => "nil".to_string(),
      LObj::Sym(ref symbol) => symbol.clone(),
      LObj::Num(number) => format!("{}", number),
      LObj::Subr(_) => "<subr>".to_string(),
      LObj::Expr(_, _) => "<expr>".to_string(),
      LObj::Cons(_, _) => format!("({})", self.to_list_string(key)),
    }
  }

  fn to_list_string(&self, key: &LRef) -> String {
    match self.get(key) {
      LObj::Cons(ref car, ref cdr) =>
          format!("{}{}", self.to_string(car), match self.get(cdr) {
            LObj::Nil => "".to_string(),
            LObj::Cons(_, _) => format!(" {}", self.to_list_string(cdr)),
            _ => format!(" . {}", self.to_string(cdr))
          }),
      _ => "<internal error>".to_string(),
    }
  }

  fn nreverse(&mut self, lst: LRef) -> LRef {
    let mut lst = lst;
    let mut ret = self.make(LObj::Nil);
    while let LObj::Cons(car, cdr) = self.get(&lst) {
      self.set(lst.clone(), LObj::Cons(car, ret));
      ret = lst;
      lst = cdr;
    }
    ret
  }

  fn pairlis(&mut self, lst1: LRef, lst2: LRef) -> LRef {
    let mut lst1 = lst1;
    let mut lst2 = lst2;
    let mut ret = self.make(LObj::Nil);
    while let LObj::Cons(car1, cdr1) = self.get(&lst1) {
      if let LObj::Cons(car2, cdr2) = self.get(&lst2) {
        let car = self.make(LObj::Cons(car1, car2));
        ret = self.make(LObj::Cons(car, ret));
        lst1 = cdr1;
        lst2 = cdr2;
        continue;
      }
      break;
    }
    self.nreverse(ret)
  }
}

#[derive(Clone, PartialEq)]
enum LObj {
  Nil, Sym(String), Num(i64), Subr(SubrFn), Expr(LRef, LRef), Cons(LRef, LRef),
}

impl LObj {
  fn sym(s: &str) -> LObj { LObj::Sym(s.into()) }
  fn t() -> LObj { LObj::sym("t") }

  fn car(&self, arena: &Arena) -> LObj
      { match self { &LObj::Cons(ref x, _) => arena.get(x), _ => LObj::Nil } }
  fn cdr(&self, arena: &Arena) -> LObj
      { match self { &LObj::Cons(_, ref x) => arena.get(x), _ => LObj::Nil } }
}

#[derive(Clone, PartialEq)]
enum SubrFn
    { Car, Cdr, Cons, Eq, Atom, Numberp, Symbolp, Add, Mul, Sub, Div, Mod, T }

impl SubrFn {
  fn call(&self, arena: &mut Arena, args: &LRef) -> LObj {
    if let LObj::Cons(ref car, ref cdr) = arena.get(args) {
      let cdar = arena.get(cdr).car(&arena);
      let cdar = arena.make(cdar);
      return match *self {
        SubrFn::Car => arena.get(car).car(&arena),
        SubrFn::Cdr => arena.get(car).cdr(&arena),
        SubrFn::Cons => LObj::Cons(car.clone(), cdar),
        SubrFn::Eq => match arena.get(car) == arena.get(&cdar)
            { true => LObj::t(), _ => LObj::Nil },
        SubrFn::Atom => match arena.get(car)
            { LObj::Cons(_, _) => LObj::Nil, _ => LObj::t() },
        SubrFn::Numberp => match arena.get(car)
            { LObj::Num(_) => LObj::t(), _ => LObj::Nil },
        SubrFn::Symbolp => match arena.get(car)
            { LObj::Sym(_) => LObj::t(), _ => LObj::Nil },
        SubrFn::Add => Self::fold(arena, car, cdr, &|x, y| x + y),
        SubrFn::Mul => Self::fold(arena, car, cdr, &|x, y| x * y),
        SubrFn::Sub => Self::fold(arena, car, cdr, &|x, y| x - y),
        SubrFn::Div => Self::fold(arena, car, cdr, &|x, y| x / y),
        SubrFn::Mod => Self::fold(arena, car, cdr, &|x, y| x % y),
        SubrFn::T => LObj::t(),
      };
    }
    LObj::Nil
  }

  fn all() -> Vec<(SubrFn, &'static str)> {
    vec![(SubrFn::Car, "car"), (SubrFn::Cdr, "cdr"), (SubrFn::Cons, "cons"),
         (SubrFn::Eq, "eq"), (SubrFn::Atom, "atom"),
         (SubrFn::Numberp, "numberp"), (SubrFn::Symbolp, "symbolp"),
         (SubrFn::Add, "+"), (SubrFn::Mul, "*"), (SubrFn::Sub, "-"),
         (SubrFn::Div, "/"), (SubrFn::Mod, "mod"), (SubrFn::T, "t")]
  }

  fn fold(arena: &mut Arena, car: &LRef, cdr: &LRef, f: &Fn(i64, i64) -> i64)
      -> LObj {
    if let LObj::Num(x) = arena.get(car) {
      if let LObj::Cons(ref cdar, ref cddr) = arena.get(cdr) {
        if let LObj::Num(y) = arena.get(cdar) {
          let sum = &arena.make(LObj::Num(f(x, y)));
          return Self::fold(arena, sum, cddr, f);
        }
      } else {
        return arena.get(car);
      }
    }
    LObj::Nil
  }
}

struct Reader<'a> { next: &'a str }

impl<'a> Reader<'a> {
  fn make_num_or_sym(s: &str) -> LObj
      { match s.parse::<i64>() { Ok(n) => LObj::Num(n), _ => LObj::sym(s) } }

  fn read_atom(&mut self) -> LObj {
    let (atom, next) = match self.next.find(
        |c: char| c == '(' || c == ')' || c == '\'' || c.is_whitespace())
        { Some(pos) => self.next.split_at(pos), _ => (self.next, "") };
    self.next = next;
    Self::make_num_or_sym(atom)
  }

  fn read_list(&mut self, arena: &mut Arena) -> Result<LObj, String> {
    let mut ret = LObj::Nil;
    loop {
      self.next = self.next.trim_left();
      if self.next.is_empty() {
        return Err("unfinished parenthesis".into());
      } else if self.next.starts_with(")") {
        self.next = self.next.split_at(1).1;
        let ret = arena.make(ret);
        let ret = arena.nreverse(ret);
        return Ok(arena.get(&ret));
      }
      let car = try!(self.read(arena));
      ret = LObj::Cons(arena.make(car), arena.make(ret));
    }
  }

  fn read(&mut self, arena: &mut Arena) -> Result<LObj, String> {
    self.next = self.next.trim_left();
    if self.next.is_empty() {
      return Err("empty input".into());
    } else if self.next.starts_with(")") {
      return Err(format!("invalid syntax: {}", self.next));
    } else if self.next.starts_with("(") {
      self.next = self.next.split_at(1).1;
      return self.read_list(arena);
    } else if self.next.starts_with("'") {
      self.next = self.next.split_at(1).1;
      let cdar = try!(self.read(arena));
      let cdr = LObj::Cons(arena.make(cdar), arena.make(LObj::Nil));
      return Ok(LObj::Cons(arena.make(LObj::sym("quote")), arena.make(cdr)));
    }
    Ok(self.read_atom())
  }
}

struct Evaluator { arena: Arena, genv: LRef }

impl Evaluator {
  fn new() -> Evaluator {
    let mut evaluator = Evaluator { arena: Arena::new(), genv: LRef(0) };
    let nil = evaluator.arena.make(LObj::Nil);
    evaluator.genv = evaluator.arena.make(LObj::Cons(nil.clone(), nil.clone()));
    for (subr, name) in SubrFn::all() {
      evaluator.add_to_env(LObj::sym(name), LObj::Subr(subr));
    }
    evaluator
  }

  fn find_var(&self, sym: &LObj, env: LRef) -> Option<LRef> {
    let mut env = env;
    while let LObj::Cons(car, cdr) = self.arena.get(&env) {
      let mut alist = car;
      while let LObj::Cons(kv, next) = self.arena.get(&alist) {
        if let LObj::Cons(ref key, ref val) = self.arena.get(&kv) {
          if self.arena.get(key) == *sym {
            return Some(val.clone());
          }
        } 
        alist = next;
      }
      env = cdr;
    }
    None
  }

  fn add_to_env(&mut self, sym: LObj, val: LObj) {
    if let LObj::Cons(car, cdr) = self.arena.get(&self.genv) {
      let sym = self.arena.make(sym);
      let val = self.arena.make(val);
      let result = self.arena.make(LObj::Cons(sym, val));
      let result = self.arena.make(LObj::Cons(result, car));
      self.arena.set(self.genv.clone(), LObj::Cons(result, cdr));
    } else {
      panic!("env must be cons");
    }
  }

  fn eval(&mut self, obj: LObj, env: LRef) -> Result<LObj, String> {
    return match &obj {
      &LObj::Nil | &LObj::Num(_) => Ok(obj.clone()),
      &LObj::Sym(ref name) => {
        let ret = match self.find_var(&obj, env) {
          Some(bind) => bind,
          _ => return Err(format!("{} has no value", name)),
        };
        Ok(self.arena.get(&ret))
      },
      &LObj::Cons(ref f, ref args) => self.apply(f.clone(), args.clone(), env),
      _ => Ok(LObj::Nil),
    }
  }

  fn evlis(&mut self, lst: LRef, env: LRef) -> Result<LRef, String> {
    let mut lst = lst;
    let mut ret = self.arena.make(LObj::Nil);
    while let LObj::Cons(car, cdr) = self.arena.get(&lst) {
      let car = self.arena.get(&car);
      let elm = try!(self.eval(car, env.clone()));
      let elm = self.arena.make(elm);
      ret = self.arena.make(LObj::Cons(elm, ret));
      lst = cdr;
    }
    Ok(self.arena.nreverse(ret))
  }

  fn progn(&mut self, body: LRef, env: LRef) -> Result<LObj, String> {
    let mut body = body;
    let mut ret = LObj::Nil;
    while let LObj::Cons(car, cdr) = self.arena.get(&body) {
      let car = self.arena.get(&car);
      ret = try!(self.eval(car, env.clone()));
      body = cdr;
    }
    Ok(ret)
  }

  fn apply(&mut self, f: LRef, args: LRef, env: LRef) -> Result<LObj, String> {
    if let LObj::Sym(name) = self.arena.get(&f) {
      match name.as_str() {
        "quote" => return Ok(self.arena.get(&args).car(&self.arena)),
        "if" => if let LObj::Cons(car, cdr) = self.arena.get(&args) {
          let car = self.arena.get(&car);
          let ret = match try!(self.eval(car, env.clone())) != LObj::Nil {
            true => self.arena.get(&cdr).car(&self.arena),
            _ => self.arena.get(&cdr).cdr(&self.arena).car(&self.arena),
          };
          return self.eval(ret, env);
        } else {
          return Ok(LObj::Nil);
        },
        "defun" => if let LObj::Cons(car, cdr) = self.arena.get(&args) {
          let car = self.arena.get(&car);
          self.add_to_env(car.clone(), LObj::Expr(cdr, env));
          return Ok(car);
        } else {
          return Ok(LObj::Nil);
        },
        "setq" => if let LObj::Cons(sym, val) = self.arena.get(&args) {
          let val = self.arena.get(&val).car(&self.arena);
          let val = try!(self.eval(val, env.clone()));
          let sym = self.arena.get(&sym);
          match self.find_var(&sym, env.clone()) {
            Some(bind) => self.arena.set(bind, val.clone()),
            None => self.add_to_env(sym, val.clone()),
          };
          return Ok(val);
        } else {
          return Ok(LObj::Nil);
        },
        "lambda" => return Ok(LObj::Expr(args, env)),
        _ => {},
      }
    }
    let f = self.arena.get(&f);
    let f = try!(self.eval(f, env.clone()));
    let args = try!(self.evlis(args, env.clone()));
    return Ok(match f {
      LObj::Subr(subr) => subr.call(&mut self.arena, &args),
      LObj::Expr(body, env) => match self.arena.get(&body) {
        LObj::Cons(car, cdr) => {
          let args = self.arena.pairlis(car, args);
          let car = self.arena.make(LObj::Cons(args, env));
          try!(self.progn(cdr, car))
        },
        _ => LObj::Nil,
      },
      _ => return Err("not function".into()),
    });
  }

  fn mark(&mut self, target: LRef, unused: &mut HashSet<LRef>) {
    if !unused.remove(&target) { return; }
    match self.arena.get(&target) {
      LObj::Expr(a, b) | LObj::Cons(a, b) => {
        self.mark(a, unused);
        self.mark(b, unused);
      },
      _ => {}
    }
  }

  fn garbage_collect(&mut self) {
    let mut unused = HashSet::new();
    for key in self.arena.data.keys() {
      unused.insert(key.clone());
    }
    let env = self.genv.clone();
    self.mark(env, &mut unused);
    for key in unused {
      self.arena.data.remove(&key);
    }
  }
}

fn process(line: &str, evaluator: &mut Evaluator) -> Result<LRef, String> {
  let obj = try!(Reader{ next: &line }.read(&mut evaluator.arena));
  let env = evaluator.genv.clone();
  let result = try!(evaluator.eval(obj, env));
  Ok(evaluator.arena.make(result))
}

fn main() {
  let mut evaluator = Evaluator::new();
  loop {
    print!("> ");
    if let Err(e) = io::stdout().flush() { panic!(e); }
    let mut line = "".to_string();
    if let Err(_) = io::stdin().read_line(&mut line) { break; }
    if line.is_empty() { break; }
    match process(&line, &mut evaluator) {
      Ok(obj) => println!("{}", evaluator.arena.to_string(&obj)),
      Err(e) => println!("<error: {}>", e),
    };
    evaluator.garbage_collect();
  }
}
