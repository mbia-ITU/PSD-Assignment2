namespace Intro;
using System.Linq;
abstract class Expr { 
    public abstract int Eval(List<(string, int)> env);
    public abstract Expr Simplify();
}

class CstI : Expr
{
    readonly int value;
    public CstI(int val)
    {
        value = val;
    }

    public override int Eval(List<(string, int)> env) => value;

    public override string ToString() => value.ToString();

    public override Expr Simplify() => new CstI(value);

}

class Var : Expr
{
    readonly string name;
    public Var(string s)
    {
        name = s;
    }
    public override int Eval(List<(string, int)> env) => env.Where(c => c.Item1 == name).FirstOrDefault().Item2;
    

    public override string ToString() => name;

    public override Expr Simplify() => new Var(name);
}

abstract class Binop : Expr
{
    public Expr exp1;
    public Expr exp2;

    public Binop(Expr a, Expr b)
    {
        exp1 = a;
        exp2 = b;
    }

    public string Print(string ope)
    {
        return $"({exp1} {ope} {exp2})";
    }
}

class Add : Binop
{
    public Add(Expr exp1, Expr exp2) : base(exp1, exp2) { }

    public override string ToString() => Print("+");

    public override int Eval(List<(string, int)> env) => exp1.Eval(env) + exp2.Eval(env);

    public override Expr Simplify(){
        var simple1 = exp1.Simplify();
        var simple2 = exp2.Simplify();
        if (simple1 == new CstI(0)){
            return simple2;
        }
        if (simple2 == new CstI(0)){
            return simple1;
        }
        return new Add(simple1, simple2);
    }
}
class Sub : Binop
{
    public Sub(Expr exp1, Expr exp2) : base(exp1, exp2) { }
    public override string ToString() => Print("-");

    public override int Eval(List<(string, int)> env) => exp1.Eval(env) - exp2.Eval(env);
    public override Expr Simplify(){
        var simple1 = exp1.Simplify();
        var simple2 = exp2.Simplify();
        if (simple2 == new CstI(0)){
            return simple1;
        }
        if (simple1 == simple2){
            return new CstI(0);
        }
        return new Sub(simple1, simple2);
    }
}
class Mul : Binop
{
    public Mul(Expr exp1, Expr exp2) : base(exp1, exp2) { }
    public override string ToString() => Print("*");

    public override int Eval(List<(string, int)> env) => exp1.Eval(env) * exp2.Eval(env);

    public override Expr Simplify(){
        var simple1 = exp1.Simplify();
        var simple2 = exp2.Simplify();
        if (simple1 == new CstI(0)){
            return new CstI(0);
        }
        if (simple2 == new CstI(0)){
            return new CstI(0);
        }
        if (simple1 == new CstI(1)){
            return simple2;
        }
        if (simple2 == new CstI(1)){
            return simple1;
        }
        return new Mul(simple1, simple2);
    }
}

class Program
{
    public static void Main()
    {
        var e1 = new Mul(new CstI(10), new CstI(2));
        var e2 = new Add(new Var("v"), new CstI(10));
        var e3 = new Sub(e1, new CstI(20));
        Console.WriteLine(e1);
        Console.WriteLine(e2);
        Console.WriteLine(e3);
        
    }
}



