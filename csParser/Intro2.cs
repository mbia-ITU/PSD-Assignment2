namespace Intro;
class Expr { }

class CstI : Expr
{
    readonly int value;
    public CstI(int val)
    {
        value = val;
    }
    public override string ToString() => value.ToString();

}

class Var : Expr
{
    readonly string name;
    public Var(string s)
    {
        name = s;
    }

    public override string ToString() => name;
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
}
class Sub : Binop
{
    public Sub(Expr exp1, Expr exp2) : base(exp1, exp2) { }
    public override string ToString() => Print("-");
}
class Mul : Binop
{
    public Mul(Expr exp1, Expr exp2) : base(exp1, exp2) { }
    public override string ToString() => Print("*");
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



