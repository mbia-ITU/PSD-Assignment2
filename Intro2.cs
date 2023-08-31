class Expr { }

class CstI : Expr {
    int value;
    CstI(int val)
    {
        value = val;
    }
    string toString() {
        return value.toString();
    }
}

class Var : Expr {
    string name;
    Var (string s)
    {
        name = s;
    }

    string toString()
    {
        return name;
    }
}

abstract class Binop : Expr {
    Expr exp1;
    Expr exp2;

    string toString(string ope) {
        return $"({exp1} {ope} {exp2})";
    }
}

class Add : Binop {
    Add(Expr a, Expr b) {
        base.exp1 = a;
        base.exp2 = b;
    }
    string toString() => base.toString("+");
}
class Sub : Binop {
    Sub(Expr a, Expr b)
    {
        base.exp1 = a;
        base.exp2 = b;
    }
    string toString() => base.toString("-");
}
class Mul : Binop {
    Mul(Expr a, Expr b)
    {
        base.exp1 = a;
        base.exp2 = b;
    }
    string toString() => base.toString("*");
}

public void main(string[] args)
{
    var e1 = Mul(CstI(10), CstI(2));
    var e2 = Add(Var("v"), CstI(10));
    var e3 = Sub(e1, CstI(20));
    Console.WriteLine(e1);
    Console.WriteLine(e2);
    Console.WriteLine(e3);
}