/*
 * @file virtual_inheritence_main.cpp
 * @date 3 June Sep 2018
 * @author Chester Gillon
 * @details Test C++ virtual_inheritence
 */

class A

{
public:
    virtual ~A(){};
    void get (int &a_out, int &b_out, int &c_out, int &d_out)
    {
        a_out = a;
        b_out = b;
        c_out = c;
        d_out = d;
    }

protected:
    A() :
    a(1),
    b(2),
    c(3),
    d(4)
    {};

private:
    int a;
    int b;
    int c;
    int d;
};

class B : public virtual A
{
public:
    virtual ~B(){};

protected:
    B(){};
};

class C : public virtual A
{
public:
    virtual ~C(){};

protected:
    C(){};
};

class D : public B, public C
{
public:
    D(){};
    virtual ~D(){};
};


int main (void)
{
    D d_test;
    int a, b, c, d;

    d_test.get (a, b, c, d);

    return a + b + c + d;
}
