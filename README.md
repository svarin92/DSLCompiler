# DSLCompiler
Some small adaptations and enhancements to the Primož Gabrijelčič compiler - alias gabr42 - which can now be run from Lazarus IDE and Free Pascal Compiler (FPC).

A must read:

* Writing a Simple DSL Compiler with Delphi  - **[Introduction and so on](https://www.thedelphigeek.com/2017/08/writing-simple-dsl-compiler-with-delphi.html)**. The Delphi Geek, gabr42, 2017
* SimpleDSLCompiler, A Personal Experiment - **[Not so simple](https://github.com/gabr42/SimpleDSLCompiler)**. Github, gabr42, 2017

***

#### Demo output:

```
----- Some statements -----

fib(i)
{
  if (i < 3)
  {
    return 1
  }
  else
  {
    return fib(i - 2) + fib(i - 1)
  }
}

mult(a,b)
{
  if (b < 2)
  {
    return a
  }
  else
  {
    return mult(a, b - 1) + a
  }
}

power(a,b)
{
  if (b < 2)
  {
    return a
  }
  else
  {
    return mult(power(a, b - 1), a)
  }
}

----- Syntactic Recognition with Computation -----

mult(5,3) = 15
2^10 = 1024
fib(7) (native) = 13
fib(7) (compiled) = 13

----- Elapsed runtime: fib(30) -----

Native: 832040 in 16 ms
Compiled: 832040 in 344 ms
Interpreted: 832040 in 2312 ms
```

***

#### Draft compilation process
![Compilation process](/docs/assets/dslCompiler.png "Draft compilation process")