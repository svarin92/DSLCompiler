program demo;

{$mode objfpc}{$H+}

uses

  // rtl, fcl, lcl
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  Classes,

  // dsl compiler
  DSLCompiler in 'DSLCompiler.pas',
  DSLCompiler.Parser in 'DSLCompiler.Parser.pas',
  DSLCompiler.AST in 'DSLCompiler.AST.pas',
  DSLCompiler.Runnable in 'DSLCompiler.Runnable.pas',
  DSLCompiler.Compiler in 'DSLCompiler.Compiler.pas',
  DSLCompiler.ErrorInfo in 'DSLCompiler.ErrorInfo.pas',
  DSLCompiler.Base in 'DSLCompiler.Base.pas',
  DSLCompiler.Tokenizer in 'DSLCompiler.Tokenizer.pas',
  DSLCompiler.Compiler.Dump in 'DSLCompiler.Compiler.Dump.pas',
  DSLCompiler.Compiler.Codegen in 'DSLCompiler.Compiler.Codegen.pas',
  DSLCompiler.Interpreter in 'DSLCompiler.Interpreter.pas';

const

  CMultiProcCode =
    'fib(i) {                          '#13#10 +
    '  if i < 3 {                      '#13#10 +
    '    return 1                      '#13#10 +
    '  } else {                        '#13#10 +
    '    return fib(i-2) + fib(i-1)    '#13#10 +
    '  }                               '#13#10 +
    '}                                 '#13#10 +
    '                                  '#13#10 +
    'mult(a,b) {                       '#13#10 +
    '  if b < 2 {                      '#13#10 +
    '    return a                      '#13#10 +
    '  } else {                        '#13#10 +
    '    return mult(a, b-1) + a       '#13#10 +
    '  }                               '#13#10 +
    '}                                 '#13#10 +
    '                                  '#13#10 +
    // 'fact(i) {                         '#13#10 +
    // '  if i <= 0 {                     '#13#10 +
    // '    return 1                      '#13#10 +
    // '  } else {                        '#13#10 +
    // '    return mult(i, fact(i-1))     '#13#10 +
    // '  }                               '#13#10 +
    // '}                                 '#13#10 +
    // '                                  '#13#10 +
    'power(a,b) {                      '#13#10 +
    '  if b < 2 {                      '#13#10 +
    '    return a                      '#13#10 +
    '  } else {                        '#13#10 +
    '    return mult(power(a, b-1), a) '#13#10 +
    '  }                               '#13#10 +
    '}                                 '#13#10;

{ DSiWin32 }

threadvar

  GLastTimeGetTime: DWORD;
  GTimeGetTimeBase: Int64;

{: 64-bit extension of MM timeGetTime. Time units are milliseconds.
   @author  gabr
   @since   2007-11-26. }
function DSiTimeGetTime64: Int64;
begin
  Result := GetTickCount64; // timeGetTime

  if
    Result < GLastTimeGetTime
  then
    GTimeGetTimeBase := GTimeGetTimeBase + $100000000;

  GLastTimeGetTime := Result;
  Result := Result + GTimeGetTimeBase;
end; { DSiTimeGetTime64 }

{: Returns time elapsed since startTime, which must be a result of
   the DSiTimeGetTime64. }
function DSiElapsedTime64(startTime: Int64): Int64;
begin
  Result := DSiTimeGetTime64 - startTime;
end; { DSiElapsedTime64 }

{ Demo }

var

  compiler   : ICompiler;
  exec       : IProgram;
  interpreter: IProgram;
  res        : Integer;
  sl         : TStringList;
  time       : Int64;

function fib(i: Integer): Integer;
begin
  if
    i < 3
  then
    Result := 1
  else
    Result := fib(i-2) + fib(i-1);
end;

function _: ICodegen;
begin
  Result := CreateCodegenDump(sl);
end;

begin
  try
    sl := TStringList.Create;
    try
      compiler := CreateCompiler;
      compiler.__CodegenFactory := @_;      
      compiler.Compile(CMultiProcCode);
      WriteLn('----- Some statements -----' + #13#10);
      Writeln(sl.Text);
    finally
      FreeAndNil(sl);
    end;

    Writeln('----- Syntactic Recognition with Computation -----' + #13#10);

    compiler := CreateCompiler;

    if
      not compiler.Compile(CMultiProcCode)
    then
      Writeln('Compilation/codegen error: ' + (compiler as IErrorInfo).ErrorInfo)
    else begin
      exec := compiler.__Code;

      if
        exec.Call('mult', [5,3], res)
      then
        Writeln('mult(5,3) = ', res)
      else
        Writeln('mult: ' + (exec as IErrorInfo).ErrorInfo);

      exec.Call('power', [2,10], res);
      Writeln('2^10 = ', res);

      // exec.Call('fact', [5], res);
      // Writeln('5! = ', res);

      Writeln('fib(7) (native) = ', fib(7));
      exec.Call('fib', [7], res);
      Writeln('fib(7) (compiled) = ', res);

      Writeln(#13#10 + '----- Elapsed runtime: fib(30) -----' + #13#10);

      time := DSiTimeGetTime64;
      res := fib(30);
      time := DSiElapsedTime64(time);
      Writeln('Native: ', res, ' in ', time, ' ms');

      time := DSiTimeGetTime64;
      res := 0;
      exec.Call('fib', [30], res);
      time := DSiElapsedTime64(time);
      Writeln('Compiled: ', res, ' in ', time, ' ms');

      interpreter := CreateInterpreter(compiler.__AST);
      time := DSiTimeGetTime64;
      res := 0;

      if
        not interpreter.Call('fib', [30], res)
      then
        Writeln('interpreter: ' + (interpreter as IErrorInfo).ErrorInfo)
      else begin
        time := DSiElapsedTime64(time);
        Writeln('Interpreted: ', res, ' in ', time, ' ms');
      end;
    end;

    Write('> ');
    Readln;
  except
    on
      E: Exception
    do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
