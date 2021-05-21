unit DSLCompiler;

/// DSL definition
///
/// program = {function}
///
/// function = identifier "(" [ identifier { "," identifier } ] ")" block
///
/// block = "{" statement {";" statement} [";"] "}"
///
/// statement = if
///           | return
///
/// if = "if" expression block "else" block
///
/// return = "return" expression
///
/// expression = term
///            | term operator term
///
/// term = numeric_constant
///      | function_call
///      | identifier
///
/// operator = "+" | "-" | "<"
///
/// function_call = identifier "(" [expression { "," expression } ] ")"
///
/// - Spacing is ignored.
/// - Only data type is integer.
/// - "If" executes "then" block if expression is <> 0 and "else" block if expression = 0.
/// - There is no assignment.
/// - Only operations are: +, -, <.
/// - Parameters are always passed by value.
/// - "Return" just sets a return value, it doesn't interrupt control flow.
/// - Function without a "return" statement returns 0.
///
/// Example:
/// fib(i) {
///   if i < 2 {
///     return 1
///   } else {
///     return fib(i-2) + fib(i-1)
///   }
/// }
///
/// mult(a,b) {
///   if b < 2 {
///     return a
///   } else {
///     return mult(a, b-1) + a
///   }
/// }

interface

{$mode objfpc}{$H+}

uses

  // dsl compiler
  DSLCompiler.AST,
  DSLCompiler.Compiler,
  DSLCompiler.Parser,
  DSLCompiler.Runnable,
  DSLCompiler.Tokenizer;

type

  ICompiler = interface 
  ['{7CF78EC7-023B-4571-B310-42873921B0BC}']
    function  GetAST: IAST;
    function  GetASTFactory: TASTFactory;
    function  GetCode: IProgram;
    function  GetCodegenFactory: TCodegenFactory;
    function  GetParserFactory: TParserFactory;
    function  GetTokenizerFactory: TTokenizerFactory;
    procedure SetASTFactory(const Value: TASTFactory);
    procedure SetCodegenFactory(const Value: TCodegenFactory);
    procedure SetParserFactory(const Value: TParserFactory);
    procedure SetTokenizerFactory(const Value: TTokenizerFactory);
  //
    function  Codegen: Boolean;
    function  Compile(const Code: string): Boolean;
    function  Parse(const Code: string): Boolean;
  //
    property __AST: IAST 
      read GetAST;
    property __Code: IProgram 
      read GetCode;
    property __ASTFactory: TASTFactory 
      read GetASTFactory 
      write SetASTFactory;
    property __CodegenFactory: TCodegenFactory 
      read GetCodegenFactory 
      write SetCodegenFactory;
    property __ParserFactory: TParserFactory 
      read GetParserFactory 
      write SetParserFactory;
    property __TokenizerFactory: TTokenizerFactory 
      read GetTokenizerFactory 
      write SetTokenizerFactory;
  end; { ICompiler }

function CreateCompiler: ICompiler;

implementation

uses

  // dsl compiler
  DSLCompiler.Base,
  DSLCompiler.ErrorInfo;

type

  TCompiler = class(TCompilerBase, ICompiler)
  strict private
    fAST             : IAST;
    fASTFactory      : TASTFactory;
    fCode            : IProgram;
    fCodegenFactory  : TCodegenFactory;
    fParserFactory   : TParserFactory;
    fTokenizerFactory: TTokenizerFactory;
  strict protected
    function  GetAST: IAST;
    function  GetASTFactory: TASTFactory; inline;
    function  GetCode: IProgram; inline;
    function  GetCodegenFactory: TCodegenFactory; inline;
    function  GetParserFactory: TParserFactory; inline;
    function  GetTokenizerFactory: TTokenizerFactory; inline;
    procedure SetASTFactory(const Value: TASTFactory); inline;
    procedure SetCodegenFactory(const Value: TCodegenFactory); inline;
    procedure SetParserFactory(const Value: TParserFactory); inline;
    procedure SetTokenizerFactory(const Value: TTokenizerFactory); inline;
  public
    constructor Create;
  //
    function  Codegen: Boolean;
    function  Compile(const Code: string): Boolean;
    function  Parse(const Code: string): Boolean;
  //
    property __AST: IAST 
      read GetAST;
    property __Code: IProgram 
      read GetCode;
    property __ASTFactory: TASTFactory 
      read GetASTFactory 
      write SetASTFactory;
    property __CodegenFactory: TCodegenFactory 
      read GetCodegenFactory 
      write SetCodegenFactory;
    property __ParserFactory: TParserFactory 
      read GetParserFactory 
      write SetParserFactory;
    property __TokenizerFactory: TTokenizerFactory 
      read GetTokenizerFactory 
      write SetTokenizerFactory;
  end; { TCompiler }

{ exports }

function CreateCompiler: ICompiler;
begin
  Result := TCompiler.Create;
end; { CreateCompiler }

{ TCompiler }

constructor TCompiler.Create;
begin
  inherited Create;
  __ASTFactory := @CreateAST;
  __CodegenFactory := @CreateCodegen;
  __ParserFactory := @CreateParser;
  __TokenizerFactory := @CreateTokenizer;
end; { TCompiler.Create }

function TCompiler.Codegen: Boolean;
var
  _codegen: ICodegen;
begin
  __LastError := '';
  
  if 
    not assigned(fAST) 
  then
    Exit(SetError('Nothing to do'))
  else begin
    _codegen := __CodegenFactory();
    Result := _codegen.Generate(fAST, fCode);

    if 
      not Result 
    then begin
      fCode := nil;
      __LastError := (_codegen as IErrorInfo).ErrorInfo;
    end;
  end;
end; { TCompiler.Codegen }

function TCompiler.Compile(const Code: string): Boolean;
begin
  Result := Parse(Code);
  
  if Result then Result := Codegen;
end; { TCompiler.Compile }

function TCompiler.GetAST: IAST;
begin
  Result := fAST;
end; { TCompiler.GetAST }

function TCompiler.GetASTFactory: TASTFactory;
begin
  Result := fASTFactory;
end; { TCompiler.GetASTFactory }

function TCompiler.GetCode: IProgram;
begin
  Result := fCode;
end; { TCompiler.GetCode }

function TCompiler.GetCodegenFactory: TCodegenFactory;
begin
  Result := fCodegenFactory;
end; { TCompiler.GetCodegenFactory }

function TCompiler.GetParserFactory: TParserFactory;
begin
  Result := fParserFactory;
end; { TCompiler.GetParserFactory }

function TCompiler.GetTokenizerFactory: TTokenizerFactory;
begin
  Result := fTokenizerFactory;
end; { TCompiler.GetTokenizerFactory }

function TCompiler.Parse(const Code: string): Boolean;
var
  _parser   : IParser;
  _tokenizer: ITokenizer;
begin
  __LastError := '';
  _parser := __ParserFactory();
  _tokenizer := __TokenizerFactory();
  fAST := __ASTFactory();
  Result := _parser.Parse(Code, _tokenizer, fAST);
  
  if 
    not Result 
  then begin
    fAST := nil;
    __LastError := (_parser as IErrorInfo).ErrorInfo;
  end
end; { TCompiler.Parse }

procedure TCompiler.SetASTFactory(const Value: TASTFactory);
begin
  fASTFactory := Value;
end; { TCompiler.SetASTFactory }

procedure TCompiler.SetCodegenFactory(const Value: TCodegenFactory);
begin
  fCodegenFactory := Value;
end; { TCompiler.SetCodegenFactory }

procedure TCompiler.SetParserFactory(const Value: TParserFactory);
begin
  fParserFactory := Value;
end; { TCompiler.SetParserFactory }

procedure TCompiler.SetTokenizerFactory(const Value: TTokenizerFactory);
begin
  fTokenizerFactory := Value;
end; { TCompiler.SetTokenizerFactory }

end.
