module AST where

-- Represents a constant value (integer, character, or string)
data Constant
  = IntConst Int      -- Integer constant
  | CharConst Char    -- Character constant
  | StringConst String -- String constant
  deriving (Show, Eq)

-- Representation of types in the language
data TypeExpr
  = TypeVar String                     -- Type variable ('a, 'b, etc.)
  | TypeFunc TypeExpr TypeExpr         -- Function type (T1 -> T2)
  | TypeTuple [TypeExpr]               -- Tuple type (T1, T2, ...)
  | TypeConstructor String [TypeExpr]  -- Parametric type constructor (List T, Option T, etc.)
  deriving (Show, Eq)

-- Pattern matching constructs
data Pattern
  = Wildcard                          -- Matches anything (_)
  | VarPattern String                 -- Matches a variable (x, y, etc.)
  | ConstPattern Constant             -- Matches a constant value (1, 'a', "hello")
  | TuplePattern [Pattern]            -- Matches a tuple pattern (x, y)
  | OrPattern Pattern Pattern         -- Matches one of two patterns (P1 | P2)
  | ConstructPattern String (Maybe Pattern) -- Matches a constructor pattern (Some x, None, etc.)
  | TypePattern Pattern TypeExpr       -- Pattern with an explicit type annotation (x: int)
  deriving (Show, Eq)

-- Expression constructs
data Expr
  = Identifier String                  -- Variable or function identifier
  | ConstantExpr Constant              -- Constant expression (1, "text", etc.)
  | LetBinding [(Pattern, Expr)] Expr  -- Let binding (let x = 1 in ...)
  | LetRecBinding [(Pattern, Expr)] Expr -- Recursive let binding (let rec f = ... in ...)
  | Lambda [Pattern] Expr              -- Function definition (fun x -> x + 1)
  | MatchExpr Expr [(Pattern, Expr)]   -- Pattern matching (match x with ...)
  | Application Expr Expr              -- Function application (f x)
  | TupleExpr [Expr]                   -- Tuple expression (1, "a", true)
  | ConstructorExpr String (Maybe Expr) -- Constructor application (Some 1, None)
  | IfExpr Expr Expr (Maybe Expr)      -- Conditional expression (if ... then ... else ...)
  | SequenceExpr [Expr]                -- Sequential expressions (E1; E2)
  | TypeAnnotation Expr TypeExpr       -- Expression with type annotation (E : T)
  deriving (Show, Eq)

-- Constructor declaration for algebraic data types
data ConstructorDecl = ConstructorDecl 
  { constructorId :: String           -- Name of the constructor
  , constructorArg :: Maybe TypeExpr  -- Optional type of the constructor argument
  } deriving (Show, Eq)

-- Type declaration for algebraic data types
data TypeDeclaration = TypeDeclaration 
  { typeId :: String                   -- Name of the type
  , typeParams :: [String]              -- Type parameters (for generics)
  , typeVariants :: [ConstructorDecl]   -- List of constructors for the type
  } deriving (Show, Eq)

-- Top-level constructs (statements in a program)
data TopLevelItem
  = EvalExpr Expr                      -- Expression to be evaluated
  | TypeDef TypeDeclaration            -- Type definition (type t = ...)
  | LetBindingItem [(Pattern, Expr)]   -- Let binding at the top level
  | LetRecBindingItem [(Pattern, Expr)] -- Recursive let binding at the top level
  deriving (Show, Eq)

-- A program is a list of top-level items
type Program = [TopLevelItem]