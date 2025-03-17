module AST where

-- Constants: integers and booleans
data Constant
  = IntConst Int    -- Integer constant
  | BoolConst Bool  -- Boolean constant
  deriving (Show, Eq)

-- Type expressions: supports basic types and function types
data TypeExpr
  = TypeInt                    -- Type: int
  | TypeBool                   -- Type: bool
  | TypeVar String             -- Polymorphic type variable (e.g., 'a)
  | TypeFunc TypeExpr TypeExpr -- Function type (T1 -> T2)
  deriving (Show, Eq)

-- Patterns for pattern matching:
-- Only variable patterns, constant patterns, and type annotated patterns are supported.
data Pattern
  = Wildcard                     -- Wildcard pattern (_)
  | VarPattern String            -- Variable pattern (e.g., x, y)
  | ConstPattern Constant        -- Constant pattern (e.g., 1, true, false)
  | TypePattern Pattern TypeExpr -- Pattern with explicit type annotation (e.g., (x : int))
  deriving (Show, Eq)

-- Expressions:
-- Function application is represented by a constructor that takes a list of arguments.
data Expr
  = Identifier String                   -- Identifier (variable or function)
  | ConstantExpr Constant               -- Constant expression
  | LetBinding [(Pattern, Expr)] Expr     -- Let binding (let x = e in ...)
  | LetRecBinding [(Pattern, Expr)] Expr  -- Recursive let binding (let rec f = ... in ...)
  | Lambda [Pattern] Expr               -- Lambda function (fun (x) -> e)
  | MatchExpr Expr [(Pattern, Expr)]    -- Pattern matching (match e with ...)
  | Application Expr [Expr]             -- Function application (f [e1, e2, ...])
  | IfExpr Expr Expr (Maybe Expr)       -- Conditional expression (if e then e else e)
  | TypeAnnotation Expr TypeExpr        -- Type annotation (e : T)
  deriving (Show, Eq)

-- Top-level items:
data TopLevelItem
  = EvalExpr Expr                       -- Expression to be evaluated
  | LetBindingItem [(Pattern, Expr)]    -- Top-level let binding (let x = e)
  | LetRecBindingItem [(Pattern, Expr)] -- Top-level recursive let binding (let rec f = ...)
  deriving (Show, Eq)

-- A program is a list of top-level items
type Program = [TopLevelItem]
