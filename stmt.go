package hir

import (
	"go/ast"
	"go/token"
	"go/types"
)

type Stmt interface {
	GoStmt() ast.Stmt
}

type Block struct {
	Pos token.Pos

	List []Stmt
}

type Return struct {
	Pos token.Pos

	Results []Expr
}

// a = b
// a, b := 1, 2
//

type VarDecl struct {
	Pos      token.Pos
	types    []ast.Expr
	implicit []bool

	List []*Assign
}

type Assign struct {
	Pos token.Pos

	Defs []*types.Var
	LHS  []Expr
	RHS  []Expr
}
