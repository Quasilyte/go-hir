package hir

import (
	"go/ast"
	"go/token"
	"go/types"
)

type Stmt interface {
	GoStmt() ast.Stmt
}

type emptyStmt struct{}

type Block struct {
	Pos token.Pos

	List []Stmt
}

type Return struct {
	Pos token.Pos

	Results []Expr
}

type IfElse struct {
	List []*If
	Else *Block
}

type If struct {
	Pos token.Pos

	Init Stmt
	Cond Expr
	Body *Block
}

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
