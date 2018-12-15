package hir

import (
	"go/ast"
	"go/token"
	"go/types"
)

type Expr interface {
	GoExpr() ast.Expr
}

type Var struct {
	Obj *types.Var
}

type Nil struct {
	Type types.Type
}

type (
	IntVal struct {
		Type types.Type

		Pos    token.Pos
		Parens bool
		Source ast.Expr

		Val int64
	}

	FloatVal struct {
		Type types.Type

		Pos    token.Pos
		Parens bool
		Source ast.Expr

		Val float64
	}

	StringVal struct {
		Type types.Type

		Pos    token.Pos
		Parens bool
		Source ast.Expr

		Val string
	}
)

type (
	OpAdd struct {
		Type   types.Type
		Pos    token.Pos
		Parens bool

		LHS Expr
		RHS Expr
	}

	OpMul struct {
		Type   types.Type
		Pos    token.Pos
		Parens bool

		LHS Expr
		RHS Expr
	}
)
