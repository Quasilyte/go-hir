package hir

import (
	"go/ast"
	"go/token"
	"go/types"
)

type Expr interface {
	GoExpr() ast.Expr
	Type() types.Type
}

type Var struct {
	Obj *types.Var

	Pos    token.Pos
	Parens bool
}

type Blank struct {
	// TODO(Quasilyte): does blank need a proper type?

	Pos token.Pos
}

type Nil struct {
	Typ types.Type

	Pos    token.Pos
	Parens bool
}

type TypeConv struct {
	Typ types.Type

	Pos    token.Pos
	Parens bool

	Arg Expr
}

type (
	IntVal struct {
		Typ types.Type

		Pos    token.Pos
		Parens bool
		Source ast.Expr

		Val int64
	}

	FloatVal struct {
		Typ types.Type

		Pos    token.Pos
		Parens bool
		Source ast.Expr

		Val float64
	}

	StringVal struct {
		Typ types.Type

		Pos    token.Pos
		Parens bool
		Source ast.Expr

		Val string
	}
)

type (
	OpAdd struct {
		Pos    token.Pos
		Parens bool

		LHS Expr
		RHS Expr
	}

	OpMul struct {
		Pos    token.Pos
		Parens bool

		LHS Expr
		RHS Expr
	}
)

func (v *Var) Type() types.Type {
	return v.Obj.Type()
}

func (blank *Blank) Type() types.Type { return nil }

func (nihil *Nil) Type() types.Type {
	return nihil.Typ
}

func (ival *IntVal) Type() types.Type {
	return ival.Typ
}

func (fval *FloatVal) Type() types.Type {
	return fval.Typ
}

func (sval *StringVal) Type() types.Type {
	return sval.Typ
}

func (op *OpAdd) Type() types.Type { return op.LHS.Type() }

func (op *OpMul) Type() types.Type { return op.LHS.Type() }

func (tconv *TypeConv) Type() types.Type { return tconv.Typ }
