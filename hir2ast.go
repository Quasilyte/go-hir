package hir

import (
	"fmt"
	"go/ast"
	"go/token"
)

func astWithParens(parens bool, e ast.Expr) ast.Expr {
	if parens {
		return &ast.ParenExpr{X: e}
	}
	return e
}

func astStmtList(in []Stmt) []ast.Stmt {
	out := make([]ast.Stmt, len(in))
	for i := range in {
		out[i] = in[i].GoStmt()
	}
	return out
}

func astExprList(in []Expr) []ast.Expr {
	out := make([]ast.Expr, len(in))
	for i := range in {
		out[i] = in[i].GoExpr()
	}
	return out
}

func (b *Block) GoStmt() ast.Stmt {
	return &ast.BlockStmt{
		List: astStmtList(b.List),
	}
}

func (ret *Return) GoStmt() ast.Stmt {
	return &ast.ReturnStmt{
		Results: astExprList(ret.Results),
	}
}

func (v *IntVal) GoExpr() ast.Expr {
	if v.Source != nil {
		return astWithParens(v.Parens, v.Source)
	}

	return astWithParens(v.Parens, &ast.BasicLit{
		Kind:  token.INT,
		Value: fmt.Sprint(v.Val),
	})
}

func (v *FloatVal) GoExpr() ast.Expr {
	if v.Source != nil {
		return astWithParens(v.Parens, v.Source)
	}

	return nil
}

func (v *StringVal) GoExpr() ast.Expr {
	if v.Source != nil {
		return astWithParens(v.Parens, v.Source)
	}

	return astWithParens(v.Parens, &ast.BasicLit{
		Kind:  token.STRING,
		Value: `"` + v.Val + `"`,
	})
}

func (op *OpAdd) GoExpr() ast.Expr {
	return astWithParens(op.Parens, &ast.BinaryExpr{
		Op: token.ADD,
		X:  op.LHS.GoExpr(),
		Y:  op.RHS.GoExpr(),
	})
}

func (op *OpMul) GoExpr() ast.Expr {
	return astWithParens(op.Parens, &ast.BinaryExpr{
		Op: token.MUL,
		X:  op.LHS.GoExpr(),
		Y:  op.RHS.GoExpr(),
	})
}

func (v *Var) GoExpr() ast.Expr {
	return &ast.Ident{Name: v.Obj.Name()}
}

func (assign *Assign) GoStmt() ast.Stmt {
	tok := token.ASSIGN
	if len(assign.Defs) != 0 {
		tok = token.DEFINE
	}
	return &ast.AssignStmt{
		Tok: tok,
		Lhs: astExprList(assign.LHS),
		Rhs: astExprList(assign.RHS),
	}
}

func (n *Nil) GoExpr() ast.Expr {
	return &ast.Ident{Name: "nil"}
}

func (decl *VarDecl) GoStmt() ast.Stmt {
	specs := make([]ast.Spec, len(decl.List))
	for i, assign := range decl.List {
		idents := make([]*ast.Ident, len(assign.LHS))
		for j, lhs := range assign.LHS {
			idents[j] = lhs.GoExpr().(*ast.Ident)
		}
		var values []ast.Expr
		if !decl.implicit[i] {
			values = astExprList(assign.RHS)
		}
		specs[i] = &ast.ValueSpec{
			Type:   decl.types[i],
			Names:  idents,
			Values: values,
		}
	}
	return &ast.DeclStmt{
		Decl: &ast.GenDecl{
			Tok:   token.VAR,
			Specs: specs,
		},
	}
}
