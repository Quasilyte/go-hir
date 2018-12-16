package hir

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
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

	return astWithParens(v.Parens, &ast.BasicLit{
		Kind:  token.FLOAT,
		Value: fmt.Sprint(v.Val),
	})
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

func (v *BoolVal) GoExpr() ast.Expr {
	if v.Source != nil {
		return astWithParens(v.Parens, v.Source)
	}

	return astWithParens(v.Parens, &ast.BasicLit{
		Kind:  token.STRING,
		Value: fmt.Sprint(v.Val),
	})
}

func (op *OpAdd) GoExpr() ast.Expr {
	return astWithParens(op.Parens, &ast.BinaryExpr{
		Op: token.ADD,
		X:  op.LHS.GoExpr(),
		Y:  op.RHS.GoExpr(),
	})
}

func (op *OpSub) GoExpr() ast.Expr {
	return astWithParens(op.Parens, &ast.BinaryExpr{
		Op: token.SUB,
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

func (op *OpQuo) GoExpr() ast.Expr {
	return astWithParens(op.Parens, &ast.BinaryExpr{
		Op: token.QUO,
		X:  op.LHS.GoExpr(),
		Y:  op.RHS.GoExpr(),
	})
}

func (v *Var) GoExpr() ast.Expr {
	return astWithParens(v.Parens, &ast.Ident{Name: v.Obj.Name()})
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
	return astWithParens(n.Parens, &ast.Ident{Name: "nil"})
}

func (blank *Blank) GoExpr() ast.Expr {
	return &ast.Ident{Name: "_"}
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

func (stmt *If) GoStmt() ast.Stmt {
	return &ast.IfStmt{
		Init: stmt.Init.GoStmt(),
		Cond: stmt.Cond.GoExpr(),
		Body: stmt.Body.GoStmt().(*ast.BlockStmt),
	}
}

func (stmt *emptyStmt) GoStmt() ast.Stmt {
	return nil
}

func (stmt *IfElse) GoStmt() ast.Stmt {
	first := stmt.List[0].GoStmt().(*ast.IfStmt)
	branch := first
	for _, stmt := range stmt.List[1:] {
		branch.Else = stmt.GoStmt()
		branch = branch.Else.(*ast.IfStmt)
	}
	if stmt.Else != nil {
		branch.Else = stmt.Else.GoStmt()
	}
	return first
}

func (tconv *TypeConv) GoExpr() ast.Expr {
	return astWithParens(tconv.Parens, &ast.CallExpr{
		Fun:  typeToAst(tconv.Typ),
		Args: []ast.Expr{tconv.Arg.GoExpr()},
	})
}

func typeToAst(typ types.Type) ast.Expr {
	switch typ := typ.(type) {
	case *types.Basic:
		return &ast.Ident{Name: typ.Name()}
	case *types.Pointer:
		return &ast.StarExpr{X: typeToAst(typ.Elem())}
	}
	panic(fmt.Sprintf("can't convert %T to ast expr", typ))
}
