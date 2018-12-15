package hir

import (
	"fmt"
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/ast/astutil"
)

type converter struct {
	info *types.Info
}

func ConvertFunc(info *types.Info, decl *ast.FuncDecl) *Block {
	conv := &converter{info: info}
	return conv.block(decl.Body)
}

func (conv *converter) block(b *ast.BlockStmt) *Block {
	if b == nil {
		return &Block{}
	}
	return &Block{
		Pos:  b.Pos(),
		List: conv.stmtList(b.List),
	}
}

func (conv *converter) exprList(in []ast.Expr) []Expr {
	out := make([]Expr, len(in))
	for i := range in {
		out[i] = conv.expr(in[i])
	}
	return out
}

func (conv *converter) stmtList(in []ast.Stmt) []Stmt {
	out := make([]Stmt, len(in))
	for i := range in {
		out[i] = conv.stmt(in[i])
	}
	return out
}

func (conv *converter) expr(expr ast.Expr) Expr {
	switch expr := expr.(type) {
	case *ast.Ident:
		return conv.ident(expr)
	case *ast.BasicLit:
		return conv.basicLit(expr)
	case *ast.ParenExpr:
		return conv.parenExpr(expr)
	case *ast.BinaryExpr:
		return conv.binaryExpr(expr)

	default:
		panic(fmt.Sprintf("unhandled expr: %T", expr))
	}
}

func (conv *converter) ident(id *ast.Ident) Expr {
	tv := conv.info.Types[id]
	if tv.Value != nil {
		switch tv.Value.Kind() {
		case constant.Int:
			v, exact := constant.Int64Val(tv.Value)
			if !exact {
				panic(fmt.Sprintf("unexpected !exact int value (%s)", id.Name))
			}
			return &IntVal{
				Pos:    id.Pos(),
				Type:   tv.Type,
				Val:    v,
				Source: id,
			}

		case constant.Float:
			v, exact := constant.Float64Val(tv.Value)
			if !exact {
				panic(fmt.Sprintf("unexpected !exact float value (%s)", id.Name))
			}
			return &FloatVal{
				Pos:    id.Pos(),
				Type:   tv.Type,
				Val:    v,
				Source: id,
			}

		case constant.String:
			return &StringVal{
				Pos:    id.Pos(),
				Type:   tv.Type,
				Val:    constant.StringVal(tv.Value),
				Source: id,
			}

		default:
			panic(fmt.Sprintf("unhandled const kind: %v", tv.Value.Kind()))
		}
	}

	if id.Name == "nil" {
		return &Nil{Type: tv.Type}
	}

	obj, ok := conv.info.ObjectOf(id).(*types.Var)
	if !ok || obj == nil {
		panic(fmt.Sprintf("unexpected nil object for %s", id.Name))
	}
	return &Var{Obj: obj}
}

func (conv *converter) basicLit(lit *ast.BasicLit) Expr {
	tv := conv.info.Types[lit]
	switch tv.Value.Kind() {
	case constant.Int:
		v, exact := constant.Int64Val(tv.Value)
		if !exact {
			panic(fmt.Sprintf("unexpected !exact int value (%s)", lit.Value))
		}
		return &IntVal{
			Pos:  lit.Pos(),
			Type: tv.Type,
			Val:  v,
		}

	case constant.Float:
		v, exact := constant.Float64Val(tv.Value)
		if !exact {
			panic(fmt.Sprintf("unexpected !exact float value (%s)", lit.Value))
		}
		return &FloatVal{
			Pos:  lit.Pos(),
			Type: tv.Type,
			Val:  v,
		}

	case constant.String:
		return &StringVal{
			Pos:  lit.Pos(),
			Type: tv.Type,
			Val:  constant.StringVal(tv.Value),
		}

	default:
		panic(fmt.Sprintf("unhandled const kind: %v", tv.Value.Kind()))
	}

}

func (conv *converter) parenExpr(paren *ast.ParenExpr) Expr {
	// Reduce any amount of surrounding parenthesis to 1.
	expr := conv.expr(astutil.Unparen(paren))
	switch expr := expr.(type) {
	case *IntVal:
		expr.Parens = true
	case *FloatVal:
		expr.Parens = true
	case *StringVal:
		expr.Parens = true
	case *OpAdd:
		expr.Parens = true
	case *OpMul:
		expr.Parens = true
	}
	return expr
}

func (conv *converter) binaryExpr(e *ast.BinaryExpr) Expr {
	typ := conv.info.TypeOf(e)
	x := conv.expr(e.X)
	y := conv.expr(e.Y)
	switch e.Op {
	case token.ADD:
		return &OpAdd{Pos: e.Pos(), Type: typ, LHS: x, RHS: y}
	case token.MUL:
		return &OpMul{Pos: e.Pos(), Type: typ, LHS: x, RHS: y}

	default:
		panic(fmt.Sprintf("unhandled binary expr: %s", e.Op.String()))
	}
}

func (conv *converter) stmt(stmt ast.Stmt) Stmt {
	switch stmt := stmt.(type) {
	case *ast.AssignStmt:
		return conv.assignStmt(stmt)
	case *ast.DeclStmt:
		return conv.declStmt(stmt)
	case *ast.ReturnStmt:
		return &Return{
			Pos:     stmt.Pos(),
			Results: conv.exprList(stmt.Results),
		}

	default:
		panic(fmt.Sprintf("unhandled stmt: %T", stmt))
	}
}

func (conv *converter) declStmt(stmt *ast.DeclStmt) Stmt {
	decl := stmt.Decl.(*ast.GenDecl)
	switch decl.Tok {
	case token.VAR:
		implicit := make([]bool, len(decl.Specs))
		types := make([]ast.Expr, len(decl.Specs))
		list := make([]*Assign, len(decl.Specs))
		for i, spec := range decl.Specs {
			spec := spec.(*ast.ValueSpec)
			list[i] = conv.specToAssign(spec)
			if spec.Type != nil {
				types[i] = spec.Type
			}
			if len(spec.Values) == 0 {
				implicit[i] = true
			}
		}
		return &VarDecl{
			Pos:      stmt.Pos(),
			types:    types,
			implicit: implicit,
			List:     list,
		}

	default:
		panic(fmt.Sprintf("unexpected decl token: %s", decl.Tok.String()))
	}
}

func (conv *converter) astZeroVal(typ types.Type) ast.Expr {
	switch typ := typ.Underlying().(type) {
	case *types.Basic:
		switch flags := typ.Info(); {
		case flags&types.IsNumeric != 0:
			return &ast.BasicLit{Value: "0"}
		case flags&types.IsString != 0:
			return &ast.BasicLit{Value: `""`}
		}
	}
	return &ast.Ident{Name: "nil"}
}

func (conv *converter) identsListToExprList(idents []*ast.Ident) []ast.Expr {
	out := make([]ast.Expr, len(idents))
	for i := range idents {
		out[i] = idents[i]
	}
	return out
}

func (conv *converter) specToAssign(spec *ast.ValueSpec) *Assign {
	out := &Assign{
		Pos: spec.Pos(),
		LHS: make([]Expr, len(spec.Names)),
	}
	for i, id := range spec.Names {
		if id.Name != "_" {
			def, ok := conv.info.Defs[id].(*types.Var)
			if ok {
				out.Defs = append(out.Defs, def)
			}
		}
		out.LHS[i] = conv.expr(id)
	}
	if len(spec.Values) == 0 {
		out.RHS = make([]Expr, len(spec.Names))
		typ := conv.info.TypeOf(spec.Type)
		for i := range spec.Names {
			out.RHS[i] = conv.makeZeroVal(spec.Names[i].Pos(), typ)
		}
	} else {
		out.RHS = make([]Expr, len(spec.Values))
		for i, rhs := range spec.Values {
			out.RHS[i] = conv.expr(rhs)
		}
	}
	return out
}

func (conv *converter) makeZeroVal(pos token.Pos, typ types.Type) Expr {
	switch typ := typ.Underlying().(type) {
	case *types.Basic:
		switch flags := typ.Info(); {
		case flags&types.IsInteger != 0:
			return &IntVal{Type: typ, Pos: pos}
		case flags&types.IsFloat != 0:
			return &FloatVal{Type: typ, Pos: pos}
		case flags&types.IsString != 0:
			return &StringVal{Type: typ, Pos: pos}
		}
	case *types.Pointer:
		return &Nil{Type: typ}
	}
	panic(fmt.Sprintf("can't make zero value for %s", typ.String()))
}

func (conv *converter) assignStmt(assign *ast.AssignStmt) *Assign {
	out := &Assign{
		Pos: assign.Pos(),
		LHS: make([]Expr, len(assign.Lhs)),
		RHS: make([]Expr, len(assign.Rhs)),
	}
	for i, lhs := range assign.Rhs {
		if id, ok := lhs.(*ast.Ident); ok && id.Name != "_" {
			def, ok := conv.info.Defs[id].(*types.Var)
			if ok {
				out.Defs = append(out.Defs, def)
			}
		}
		out.LHS[i] = conv.expr(lhs)
	}
	for i, rhs := range assign.Rhs {
		out.RHS[i] = conv.expr(rhs)
	}
	return out
}
