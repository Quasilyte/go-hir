package hir

import (
	"fmt"
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"

	"github.com/go-toolsmith/typep"
	"golang.org/x/tools/go/ast/astutil"
)

type hirBuilder struct {
	info *types.Info
}

func (hb *hirBuilder) blockStmt(b *ast.BlockStmt) *Block {
	return &Block{
		Pos:  b.Pos(),
		List: hb.stmtList(b.List),
	}
}

func (hb *hirBuilder) exprList(in []ast.Expr) []Expr {
	out := make([]Expr, len(in))
	for i := range in {
		out[i] = hb.expr(in[i])
	}
	return out
}

func (hb *hirBuilder) stmtList(in []ast.Stmt) []Stmt {
	out := make([]Stmt, len(in))
	for i := range in {
		out[i] = hb.stmt(in[i])
	}
	return out
}

func (hb *hirBuilder) expr(expr ast.Expr) Expr {
	switch expr := expr.(type) {
	case *ast.Ident:
		return hb.ident(expr)
	case *ast.BasicLit:
		return hb.basicLit(expr)
	case *ast.ParenExpr:
		return hb.parenExpr(expr)
	case *ast.BinaryExpr:
		return hb.binaryExpr(expr)
	case *ast.CallExpr:
		return hb.callExpr(expr)

	default:
		panic(fmt.Sprintf("unhandled expr: %T", expr))
	}
}

func (hb *hirBuilder) ident(id *ast.Ident) Expr {
	tv := hb.info.Types[id]
	if tv.Value != nil {
		switch tv.Value.Kind() {
		case constant.Int:
			v, exact := constant.Int64Val(tv.Value)
			if !exact {
				panic(fmt.Sprintf("unexpected !exact int value (%s)", id.Name))
			}
			return &IntVal{
				Pos:    id.Pos(),
				Typ:    tv.Type,
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
				Typ:    tv.Type,
				Val:    v,
				Source: id,
			}

		case constant.String:
			return &StringVal{
				Pos:    id.Pos(),
				Typ:    tv.Type,
				Val:    constant.StringVal(tv.Value),
				Source: id,
			}

		case constant.Bool:
			return &BoolVal{
				Pos:    id.Pos(),
				Typ:    tv.Type,
				Val:    constant.BoolVal(tv.Value),
				Source: id,
			}

		default:
			panic(fmt.Sprintf("unhandled const kind: %v", tv.Value.Kind()))
		}
	}

	if id.Name == "nil" {
		return &Nil{Typ: tv.Type}
	}

	obj, ok := hb.info.ObjectOf(id).(*types.Var)
	if !ok || obj == nil {
		panic(fmt.Sprintf("unexpected nil object for %s", id.Name))
	}
	return &Var{Obj: obj, Pos: id.Pos()}
}

func (hb *hirBuilder) basicLit(lit *ast.BasicLit) Expr {
	tv := hb.info.Types[lit]
	switch tv.Value.Kind() {
	case constant.Int:
		v, exact := constant.Int64Val(tv.Value)
		if !exact {
			panic(fmt.Sprintf("unexpected !exact int value (%s)", lit.Value))
		}
		return &IntVal{
			Pos:    lit.Pos(),
			Typ:    tv.Type,
			Val:    v,
			Source: lit,
		}

	case constant.Float:
		v, exact := constant.Float64Val(tv.Value)
		if !exact {
			panic(fmt.Sprintf("unexpected !exact float value (%s)", lit.Value))
		}
		return &FloatVal{
			Pos:    lit.Pos(),
			Typ:    tv.Type,
			Val:    v,
			Source: lit,
		}

	case constant.String:
		return &StringVal{
			Pos:    lit.Pos(),
			Typ:    tv.Type,
			Val:    constant.StringVal(tv.Value),
			Source: lit,
		}

	default:
		panic(fmt.Sprintf("unhandled const kind: %v", tv.Value.Kind()))
	}
}

func (hb *hirBuilder) parenExpr(paren *ast.ParenExpr) Expr {
	// Reduce any amount of surrounding parenthesis to 1.
	expr := hb.expr(astutil.Unparen(paren))

	// Now assign parenthesis attribute.
	switch expr := expr.(type) {
	case *IntVal:
		expr.Parens = true
	case *FloatVal:
		expr.Parens = true
	case *StringVal:
		expr.Parens = true
	case *BoolVal:
		expr.Parens = true
	case *OpAdd:
		expr.Parens = true
	case *OpMul:
		expr.Parens = true
	case *Nil:
		expr.Parens = true
	case *TypeConv:
		expr.Parens = true
	case *Var:
		expr.Parens = true
	}

	return expr
}

func (hb *hirBuilder) binaryExpr(e *ast.BinaryExpr) Expr {
	x := hb.expr(e.X)
	y := hb.expr(e.Y)
	switch e.Op {
	case token.ADD:
		return &OpAdd{Pos: e.Pos(), LHS: x, RHS: y}
	case token.MUL:
		return &OpMul{Pos: e.Pos(), LHS: x, RHS: y}

	default:
		panic(fmt.Sprintf("unhandled binary expr: %s", e.Op.String()))
	}
}

func (hb *hirBuilder) callExpr(call *ast.CallExpr) Expr {
	if typep.IsTypeExpr(hb.info, call.Fun) {
		typ := hb.info.TypeOf(call)
		return &TypeConv{
			Typ: typ,
			Pos: call.Pos(),
			Arg: hb.expr(call.Args[0]),
		}
	}

	panic("can't handle normal calls yet")
}

func (hb *hirBuilder) stmt(stmt ast.Stmt) Stmt {
	switch stmt := stmt.(type) {
	case *ast.BlockStmt:
		return hb.blockStmt(stmt)
	case *ast.AssignStmt:
		return hb.assignStmt(stmt)
	case *ast.DeclStmt:
		return hb.declStmt(stmt)
	case *ast.ReturnStmt:
		return &Return{
			Pos:     stmt.Pos(),
			Results: hb.exprList(stmt.Results),
		}

	default:
		panic(fmt.Sprintf("unhandled stmt: %T", stmt))
	}
}

func (hb *hirBuilder) declStmt(stmt *ast.DeclStmt) Stmt {
	decl := stmt.Decl.(*ast.GenDecl)
	switch decl.Tok {
	case token.VAR:
		implicit := make([]bool, len(decl.Specs))
		types := make([]ast.Expr, len(decl.Specs))
		list := make([]*Assign, len(decl.Specs))
		for i, spec := range decl.Specs {
			spec := spec.(*ast.ValueSpec)
			list[i] = hb.specToAssign(spec)
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

func (hb *hirBuilder) specToAssign(spec *ast.ValueSpec) *Assign {
	out := &Assign{
		Pos: spec.Pos(),
		LHS: make([]Expr, len(spec.Names)),
	}
	for i, id := range spec.Names {
		if id.Name == "_" {
			out.LHS[i] = &Blank{Pos: id.Pos()}
			continue
		}
		def, ok := hb.info.Defs[id].(*types.Var)
		if ok {
			out.Defs = append(out.Defs, def)
		}
		out.LHS[i] = hb.expr(id)
	}
	if len(spec.Values) == 0 {
		out.RHS = make([]Expr, len(spec.Names))
		typ := hb.info.TypeOf(spec.Type)
		for i := range spec.Names {
			out.RHS[i] = hb.makeZeroVal(spec.Names[i].Pos(), typ)
		}
	} else {
		out.RHS = make([]Expr, len(spec.Values))
		for i, rhs := range spec.Values {
			out.RHS[i] = hb.expr(rhs)
		}
	}
	return out
}

func (hb *hirBuilder) makeZeroVal(pos token.Pos, typ types.Type) Expr {
	switch typ := typ.Underlying().(type) {
	case *types.Basic:
		switch flags := typ.Info(); {
		case flags&types.IsInteger != 0:
			return &IntVal{Typ: typ, Pos: pos}
		case flags&types.IsFloat != 0:
			return &FloatVal{Typ: typ, Pos: pos}
		case flags&types.IsString != 0:
			return &StringVal{Typ: typ, Pos: pos}
		case flags&types.IsBoolean != 0:
			return &BoolVal{Typ: typ, Pos: pos}
		}
	case *types.Pointer:
		return &Nil{Typ: typ}
	}
	panic(fmt.Sprintf("can't make zero value for %s", typ.String()))
}

func (hb *hirBuilder) assignStmt(assign *ast.AssignStmt) *Assign {
	out := &Assign{
		Pos: assign.Pos(),
		LHS: make([]Expr, len(assign.Lhs)),
		RHS: make([]Expr, len(assign.Rhs)),
	}
	for i, lhs := range assign.Lhs {
		if id, ok := lhs.(*ast.Ident); ok {
			if id.Name == "_" {
				out.LHS[i] = &Blank{Pos: id.Pos()}
				continue
			}
			def, ok := hb.info.Defs[id].(*types.Var)
			if ok {
				out.Defs = append(out.Defs, def)
			}
		}
		out.LHS[i] = hb.expr(lhs)
	}
	for i, rhs := range assign.Rhs {
		out.RHS[i] = hb.expr(rhs)
	}
	return out
}
