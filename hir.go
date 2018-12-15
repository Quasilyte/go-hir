package hir

import (
	"go/ast"
	"go/types"
)

// - Type is carried inside IR nodes.
// - Some AST nodes are simplified/merged (no information loss).
// - Extendable.
// - Simpler to deal with parenthesis.

func ConvertFunc(info *types.Info, decl *ast.FuncDecl) *Block {
	if decl.Body == nil {
		return &Block{}
	}
	hb := &hirBuilder{info: info}
	return hb.blockStmt(decl.Body)
}
