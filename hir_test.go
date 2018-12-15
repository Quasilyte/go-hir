package hir

import (
	"go/ast"
	"testing"

	"github.com/go-toolsmith/astfmt"
	"golang.org/x/tools/go/packages"
)

func TestEncDec(t *testing.T) {
	cfg := &packages.Config{Mode: packages.LoadSyntax}
	pkgs, err := packages.Load(cfg, "./testdata/encdec")
	if err != nil {
		t.Fatalf("load package: %v", err)
	}
	if packages.PrintErrors(pkgs) > 0 {
		t.Fatal("some loaded packages were not loaded successfully")
	}
	if len(pkgs) == 0 {
		t.Fatal("no packages were loaded")
	}

	for _, pkg := range pkgs {
		for _, f := range pkg.Syntax {
			for _, decl := range f.Decls {
				decl, ok := decl.(*ast.FuncDecl)
				if !ok {
					continue
				}
				b := ConvertFunc(pkg.TypesInfo, decl)
				have := astfmt.Sprint(b.GoStmt())
				want := astfmt.Sprint(decl.Body)
				if have != want {
					t.Errorf("%s func body mismatch:\nhave: %s\nwant: %s",
						decl.Name.Name, have, want)
				}
			}
		}
	}
}
