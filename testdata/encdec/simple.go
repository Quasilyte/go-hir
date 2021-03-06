package encdec

const iconst = 102
const fconst = 1.4
const sconst = "493c"
const bconst = true

func typedNil() *int {
	return (nil)
}

func nestedBlock() {
	x := 1
	{
		x := 1
		{
			x := 1
			_ = x
		}
		_ = x
	}
	_ = x
}

func implicitZeroVal() {
	var _ *int
}

func varDecl() {
	var (
		x           = 1
		y      uint = 1
		z1, z2      = 3, 4
	)
	_ = x
	_ = y
	_ = z1
	_ = z2
}

func intVal() {
	_ = 10
	_ = (10)

	var x int
	(x) = 10
	x2 := 10
	x2 = x
	_ = x2

	_ = iconst
}

func floatVal() {
	_ = 1.5
	_ = (3.5)

	var x float64
	(x) = 2.5
	_ = x
	var y float32
	y = 1.22
	_ = y

	_ = fconst
}

func stringVal() {
	_ = "34"
	_ = ("xor")

	var x string
	(x) = "2d9s"
	_ = (x)

	_ = sconst
}

func boolVal() {
	_ = true
	_ = (false)

	var x bool
	(x) = true
	_ = (x)

	_ = bconst
}

func multiRet() (int, int) {
	return 1, 2
}

func opAddMul() {
	_ = (1 + 2)
	x := 1
	y := 2
	_ = (x*2)*y + 3
}

func opSubQuo(x, y int) {
	_ = x - y - 1
	_ = x - (1-2)/2
	_ = 1 / (x - 4)
	_ = (x / y) / 2
}

func typeConv() {
	_ = int(2)
	x := 1
	_ = (float64(x))
	_ = (*int)(nil)
}

func ifelse() {
	if true {
		_ = 1
	}

	if x := 1; true {
		_ = x
	}

	if true {
		_ = 1
	} else {
		_ = 2
	}

	if x := 1; true {
		_ = x
	} else {
		_ = x
	}

	if x := true; x {
		_ = 1
	} else if true {
		_ = 2
	} else if false {
		_ = 3
	} else {
		_ = 4
	}

	if x := 1; true {
		_ = x + 1
	} else if x := 2; true {
		_ = x + 2
	} else if x := (3); true {
		_ = (x + 3)
	} else if x := 4; true {
		_ = x + 4
	}
}
