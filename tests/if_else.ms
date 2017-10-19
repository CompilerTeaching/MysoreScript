var x = 4;
var y = 8;

// CHECK: x is four.
if (x == 4) {
	"x is four.\n".print();
} else {
	"x is not four.\n".print();
}
// CHECK: x has some value.
"x has some value.\n".print();

// CHECK: y is not four.
if (y == 4) {
	"y is four.\n".print();
} else {
	"y is not four.\n".print();
}
// CHECK: y has some value.
"y has some value.\n".print();