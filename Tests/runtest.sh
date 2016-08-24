INTERPETER=$1
shift
TEST=$1
shift
exec "$INTERPETER" $@ -f "$TEST" | FileCheck "$TEST"
