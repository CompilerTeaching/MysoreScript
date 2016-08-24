INTERPETER=$1
shift
TEST=$1
shift
FILECHECK=$1
shift
exec "$INTERPETER" $@ -f "$TEST" | "${FILECHECK}" "$TEST"
