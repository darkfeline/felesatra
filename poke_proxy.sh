set -eu
cd appengine/srv/goproxy || exit 1
readonly host=https://proxy.golang.org
find * -name "*.info" -exec curl "${host}/{}" \; >/dev/null
