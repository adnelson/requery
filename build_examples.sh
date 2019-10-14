set -ex
for pkg in abstract postgres sqlite; do
(
set -e
cd packages/$pkg/example
yarn build
)
done
