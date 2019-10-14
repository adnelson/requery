set -ex
for pkg in abstract postgres sqlite; do
(
set -e
cd packages/$pkg/example
npx bsb -clean-world
yarn build
)
done
