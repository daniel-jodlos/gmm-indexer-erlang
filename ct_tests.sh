docker build --pull --rm -f "Dockerfile" -t gmmindexererlang:latest "."
cd ./gmm_client
rm -rf ./_build
./rebar3 ct