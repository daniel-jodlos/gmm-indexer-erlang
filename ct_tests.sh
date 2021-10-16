docker build --pull --rm -f "Dockerfile" -t gmmindexererlang:latest "."
docker build --pull --rm -f "Dockerfile single_image" -t gmmindexererlang_single_image:latest "."
cd ./gmm_client
rm -rf ./_build
rebar3 ct