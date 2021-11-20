docker build --pull --rm -f "Dockerfile" -t gmmindexererlang:latest "."
cd ./gmm_client
chmod 777 ./test/indexed_queries_using_java_SUITE_data/entrypoint.sh
rm -rf ./_build
rebar3 ct