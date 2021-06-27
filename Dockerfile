
FROM erlang:alpine

RUN mkdir /buildroot
WORKDIR /buildroot

RUN apk add -uUv erlang git gcc g++ redis && \
    git clone https://github.com/erlang/rebar3.git && \
    cd rebar3 && \
    ./bootstrap

RUN apk --update add build-base erlang-crypto erlang-syntax-tools erlang-parsetools erlang-inets erlang-ssl erlang-public-key erlang-eunit \
        rm -rf /var/cache/apk/*
COPY src src/
COPY include include/
COPY rebar.config .
RUN rebar3 as prod release

FROM erlang:alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs

COPY --from=0 /buildroot/_build/prod/rel/prod /prod

CMD ["/prod/bin/prod", "console"]
