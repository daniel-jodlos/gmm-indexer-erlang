
FROM erlang:alpine

RUN mkdir /buildroot
WORKDIR /buildroot

RUN apk add -uUv git gcc g++ && \
    git clone https://github.com/erlang/rebar3.git && \
    cd rebar3 && \
    ./bootstrap

COPY src src/
COPY include include/
COPY rebar.config .
RUN rebar3 as prod release

FROM erlang:alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs

COPY --from=0 /buildroot/_build/prod/rel/prod /prod

EXPOSE 8080
CMD ["/prod/bin/prod", "console"]
