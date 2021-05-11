FROM erlang:23

RUN mkdir /buildroot
WORKDIR /buildroot

COPY src src/
COPY include include/
COPY rebar.config .
RUN rebar3 as prod release

FROM alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs

COPY --from=0 /buildroot/_build/prod/rel/prod /prod

EXPOSE 8080
CMD ["/prod/bin/prod", "console"]
