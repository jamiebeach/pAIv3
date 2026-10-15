# pAI v3 development and test toolchain. Source is bind-mounted at /v3.

# SQLite3 Multiple Ciphers (spec §17, ENC-1): every v3 store is encrypted in
# the database. Built from the pinned, checksummed amalgamation.
FROM debian:bookworm-slim@sha256:7b140f374b289a7c2befc338f42ebe6441b7ea838a042bbd5acbfca6ec875818 AS sqlite3mc
ARG SQLITE3MC_VERSION=2.5.1
ARG SQLITE_VERSION=3.53.4
ARG SQLITE3MC_SHA256=4125f8ff275ea953dabb3289331b20a0e76d4fc060f57148f4a5df3bf3b0d5e0
RUN apt-get update     && apt-get install -y --no-install-recommends ca-certificates curl unzip gcc libc6-dev     && rm -rf /var/lib/apt/lists/*
RUN set -eu;     curl -fsSL -o /tmp/mc.zip "https://github.com/utelle/SQLite3MultipleCiphers/releases/download/v${SQLITE3MC_VERSION}/sqlite3mc-${SQLITE3MC_VERSION}-sqlite-${SQLITE_VERSION}-amalgamation.zip";     echo "${SQLITE3MC_SHA256}  /tmp/mc.zip" | sha256sum -c -;     mkdir /tmp/mc && cd /tmp/mc && unzip -q /tmp/mc.zip;     gcc -O2 -fPIC -shared -DSQLITE_THREADSAFE=1 -DSQLITE_ENABLE_FTS5 -DSQLITE_DQS=0         sqlite3mc_amalgamation.c -o /tmp/libsqlite3mc.so -lpthread -ldl -lm

FROM debian:bookworm-slim@sha256:7b140f374b289a7c2befc338f42ebe6441b7ea838a042bbd5acbfca6ec875818

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates curl libsqlite3-0 sqlite3 sbcl \
    && rm -rf /var/lib/apt/lists/*

COPY docker/quicklisp.lock /tmp/quicklisp.lock

RUN set -eu; \
    . /tmp/quicklisp.lock; \
    curl -fsSL https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp; \
    echo "$QUICKLISP_BOOTSTRAP_SHA256  /tmp/quicklisp.lisp" | sha256sum -c -; \
    sbcl --noinform --non-interactive \
        --load /tmp/quicklisp.lisp \
        --eval "(quicklisp-quickstart:install :path \"/opt/quicklisp/\" :client-url \"$QUICKLISP_CLIENT_URL\" :dist-url \"$QUICKLISP_DIST_URL\")"; \
    echo "$QUICKLISP_DISTINFO_SHA256  /opt/quicklisp/dists/quicklisp/distinfo.txt" | sha256sum -c -; \
    sbcl --noinform --non-interactive \
        --load /opt/quicklisp/setup.lisp \
        --eval '(ql:quickload (quote (:cffi :shasht :ironclad :bordeaux-threads :babel :dexador :hunchentoot)))'; \
    rm -f /tmp/quicklisp.lisp /tmp/quicklisp.lock

COPY --from=sqlite3mc /tmp/libsqlite3mc.so /opt/pai/lib/libsqlite3mc.so
ENV PAI_SQLITE_LIBRARY=/opt/pai/lib/libsqlite3mc.so

RUN useradd --create-home --uid 1000 --shell /bin/bash v3 \
    && chown -R v3:v3 /opt/quicklisp
ENV PAI_QUICKLISP_SETUP=/opt/quicklisp/setup.lisp
WORKDIR /v3
USER v3
