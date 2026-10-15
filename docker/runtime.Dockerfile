# Runtime for a pAI v3 agent: a bare Debian with no toolchain and no source.
# The binary and the agent's data live in one named volume at /home/pai, so
# the container mounts nothing from the host, and an update is a copy into
# the running container and a restart, not an image rebuild.
FROM debian:bookworm-slim@sha256:7b140f374b289a7c2befc338f42ebe6441b7ea838a042bbd5acbfca6ec875818

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --create-home --uid 1000 pai

USER pai
WORKDIR /home/pai
ENV AGENT_DIR=/home/pai/agent
CMD ["sh", "-c", "exec /home/pai/bin/pai run \"$AGENT_DIR\""]
