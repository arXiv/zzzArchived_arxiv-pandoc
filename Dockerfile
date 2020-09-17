FROM haskell:8

WORKDIR /opt/project

# TODO: install libyaml for hpack

RUN apt update -y && apt install -y --no-install-recommends && apt clean

RUN apt update -y && apt install -y --no-install-recommends \
     hpack libmagic-dev libyaml-dev \
  && apt clean

RUN cabal update

RUN echo "#!/usr/bin/env bash\n\$@\n" > /opt/entrypoint && \
  chmod a+x /opt/entrypoint

ENTRYPOINT ["/opt/entrypoint"]