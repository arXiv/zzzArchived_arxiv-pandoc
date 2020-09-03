FROM haskell:8

WORKDIR /opt/project

RUN cabal update

RUN echo "#!/usr/bin/env bash\n\$@\n" > /opt/entrypoint && \
  chmod a+x /opt/entrypoint

ENTRYPOINT ["/opt/entrypoint"]