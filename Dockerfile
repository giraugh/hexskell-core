FROM haskell:7.10

# install node
RUN apt-get update
RUN apt-get -y install curl gnupg
RUN curl -sL https://deb.nodesource.com/setup_11.x  | bash -
RUN apt-get -y install nodejs

WORKDIR /hexskell

# Install only dependencies first
COPY ./hexskell.cabal /hexskell/hexskell.cabal
RUN cabal update
RUN cabal install --only-dependencies -j4

# Add source code and build it
ADD . /hexskell
RUN cabal install

ENTRYPOINT ["hexskell"]