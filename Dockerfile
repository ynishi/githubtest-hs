FROM fpco/stack-build:lts-13.29 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc
RUN ls -t /opt/build/.stack-work/install/x86_64-linux/ | head -1 | xargs -i ln -s /opt/build/.stack-work/install/x86_64-linux/{} /opt/build/latest

FROM ubuntu:16.04
RUN mkdir -p /opt/githubtest
WORKDIR /opt/githubtest
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
COPY --from=build /opt/build/latest/8.6.5/bin/githubtest-exe /opt/githubtest/. 
CMD ["/opt/githubtest/githubtest-exe"]
