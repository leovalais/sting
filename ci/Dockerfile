FROM lokedhs/sbcl-quicklisp:latest
LABEL maintainer="VALAIS Léo <leo.valais97@gmail.com>"

ARG LISP=sbcl
ARG LISP_EXEC="${LISP} --script"

# We arbitrarily set $HOME to / hereby forcing to
# explicitely reset HOME in every CI whether it
# is GitLab, GH Actions, ...
# This is necessary in order to ensure that ~/common-lisp,
# ~/quicklisp and ~/.cache/quicklisp have the expected
# behaviour without additional symlinks.
ENV HOME=/
WORKDIR $HOME
RUN ln -s /root/quicklisp $HOME/quicklisp

COPY . /sting
ENV ASDF_PATH=$HOME/common-lisp
RUN mkdir -p $ASDF_PATH && ln -s /sting $ASDF_PATH/sting

ENV STING=/sting/ci/bin/sting
RUN mkdir -p /sting/ci/bin \
    && cp /sting/ci/sting-$LISP.lisp $STING
ENV PATH="/sting/ci/bin:${PATH}"

RUN /sting/ci/install-dependencies.sh \
    alexandria \
    trivial-timeout \
    lparallel \
    closer-mop \
    sting

CMD $STING
