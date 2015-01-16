FROM temporal/quicksbcl
MAINTAINER Jacek Złydach <temporal.pl@gmail.com>

ADD ./startup.lisp /tmp/startup.lisp

CMD sbcl --load /tmp/startup.lisp

EXPOSE 4005
