FROM temporal/quicksbcl
MAINTAINER Jacek Złydach <temporal.pl@gmail.com>

# Configure timezone and locale
RUN echo "Europe/Warsaw" > /etc/timezone; dpkg-reconfigure -f noninteractive tzdata

ADD ./startup.lisp /tmp/startup.lisp

CMD sbcl --load /tmp/startup.lisp

EXPOSE 4005
