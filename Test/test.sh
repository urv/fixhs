#!/bin/sh

function stream {
for i in {1..1000}
do
  /bin/echo -n `cat ExampleFixMessages3.txt` > $FIFO &
done
}

# main

FIFO=/tmp/stream
if [ -p $FIFO ]; then
  rm $FIFO
fi 

mkfifo $FIFO
stream &
nc -l -p 3000 < $FIFO
