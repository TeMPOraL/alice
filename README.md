Alice Margatroid

![](alice_margatroid.png)

(credit: http://konachan.com/post/show/79852/alice_margatroid-shanghai_doll-touhou)


# Running

The simplest way to deploy Alice is to use Docker. Install Docker, and then run `sudo ./build-all` and `sudo ./run`.

# FAQ

## Alice's timezone is wrong, how do I fix it?

The default timezone is set to CET/CEST, Europe/Warsaw in particular. To change it,
go to [docker/alice/Dockerfile](docker/alice/Dockerfile), find the
string that says `"Europe/Warsaw"` and change it to the [proper value for your timezone](http://en.wikipedia.org/wiki/List_of_tz_database_time_zones).

