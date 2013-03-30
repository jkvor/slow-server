#### Clone app

```
$ git clone git://github.com/JacobVorreuter/slow-server.git
$ cd slow-server
```

#### Create Heroku app

```
$ heroku create
```

#### Add Erlang buildpack URL

```
$ heroku config:add BUILDPACK_URL="https://github.com/archaelus/heroku-buildpack-erlang.git"
```

#### Push app

```
$ git push heroku master
```

#### Tail logs

```
$ heroku logs -t
```

#### Run test

> bin/slow_client HOST BODY_SIZE MAX_PACKET_SIZE SLEEP_MS_BTW_PACKETS

```
$ heroku run bin/slow_client YOURAPPNAME.herokuapp.com 1000 100 500
```
