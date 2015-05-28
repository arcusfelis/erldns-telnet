Chatserver
==========

Author: arcusfelis@gmail.com Uvarov Michael

There are several environment variables:
- `{listen_port, 5322}` - a port to listen;

Commands
--------

Compilation:
```
make
```

Test running:
```
make test
```

Server running in interactive (console) mode:
```
make erl
```

After that, to check debug messages:
```
lager:set_loglevel(lager_console_backend, debug).
```


TODO
----

Add release support
