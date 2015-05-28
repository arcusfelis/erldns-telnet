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


Example
-------


Run erldns:

```
erl -config erldns.config -pa ebin -pa deps/**/ebin -s erldns -boot start_sasl -sname erldns
```


Run erldns_telnet:

```
make erl
```

Register:

```
telnet 127.0.0.1 5322
Trying 127.0.0.1...
Connected to 127.0.0.1.
Escape character is '^]'.
REGISTER name example6.com ip 1.2.3.6
OK
Connection closed by foreign host.
```

Check:

```
nslookup -port=8053 example6.com 127.0.0.1
Server:         127.0.0.1
Address:        127.0.0.1#8053

Name:   example6.com
Address: 1.2.3.6
```


Calling from bash example
-----------

```
until exec 3<>/dev/tcp/127.0.0.1/5322                                                
do                                                                                   
  sleep 2                                                                            
done                                                                                 
echo -e "REGISTER name example6.com ip 1.2.3.6\r\n" >&3                              
cat <&3                                                                              
```

TODO
----

Add release support
