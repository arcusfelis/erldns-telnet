#!/bin/bash
erl -pa deps/*/ebin ebin -eval "[application:start(X) || X <- [syntax_tools, compiler, goldrush, lager, ranch, erldns_telnet]]."

