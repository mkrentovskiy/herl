#!/bin/sh
erl -pa ebin -boot start_sasl -sname herl -s herl -s reloader
