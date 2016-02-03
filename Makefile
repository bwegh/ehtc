PROJECT = ehtc 

DEPS = cowlib uri base64url gun
dep_base64url = git https://github.com/indigo-dc-tokentranslation/base64url.git master
dep_uri = git https://github.com/indigo-dc-tokentranslation/uri.git master
include erlang.mk
