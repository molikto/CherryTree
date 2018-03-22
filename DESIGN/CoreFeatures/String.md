
# how to deal with strings

the b[] of string - in client side string is indexed sequence of 16-bit

so we currently don't talk in utf-8 now

on the server side and data side, a string is considered a b[]


the exact meaning of these bs is only got in the client renderer

with one thing: a continuous selection in renderer is a continuous selection in the b[]

~~also notice the case with bi-directional text, the selection might be cut off in two parts~~
