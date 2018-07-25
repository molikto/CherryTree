package command.defaults

import command.CommandCategory

class Search extends CommandCategory("search & pattern matches") {
// TODO
  // Q_pa          Pattern searches
  //
  ///     N  /{pattern}[/[offset]]<CR>
  //                        search forward for the Nth occurrence of {pattern}
  //?     N  ?{pattern}[?[offset]]<CR>
  //                        search backward for the Nth occurrence of {pattern}
  ///<CR> N  /<CR>        repeat last search, in the forward direction
  //?<CR> N  ?<CR>        repeat last search, in the backward direction
  //n     N  n            repeat last search
  //N     N  N            repeat last search, in opposite direction
  //star  N  *            search forward for the identifier under the cursor
  //#     N  #            search backward for the identifier under the cursor
  //gstar N  g*           like "*", but also find partial matches
  //g#    N  g#           like "#", but also find partial matches
  //gd       gd           goto local declaration of identifier under the cursor
  //gD       gD           goto global declaration of identifier under the cursor
  //
  //pattern               Special characters in search patterns
  //
  //                        meaning               magic   nomagic
  //                matches any single character    .       \.
  //                       matches start of line    ^       ^
  //                               matches <EOL>    $       $
  //                       matches start of word    \<      \<
  //                         matches end of word    \>      \>
  //        matches a single char from the range    [a-z]   \[a-z]
  //      matches a single char not in the range    [^a-z]  \[^a-z]
  //                  matches an identifier char    \i      \i
  //                   idem but excluding digits    \I      \I
  //                 matches a keyword character    \k      \k
  //                   idem but excluding digits    \K      \K
  //               matches a file name character    \f      \f
  //                   idem but excluding digits    \F      \F
  //               matches a printable character    \p      \p
  //                   idem but excluding digits    \P      \P
  //             matches a white space character    \s      \s
  //         matches a non-white space character    \S      \S
  //
  //                               matches <Esc>    \e      \e
  //                               matches <Tab>    \t      \t
  //                                matches <CR>    \r      \r
  //                                matches <BS>    \b      \b
  //
  //     matches 0 or more of the preceding atom    *       \*
  //     matches 1 or more of the preceding atom    \+      \+
  //        matches 0 or 1 of the preceding atom    \=      \=
  //        matches 2 to 5 of the preceding atom    \{2,5}  \{2,5}
  //                  separates two alternatives    \|      \|
  //                group a pattern into an atom    \(\)    \(\)
  //
  //search-offset         Offsets allowed after search command
  //
  //    [num]       [num] lines downwards, in column 1
  //    +[num]      [num] lines downwards, in column 1
  //    -[num]      [num] lines upwards, in column 1
  //    e[+num]     [num] characters to the right of the end of the match
  //    e[-num]     [num] characters to the left of the end of the match
  //    s[+num]     [num] characters to the right of the start of the match
  //    s[-num]     [num] characters to the left of the start of the match
  //    b[+num]     [num] identical to s[+num] above (mnemonic: begin)
  //    b[-num]     [num] identical to s[-num] above (mnemonic: begin)
  //    ;{search-command}   execute {search-command} next
}
