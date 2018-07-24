Essentially prints a nice 'log' of all 'done' org-mode headings.

Converts this: 

  * test
  ** test sub heading
  *** This is a sub sub test
  *** DONE This sub sub test was completed
      CLOSED: [2018-07-24 Tue 22:54]
  ** TODO test 2
  ** DONE test 3 xyz
     CLOSED: [2018-07-15 Sun 08:58]
  ** DONE test 9 abc
     CLOSED: [2018-07-15 Sun 08:58]

To this:

  Sun Jul 15 08:58:00 UTC 2018 || test || test 3 xyz
  Sun Jul 15 08:58:00 UTC 2018 || test || test 9 abc
  Tue Jul 24 22:54:00 UTC 2018 || test || test sub heading || This sub sub test was completed

