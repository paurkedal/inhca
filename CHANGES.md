[v0.4.0] - 2017-10-27
---------------------

* Improve error handling in interface to openssl tools.
* Improve admin page: Show issued certificates and support revocation.
* Improve error handing an state tracking in the public pages.
* Detect missing `<keygen/>` support, but really provide a plain download
  alternative. This is not ideal, but certificate enrollment has removed
  without a complete replacement. Webcrypto contains some of the
  functionality needed, hopefully it will provide an viable alternative at
  some point.

[v0.3.1] - 2016-11-12
---------------------

* Fix server-client typing.

[v0.3.0] - 2016-09-06
---------------------

* Internal improvements including update to Eliom 5.0 and switching build
  system to topkg.

[v0.4.0]: https://github.com/paurkedal/inhca/compare/v0.3.1...v0.4.0
[v0.3.1]: https://github.com/paurkedal/inhca/compare/v0.3.0...v0.3.1
[v0.3.0]: https://github.com/paurkedal/inhca/compare/v0.2.0...v0.3.0
