
Aum is a somewhat opiniated library built on top of om next meant to be used
to build full stack apps in clojure/script. The backend implements a generic
resolver of om next queries against a sql database. The frontend lets you
build components with localized queries for data. The underlying frontend
part of the aum library then works out what needs to be requested from the
backend and what is already available. Many of the practical considerations
when it comes to building a actual production app have been incorporated
into the library such as security, database management, networking,
environment management, build system, internationalization, state management
etc. There is also a data inspector, test runner and fullstack testing
(emulating backend in frontend).

See documentation at http://www.axion5.net/slate/
