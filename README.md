# Haskell-Webservice

## Notes and Instuctions

**Please read Jim's note in oldREADME.md before this if you are new

This project contains a web service written in Haskell using Happstack and SQLite.
The web service provides an interface to (fictitious, randomly generated) weather 
readings taken at the North Pole during 2017. It has 4 endpoints,
the response to which will depend on the request type.

** Home Page: http://localhost:8000/weather/
Show some messages and instructions.

** Single date query: weather/date/YYYY-mm-dd
Return JSON file if weather data for that date is held.

** Insert/Update date query:  weather/date/YYYY-mm-dd/temperature
**                          (or weather/date-put/YYYY-mm-dd/temperature in web routes version)
Insert or update a date/temperature pair in the database.

** Range dates query: weather/range/d1/d2
**             where: d1  = YYYY-mm-dd
**                   d2  = YYYY-mm-dd
**                   d1 != d2
Return JSON file if weather data for dates between date d1 and d2 are held.

** Max dates query: weather/max/d1/d2
**           where: d1  = YYYY-mm-dd
**                  d2  = YYYY-mm-dd
**                  d1 != d2
Return JSON file if weather data for dates between date d1 and d2 with highest temperature are held.

** Above temperature query: weather/above/t
**                  where: t is a real number
Return JSON file if weather data for dates that have temperature higher than t.

** Send error 405 and messages on illegal request.

## Installation

** If you are using Windows, you should install CygWin and use the CygWin terminal instead
of the Windows command prompt, as several of the packages you need require a bash-like environment.**

** If you are working on this code in the labs, do not install it on a Windows drive
(e.g. `~/W_DRIVE` or `~/M_DRIVE`), as cabal is known to have problems with Windows network shares.**

** Check for update:

    $ cabal update
    $ cabal install cabal-install
	
** Clone project

    $ git clone https://github.com/ChrisTran96/Haskell-Webservice.git
    $ cd haskell-webservice

** Create sandbox (optional)

    $ cabal sandbox init

** Configure and install missing package

    $ cabal configure

** Build and install

	  $ cabal build
	  $ cabal install

** Run DB-setup

	  $ cabal run DB-setup

**Run webservice

    $ cabal run webservice

**Run webroutes (web routes version of webservice)

    $ cabal run webroutes

**Try test (work on both webservice and webroutes)
While THE SERIVICE is RUNNING open a new terminal and run the tests:

    $ cabal test

A log can be found at:
dist/test/haskell-webservice-0.1.0.0-test-webservice.log

