feedbot (under construction)
=======

Feed IRC bot by Haskell

Requirement
----
 * ghc 7.4.2
 * cabal-install 1.16.0.2
 * irc 0.5.1.0
 * ircbot 0.5.3
 * HaXml 1.23.3

Usage
----

ex) runghc feedbot.hs --irc-server abc.com --port 6667 --channel "#haskell" --nick hoge --realname hoge --username hoge --feed-url "http://feed/rss" --feed-span 120

