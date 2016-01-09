
MouseBox
========

MouseBox is a standlone (as in no OpenSSL dependencies) tool 
that takes care of setting up and leveraging a fake Certification
Authority suitable for https:// website development. 


Why?
----

Serving sites with https://  is particularly hard at development time, because
first one has to setup a fake certification authority, and then create the
certificates for the sites that one wishes to develop. 
As of today, most developers use OpenSSL to that end.
But since OpenSSL is geared toward *real* certificates, it requires a lot of 
bureaucracy  and even user interaction.
Furthermore, what are considered best-practices with *real, production* certificates make 
little sense or are even counter-productive when *developing* the https:// sites. 

Things get a bit more complicated when a team of multiple developers is working
on an https:// project, since every team member will have to setup his/her own
private certificate authority.  The alternative is *to distribute* a common fake
CA certificate and its private keys, which may end-up compromising the team's
browser security a bit. 

This wasn't much of problem before because developers would use HTTP/1.1 over plain http://
to develop websites. 
With the advent of HTTP/2 and [the decision of browser makers to only implement it over
TLS](http://daniel.haxx.se/blog/2015/03/06/tls-in-http2/), *developing* sites over https:// may 
become more popular. 

How do I use it?
----------------

Mousebox can be run inside a web project's directory to setup a fake local
Certification Authority and certificates.  For example, if you have at the root
of your project a file named `mouseboxf` with the following contents (of course you 
can use whatever sites you want!):

    # mouseboxf
    domains:
      - www.zunzun.se
      - ima.zunzun.se
      - www.shimmercat.com

and invoke mousebox from the directory root as:

    $ mousebox

Mousebox will automatically set up a fake local Certification Authority suitable for development, if none 
exists already, leaving the CA certificate at 

     /home/yourusername/.config/mousebox/mousebox_ca_root_xxxxxx.pem

You then need to install this unique CA certificate in your browser(s), this is needed only once per user. 

The second thing that mousebox will do is to create a leaf certificate signed by the CA in the folder "\_priv/cert.pem" in your 
project root directory, with private key in "\_priv/privkey.pem".
The created certificate will be valid for all the domains that you entered in your mouseboxf file. 

The file mouseboxf can be kept under revision control, and members of your team will just need to execute the 
mousebox command to start developing securely your secure website without having to install a common CA root 
certificate in their browsers.


Change /etc/hosts **or** configure a SOCKS proxy
--------------------------------------------

You still will need a way for the browser to identify any domains with your local IP address (most likely 127.0.0.1), at 
present that implies adding alias to "/etc/hosts" for each of your domains:

    # /etc/hosts
    127.0.0.1     www.example.com

You most likely will need to use non-standard ports to serve your applications, as usual. For example, you would 
need to navigate to https://www.example.com:4043/ when testing your site over https:// . *This should work with all
web servers*

An alternative that will make your life a lot simpler  is to use a special developer profile
in your browser and configure it to use a SOCKS 5 proxy at address 127.0.0.1:9871. [Here is how you do it in 
Google Chrome](https://www.chromium.org/developers/design-documents/network-stack/socks-proxy). The instructions
to do it in Firefox can be found [here](http://www.commandlineisking.com/2008/09/firefox-have-your-proxy-do-dns-lookups.html).
The instructions in the links above prescribe DNS resolution through the SOCKS proxy, that is needed
of course.

Then use the devlove mode of our favourite web server, ShimmerCat:

     $ shimmercat devlove

ShimmerCat already includes MouseBox, so you can just [download ShimmerCat's binary](https://www.shimmercat.com/info/articles/download/)
and get your site [up and running in less than five minutes](https://www.youtube.com/watch?v=_0VdzHEISbo). 
In development mode, ShimmerCat activates a built-in SOCKS 5 proxy in port 2006, and sets itself behind the proxy 
server. It even activates a built-in DNS server that becomes
authoritative for all your in-development domains, so you can access your site as just https://www.example.com/ 
*in your local host*, *without root privileges*, and *without needing to change /etc/hosts* . 

Using Mousebox with Shimmercat
------------------------------

Instead of having a "mouseboxf" file, you can have a file named "devlove" for Shimmercat to work in developer mode:

    shimmercat-devlove:
      domains:
        www.mysite.com:
            root-dir: www

When mousebox is ran, if there is no mouseboxf file and there is a devlove file, Mousebox will extract the domains from it. 
Instead of storing the certificates in a "\_priv" folder, Mousebox will use Shimmercat's developer mode
scratch area (usually .shimmercat.loves.devs in your project's root directory). 

Shimmercat will also invoke mousebox automatically when executed in developer mode, if there are no certificates yet.

    $ shimmercat --devlove


Compiling MouseBox as a standalone tool
---------------------------------------

MouseBox uses [Haskell Stack](http://docs.haskellstack.org/en/stable/README.html), and therefore is as easy to build 
as any other Haskell project. Just install stack, and then do:

    stack build

and then fetch the neatly standalone binary from `.stack-work/install/x86_64-linux/nightly-XXXX-XX-XX/X.XX.X/bin/mousebox-exe
where the "X"s should be filled with the nightly version and the ghc version. In most cases, you will have only one directory
there.


Setting up certificates for development with Nginx
--------------------------------------------------

*Contributions appreciated to this doc* // The certificates and private key should work with NginX

Setting up certificates for development with Apache
---------------------------------------------------

*Contributions appreciated* // The certificates and private key should work with Apache

