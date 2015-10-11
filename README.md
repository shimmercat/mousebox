
Mousebox
========

Mousebox is a standlone (as in no OpenSSL dependencies) tool 
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

What's Mousebox?
----------------

Mousebox can be run inside a web project's directory to setup a fake local
Certification Authority and certificates.  For example, if you have at the root
of your project a file named `mouseboxf` with the following contents:

    # mouseboxf
    domains:
      - www.example.com
      - my.example.com
      - *.example.com

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
mousebox command to start developing securely your secure website. 


Using Mousebox with Shimmercat
------------------------------

Instead of having a "mouseboxf" file, you can have a file named "devlove" for Shimmercat to work in developer mode:

    devlove:
        domains:
            www:
                root-dir: djangoproject/app1/static
                domain: www.zunzun.se
            img:
                root-dir: artwork/compiled/
                domain: img.zunzun.se

When mousebox is ran, if there is no mouseboxf file and there is a devlove file, Mousebox will extract the domains from it. 
Instead of storing the certificates in a "\_priv" folder, Mousebox will use Shimmercat's developer mode
scratch area (usually .shimmercat.loves.devs in your project's root directory). 

Shimmercat will also invoke mousebox automatically when executed in developer mode:

    $ shimmercat --devlove


