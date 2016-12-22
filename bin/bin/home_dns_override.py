#!/usr/bin/env python

import getpass
import sys
import xmlrpclib
import urllib2

if __name__ == "__main__":

    wf_account  = 'rothenberg'           # Your WebFaction Account Name
    home_domain = 'home.danielrothenberg.com'    # The Domain to update (must exist in the Control Panel)

    wf_password = getpass.getpass()

    server = xmlrpclib.ServerProxy('https://api.webfaction.com/')
    (session_id, account) = server.login(wf_account, wf_password)

    home_override = None
    for override in server.list_dns_overrides(session_id):
        if override['domain'] == home_domain:
            home_override = override
            break

    my_ip = urllib2.urlopen('http://ip.ryansanden.com/').read().strip()

    if home_override and home_override['a_ip'] == my_ip:
        sys.exit(0)

    if home_override:
        server.delete_dns_override(session_id, home_domain, home_override['a_ip'])

    server.create_dns_override(session_id, home_domain, my_ip)