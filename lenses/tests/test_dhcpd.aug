module Test_dhcpd = 

let lns = Dhcpd.lns

let conf = "#
# Sample configuration file for ISC dhcpd for Debian
#
# Attention: If /etc/ltsp/dhcpd.conf exists, that will be used as
# configuration file instead of this file.
#
# $Id: dhcpd.conf,v 1.1.1.1 2002/05/21 00:07:44 peloy Exp $
#

# The ddns-updates-style parameter controls whether or not the server will
# attempt to do a DNS update when a lease is confirmed. We default to the
# behavior of the version 2 packages ('none', since DHCP v2 didn't
# have support for DDNS.)
ddns-update-style none;

# option definitions common to all supported networks...
option domain-name \"example.org\";
option domain-name-servers ns1.example.org, ns2.example.org;

default-lease-time 600;
max-lease-time 7200;

# If this DHCP server is the official DHCP server for the local
# network, the authoritative directive should be uncommented.
authoritative;

# Use this to send dhcp log messages to a different log file (you also
# have to hack syslog.conf to complete the redirection).
log-facility local7;

# No service will be given on this subnet, but declaring it helps the 
# DHCP server to understand the network topology.

subnet 10.152.187.0 netmask 255.255.255.0 {
}

# This is a very basic subnet declaration.

subnet 10.254.239.0 netmask 255.255.255.224 {
  range 10.254.239.10 10.254.239.20;
  option routers rtr-239-0-1.example.org, rtr-239-0-2.example.org;
}

# This declaration allows BOOTP clients to get dynamic addresses,
# which we don't really recommend.

subnet 10.254.239.32 netmask 255.255.255.224 {
  range dynamic-bootp 10.254.239.40 10.254.239.60;
  option broadcast-address 10.254.239.31;
  option routers rtr-239-32-1.example.org;
}

# A slightly different configuration for an internal subnet.
subnet 10.5.5.0 netmask 255.255.255.224 {
  range 10.5.5.26 10.5.5.30;
  option domain-name-servers ns1.internal.example.org;
  option domain-name \"internal.example.org\";
  option routers 10.5.5.1;
  option broadcast-address 10.5.5.31;
  default-lease-time 600;
  max-lease-time 7200;
}

# Hosts which require special configuration options can be listed in
# host statements.   If no address is specified, the address will be
# allocated dynamically (if possible), but the host-specific information
# will still come from the host declaration.

host passacaglia {
  hardware ethernet 0:0:c0:5d:bd:95;
  filename \"vmunix.passacaglia\";
  server-name \"toccata.fugue.com\";
}

# Fixed IP addresses can also be specified for hosts.   These addresses
# should not also be listed as being available for dynamic assignment.
# Hosts for which fixed IP addresses have been specified can boot using
# BOOTP or DHCP.   Hosts for which no fixed address is specified can only
# be booted with DHCP, unless there is an address range on the subnet
# to which a BOOTP client is connected which has the dynamic-bootp flag
# set.
host fantasia {
  hardware ethernet 08:00:07:26:c0:a5;
  fixed-address fantasia.fugue.com;
}

# You can declare a class of clients and then do address allocation
# based on that.   The example below shows a case where all clients
# in a certain class get addresses on the 10.17.224/24 subnet, and all
# other clients get addresses on the 10.0.29/24 subnet.

#class \"foo\" {
#  match if substring (option vendor-class-identifier, 0, 4) = \"SUNW\";
#}

shared-network 224-29 {
  subnet 10.17.224.0 netmask 255.255.255.0 {
    option routers rtr-224.example.org;
  }
  subnet 10.0.29.0 netmask 255.255.255.0 {
    option routers rtr-29.example.org;
  }
  pool {
    allow members of \"foo\";
    range 10.17.224.10 10.17.224.250;
  }
  pool {
    deny members of \"foo\";
    range 10.0.29.10 10.0.29.230;
  }
}
"

test lns get "authoritative;" = { "authoritative" }
test lns get "ddns-update-style none;" = { "ddns-update-style" = "none" }
test lns get "option domain-name \"example.org\";" = 
  { "option"
    { "domain-name" = "\"example.org\"" }
  }

test lns get "option domain-name-servers ns1.example.org, ns2.example.org;" =
  { "option"
    { "domain-name-servers" = "ns1.example.org, ns2.example.org" }
  }

test lns get "default-lease-time 600;" = { "default-lease-time" = "600" }
test lns get "range 10.254.239.60;" =   
{ "range"
    { "to" = "10.254.239.60" }
  }

test lns get "range dynamic-bootp 10.254.239.60;" = 
  { "range"
    { "flag" = "dynamic-bootp" }
    { "to" = "10.254.239.60" }
  }

test lns get "range dynamic-bootp 10.254.239.40 10.254.239.60;" = 
  { "range"
    { "flag" = "dynamic-bootp" }
    { "from" = "10.254.239.40" }
    { "to" = "10.254.239.60" }
  }

test lns get "subnet 10.152.187.0 netmask 255.255.255.0 {}\n" = 
  { "subnet"
    { "network" = "10.152.187.0" }
    { "netmask" = "255.255.255.0" }
  }

test lns get " pool {
    pool {

    }
} 
" = 
  { "pool"
    {  }
    { "pool"
      {  }
      {  }
    }
  }

test lns get "group { host Sentier-Xerox-WC5655-1 {hardware ethernet 00:00:aa:cf:47:e2;
fixed-address 10.106.64.20;}}" = 
  { "group"
    { "host" = "Sentier-Xerox-WC5655-1"
      { "hardware"
        { "type" = "ethernet" }
        { "address" = "00:00:aa:cf:47:e2" }
      }
      {  }
      { "fixed-address" = "10.106.64.20" }
    }
  }

(* FIXME: client shouldn't have to know if quotes are necessary *) 
test Dhcpd.stmt_secu get "allow members of \"foo\";" =  { "allow-members-of" = "\"foo\"" }

test lns get conf = 
  {  }
  { "#comment" = "Sample configuration file for ISC dhcpd for Debian" }
  {  }
  { "#comment" = "Attention: If /etc/ltsp/dhcpd.conf exists, that will be used as" }
  { "#comment" = "configuration file instead of this file." }
  {  }
  { "#comment" = "$Id: dhcpd.conf,v 1.1.1.1 2002/05/21 00:07:44 peloy Exp $" }
  {  }
  {  }
  { "#comment" = "The ddns-updates-style parameter controls whether or not the server will" }
  { "#comment" = "attempt to do a DNS update when a lease is confirmed. We default to the" }
  { "#comment" = "behavior of the version 2 packages ('none', since DHCP v2 didn't" }
  { "#comment" = "have support for DDNS.)" }
  { "ddns-update-style" = "none" }
  {  }
  {  }
  { "#comment" = "option definitions common to all supported networks..." }
  { "option"
    { "domain-name" = "\"example.org\"" }
  }
  {  }
  { "option"
    { "domain-name-servers" = "ns1.example.org, ns2.example.org" }
  }
  {  }
  {  }
  { "default-lease-time" = "600" }
  {  }
  { "max-lease-time" = "7200" }
  {  }
  {  }
  { "#comment" = "If this DHCP server is the official DHCP server for the local" }
  { "#comment" = "network, the authoritative directive should be uncommented." }
  { "authoritative" }
  {  }
  {  }
  { "#comment" = "Use this to send dhcp log messages to a different log file (you also" }
  { "#comment" = "have to hack syslog.conf to complete the redirection)." }
  { "log-facility" = "local7" }
  {  }
  {  }
  { "#comment" = "No service will be given on this subnet, but declaring it helps the" }
  { "#comment" = "DHCP server to understand the network topology." }
  {  }
  { "subnet"
    { "network" = "10.152.187.0" }
    { "netmask" = "255.255.255.0" }
    {  }
  }
  { "#comment" = "This is a very basic subnet declaration." }
  {  }
  { "subnet"
    { "network" = "10.254.239.0" }
    { "netmask" = "255.255.255.224" }
    {  }
    { "range"
      { "from" = "10.254.239.10" }
      { "to" = "10.254.239.20" }
    }
    {  }
    { "option"
      { "routers" = "rtr-239-0-1.example.org, rtr-239-0-2.example.org" }
    }
    {  }
  }
  { "#comment" = "This declaration allows BOOTP clients to get dynamic addresses," }
  { "#comment" = "which we don't really recommend." }
  {  }
  { "subnet"
    { "network" = "10.254.239.32" }
    { "netmask" = "255.255.255.224" }
    {  }
    { "range"
      { "flag" = "dynamic-bootp" }
      { "from" = "10.254.239.40" }
      { "to" = "10.254.239.60" }
    }
    {  }
    { "option"
      { "broadcast-address" = "10.254.239.31" }
    }
    {  }
    { "option"
      { "routers" = "rtr-239-32-1.example.org" }
    }
    {  }
  }
  { "#comment" = "A slightly different configuration for an internal subnet." }
  { "subnet"
    { "network" = "10.5.5.0" }
    { "netmask" = "255.255.255.224" }
    {  }
    { "range"
      { "from" = "10.5.5.26" }
      { "to" = "10.5.5.30" }
    }
    {  }
    { "option"
      { "domain-name-servers" = "ns1.internal.example.org" }
    }
    {  }
    { "option"
      { "domain-name" = "\"internal.example.org\"" }
    }
    {  }
    { "option"
      { "routers" = "10.5.5.1" }
    }
    {  }
    { "option"
      { "broadcast-address" = "10.5.5.31" }
    }
    {  }
    { "default-lease-time" = "600" }
    {  }
    { "max-lease-time" = "7200" }
    {  }
  }
  { "#comment" = "Hosts which require special configuration options can be listed in" }
  { "#comment" = "host statements.   If no address is specified, the address will be" }
  { "#comment" = "allocated dynamically (if possible), but the host-specific information" }
  { "#comment" = "will still come from the host declaration." }
  {  }
  { "host" = "passacaglia"
    {  }
    { "hardware"
      { "type" = "ethernet" }
      { "address" = "0:0:c0:5d:bd:95" }
    }
    {  }
    { "filename" = "\"vmunix.passacaglia\"" }
    {  }
    { "server-name" = "\"toccata.fugue.com\"" }
    {  }
  }
  { "#comment" = "Fixed IP addresses can also be specified for hosts.   These addresses" }
  { "#comment" = "should not also be listed as being available for dynamic assignment." }
  { "#comment" = "Hosts for which fixed IP addresses have been specified can boot using" }
  { "#comment" = "BOOTP or DHCP.   Hosts for which no fixed address is specified can only" }
  { "#comment" = "be booted with DHCP, unless there is an address range on the subnet" }
  { "#comment" = "to which a BOOTP client is connected which has the dynamic-bootp flag" }
  { "#comment" = "set." }
  { "host" = "fantasia"
    {  }
    { "hardware"
      { "type" = "ethernet" }
      { "address" = "08:00:07:26:c0:a5" }
    }
    {  }
    { "fixed-address" = "fantasia.fugue.com" }
    {  }
  }
  { "#comment" = "You can declare a class of clients and then do address allocation" }
  { "#comment" = "based on that.   The example below shows a case where all clients" }
  { "#comment" = "in a certain class get addresses on the 10.17.224/24 subnet, and all" }
  { "#comment" = "other clients get addresses on the 10.0.29/24 subnet." }
  {  }
  { "#comment" = "class \"foo\" {" }
  { "#comment" = "match if substring (option vendor-class-identifier, 0, 4) = \"SUNW\";" }
  { "#comment" = "}" }
  {  }
  { "shared-network" = "224-29"
    {  }
    { "subnet"
      { "network" = "10.17.224.0" }
      { "netmask" = "255.255.255.0" }
      {  }
      { "option"
        { "routers" = "rtr-224.example.org" }
      }
      {  }
    }
    { "subnet"
      { "network" = "10.0.29.0" }
      { "netmask" = "255.255.255.0" }
      {  }
      { "option"
        { "routers" = "rtr-29.example.org" }
      }
      {  }
    }
    { "pool"
      {  }
      { "allow-members-of" = "\"foo\"" }
      {  }
      { "range"
        { "from" = "10.17.224.10" }
        { "to" = "10.17.224.250" }
      }
      {  }
    }
    { "pool"
      {  }
      { "deny-members-of" = "\"foo\"" }
      {  }
      { "range"
        { "from" = "10.0.29.10" }
        { "to" = "10.0.29.230" }
      }
      {  }
    }
  }

