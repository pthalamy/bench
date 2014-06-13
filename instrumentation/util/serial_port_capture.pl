#!/usr/bin/perl

use Device::SerialPort;

my $port = Device::SerialPort->new("/dev/ttyUSB0");
$port->databits(8);
$port->baudrate(19200);
$port->parity("none");
$port->stopbits(1);
$port->handshake("none");

while(1) {
  my $byte=$port->read(1);
open (MYFILE, '>> serial_capture.log');
  print MYFILE "$byte";
close (MYFILE);
}

