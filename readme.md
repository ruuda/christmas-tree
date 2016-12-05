# Christmas Tree

An API for lighting up my Christmas tree.

## Hardware

I am using the components of the [Adalight][adalight]: a 25 pixel RGB LED
strand driven by an Arduino Uno. The standard Adalight driver program runs on
the Arduino and accepts color data over USB. The Arduino presents itself as a
serial device to the computer, and the `christmas-tree` program writes color
data to the serial port.

## Software

There are two programs:

 * `server`, the API server that runs on a publicly accessible server. It
   exposes a simple API over https on one port (protected by basic auth). On a
   different port it listens for connecting clients. When a client connects, it
   the server sends it mode changes via the socket.

 * `cristmas-tree`, the program that runs in a local network and writes color
   data to the serial port. Colors are determined by the current mode, and the
   current mode is retrieved by connecting to the server and listening for mode
   changes.

## License

Christmas Tree is free software. It is licensed under the
[GNU General Public License][gplv3], version 3.

[gplv3]:    https://www.gnu.org/licenses/gpl-3.0.html
[adalight]: https://learn.adafruit.com/adalight-diy-ambient-tv-lighting
