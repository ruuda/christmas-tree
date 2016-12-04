# Christmas Tree

An API for lighting up my Christmas tree.

## Hardware

I am using the components of the [Adalight][adalight]: a 25 pixel RGB LED
strand driven by an Arduino Uno. The standard Adalight driver program runs on
the Arduino and accepts color data over USB. The Arduino presents itself as a
serial device to the computer, and the `christmas-tree` program writes color
data to the serial port.

How does the `christmas-tree` program pick the colors? Good question.

## License

Christmas Tree is free software. It is licensed under the
[GNU General Public License][gplv3], version 3.

[gplv3]:    https://www.gnu.org/licenses/gpl-3.0.html
[adalight]: https://learn.adafruit.com/adalight-diy-ambient-tv-lighting
