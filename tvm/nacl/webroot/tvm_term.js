// tvm_term.js -- TVM interface to Javascript VT100 emulator.
// Copyright (C) 2011  Carl Ritson <critson@perlfu.co.uk>
// 
// Based on ShellInABox.js.
// Copyright (C) 2008-2010 Markus Gutschke <markus@shellinabox.com>
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 2 as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

function extend(subClass, baseClass) {
  function inheritance() { }
  inheritance.prototype          = baseClass.prototype;
  subClass.prototype             = new inheritance();
  subClass.prototype.constructor = subClass;
  subClass.prototype.superClass  = baseClass.prototype;
};

function TVMTerminal(container) {
  this.pendingKeys  = '';
  this.keysInFlight = false;
  this.superClass.constructor.call(this, container);
};
extend(TVMTerminal, VT100);

TVMTerminal.prototype.sessionClosed = function() {
};

TVMTerminal.prototype.keysPressed = function(ch) {
  var hex = '0123456789ABCDEF';
  var s   = '';
  for (var i = 0; i < ch.length; i++) {
    var c = ch.charCodeAt(i);
    if (c < 128) {
      s += hex.charAt(c >> 4) + hex.charAt(c & 0xF);
    } else if (c < 0x800) {
      s += hex.charAt(0xC +  (c >> 10)       ) +
           hex.charAt(       (c >>  6) & 0xF ) +
           hex.charAt(0x8 + ((c >>  4) & 0x3)) +
           hex.charAt(        c        & 0xF );
    } else if (c < 0x10000) {
      s += 'E'                                 +
           hex.charAt(       (c >> 12)       ) +
           hex.charAt(0x8 +  (c >> 10) & 0x3 ) +
           hex.charAt(       (c >>  6) & 0xF ) +
           hex.charAt(0x8 + ((c >>  4) & 0x3)) +
           hex.charAt(        c        & 0xF );
    } else if (c < 0x110000) {
      s += 'F'                                 +
           hex.charAt(       (c >> 18)       ) +
           hex.charAt(0x8 +  (c >> 16) & 0x3 ) +
           hex.charAt(       (c >> 12) & 0xF ) +
           hex.charAt(0x8 +  (c >> 10) & 0x3 ) +
           hex.charAt(       (c >>  6) & 0xF ) +
           hex.charAt(0x8 + ((c >>  4) & 0x3)) +
           hex.charAt(        c        & 0xF );
    }
  }
  this.vt100(ch);
  //alert(s);
};

TVMTerminal.prototype.resized = function(w, h) {
};

TVMTerminal.prototype.extendContextMenu = function(entries, actions) {
};

TVMTerminal.prototype.about = function() {
};

