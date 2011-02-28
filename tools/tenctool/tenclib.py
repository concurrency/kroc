class TEncParsingException(Exception):
    def __init__(self, ptr, data, range=None):
        self.ptr  = ptr
        self.data = data
        self.range = range

    def __str__(self):
        w = 12
        s_ptr = self.ptr - w
        if s_ptr < 0: s_ptr = 0
        e_ptr = s_ptr + w * 2
        if e_ptr > len(self.data): e_ptr = len(self.data)
        s = '\nError: %s @ %d (0x%X)\n' % (self.__class__.__name__, self.ptr,
                self.ptr)
        s += '  printing from %d (0x%X) to %d (0x%X)\n' % (s_ptr, s_ptr, e_ptr,
                e_ptr)
        s += '  '
        for b in self.data[s_ptr:e_ptr]:
            if ord(b) < 32 or ord(b) > 126:
                s += ' . '
            else:
                s += ' ' + b + ' '
        s += '\n  '
        for b in self.data[s_ptr:e_ptr]:
            c = hex(ord(b))[2:]
            if len(c) == 1: c = '0' + c
            s += c + ' '
        if self.range:
            s += '\n  '
            for i in range(s_ptr, e_ptr):
                if len(self.range) == 1:
                    if i == self.range[0]:
                        s += '/\\ '
                    else:
                        s += '   '
                else:
                    if i >= self.range[0] and i < self.range[1]:
                        s += '/\\ '
                    else:
                        s += '   '

        return s

class TEncBadHeader(TEncParsingException): pass
class TEncBadElementType(TEncParsingException): pass
class TEncUnimplementedType(TEncParsingException): pass
class TEncInternalParsingException(TEncParsingException): pass

def bytes_to_int(bytes):
    i = 0
    for b in bytes:
        i = ord(b) + (i << 8)
    return i

def int_to_bytes(int, word_size):
    b = ''
    while int > 0:
        b = chr(int & 0xFF) + b
        int = int >> 8
    b = ((word_size - len(b)) * '\x00') + b
    return b

def indent_str(string, indent=2, indent_char=' '):
    strings = [indent_char * indent + s for s in string.split('\n')]
    return '\n'.join(strings)

class TEnc(object):
    def __init__(self, word_size=4, elements = []):
        self.elements  = elements
        self.word_size = word_size

    def dump_bytes(self):
        bytes = ''
        for e in self.elements:
            bytes += e.bytes(self.word_size)
        if self.word_size == 2:
            tag = 'tenc' 
        else:
            tag = 'TEnc'
        return tag + int_to_bytes(len(bytes), self.word_size) + bytes

    def __str__(self):
        if self.word_size == 2:
            tag = 'tenc'
        else:
            tag = 'TEnc'
        return tag + '\n' + \
                indent_str('\n'.join([str(e) for e in self.elements]))

class TEncList(object):
    def __init__(self, pos, tag, elements):
        self.elements = elements
        self.pos      = pos
        self.tag      = tag

    def bytes(self, word_size):
        bytes = ''
        for e in self.elements:
            bytes += e.bytes(word_size)
        return self.tag + int_to_bytes(len(bytes), word_size) + bytes


    def __str__(self):
        return self.tag + '\n' + \
                indent_str('\n'.join([str(e) for e in self.elements]))

class TEncNumber(object):
    def __init__(self, pos, tag, value):
        self.value = value
        self.pos   = pos
        self.tag   = tag

    def __str__(self):
        return '%s %d' % (self.tag, self.value)

class TEncUnsignedInt(TEncNumber):
    def bytes(self, word_size):
        return self.tag + int_to_bytes(self.value, word_size)
  
class TEncSignedInt(TEncNumber): pass

class TEncByteString(object):
    def __init__(self, pos, tag, bytes):
        self.byte_string = bytes
        self.pos         = pos
        self.tag         = tag

    def bytes(self, word_size):
        bytes = self.tag + \
                int_to_bytes(len(self.byte_string), word_size) +\
                self.byte_string + \
                '\x00' * (len(self.byte_string) % word_size)
        return bytes

    def __str__(self):
        bytes = ' '.join(['0x' + hex(ord(b))[2:].zfill(2) \
                for b in self.byte_string[0:8]])
        if len(self.byte_string) <= 8:
            teaser = '[%s]' % bytes
        else:
            teaser = '[%s ...]' % bytes
        return '%s %d %s' % (self.tag, len(self.byte_string), teaser)
  
class TEncUTF8String(object):
    def __init__(self, pos, tag, bytes):
        self.utf8_string = bytes
        self.pos         = pos
        self.tag         = tag

    def bytes(self, word_size):
        bytes = self.tag + \
                int_to_bytes(len(self.utf8_string), word_size) +\
                self.utf8_string + \
                '\x00' * (len(self.utf8_string) % word_size)
        return bytes

   
    def __str__(self):
        bytes = self.utf8_string[:40].strip()
        new_bytes = ''
        for b in bytes:
            if b == '\t':
                b = '\\t'
            elif b == '\n':
                b = '\\n'
            elif b == '\r':
                b = '\\r'
            elif ord(b) < 32:
                b = '\\x%s' % (hex(ord(b))[2:].zfill(2))
            new_bytes += b
        if len(self.utf8_string) <= 40:
            teaser = '[%s]' % new_bytes
        else:
            teaser = '[%s ...]' % new_bytes
        return '%s %d %s' % (self.tag, len(self.utf8_string), teaser)
 
class TEncParser(object):
    def __init__(self, data):
        self.data = data

    def parse(self):
        pos, rest = self.parse_header(0, self.data)
        elements = []
        while rest:
            element, pos, rest = self.parse_element(pos, rest)
            elements += [element]
        return TEnc(self.word_size, elements)

    def parse_header(self, pos, data):
        if data[0:4] == 'tenc':
            self.word_size = 2
        elif data[0:4] == 'TEnc':
            self.word_size = 4
        else:
            raise TEncBadHeader(pos, self.data, [0, 4])
        if self.word_size == 2:
            self.size = bytes_to_int(data[4:6])
            pos = 6
        else:
            self.size = bytes_to_int(data[4:8])
            pos = 8
        return pos, data[pos:]

    def parse_element(self, pos, data):
        if len(data) < 3:
            raise TEncInternalParsingException(pos, self.data, [pos, pos + 4])
        type = data[3]
        if type == 'B':
            return self.parse_byte_string_element(pos, data)
        elif type == 'I':
            return self.parse_signed_int_element(pos, data)
        elif type == 'L':
            return self.parse_list_element(pos, data)
        elif type == 'S':
            return self.parse_utf8_string_element(pos, data)
        elif type == 'U':
            return self.parse_unsigned_int_element(pos, data)
        else:
            raise TEncBadElementType(pos, self.data, [pos + 3])

    def get_data(self, pos, data, len):
        return data[0:len], pos + len, data[len:]

    def get_padding(self, pos, data):
        padding = pos % self.word_size
        if padding: padding = self.word_size - padding
        return self.get_data(pos, data, padding)

    def get_tag(self, pos, data):
        return self.get_data(pos, data, 4)

    def get_unsigned_integer(self, pos, data):
        i, pos, rest = self.get_data(pos, data, self.word_size)
        return bytes_to_int(i), pos, rest

    def get_signed_integer(self, pos, data):
        raise TEncUnimplementedType(pos, self.data, [pos + 3])

    def parse_list_element(self, pos, data):
        tag, pos, rest  = self.get_tag(pos, data)
        len, pos, rest  = self.get_unsigned_integer(pos, rest)
        after_tag       = rest[len:]
        rest            = rest[:len]
        lst             = []
        elements        = []
        while rest:
            element, pos, rest = self.parse_element(pos, rest)
            elements += [element]
        return TEncList(pos, tag, elements), pos, after_tag

    def parse_unsigned_int_element(self, pos, data):
        tag, pos, rest  = self.get_tag(pos, data)
        uint, pos, rest  = self.get_unsigned_integer(pos, rest)
        return TEncUnsignedInt(pos, tag, uint), pos, rest

    def parse_signed_int_element(self, pos, data):
        tag, pos, rest  = self.get_tag(pos, data)
        sint, pos, rest  = self.get_unsigned_integer(pos, rest)
        return TEncSignedInt(pos, tag, sint), pos, rest

    def parse_byte_string_element(self, pos, data):
        tag, pos, rest  = self.get_tag(pos, data)
        len, pos, rest  = self.get_unsigned_integer(pos, rest)
        bs, pos, rest   = self.get_data(pos, rest, len)
        _, pos, rest    = self.get_padding(pos, rest)
        return TEncByteString(pos, tag, bs), pos, rest

    def parse_utf8_string_element(self, pos, data):
        tag, pos, rest  = self.get_tag(pos, data)
        len, pos, rest  = self.get_unsigned_integer(pos, rest)
        bs, pos, rest   = self.get_data(pos, rest, len)
        _, pos, rest    = self.get_padding(pos, rest)
        return TEncUTF8String(pos, tag, bs), pos, rest
