#! /usr/bin/env python3

from fontTools.ttLib import TTFont
import io
import os
import re
import sys
import xml.etree.ElementTree as ET


def ElispLiteral(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


class Glyph:
    def __init__(self, **kw):
        self.__dict__.update(kw)


def Main():
    input_file, output_file, demo_file = sys.argv[1:]
    font = TTFont(input_file, lazy=False)

    # Rename the font.
    for name in font["name"].names:
        new_name = (
            name.toUnicode()
            .replace("Fira Code", "Fira Emacs")
            .replace("FiraCode", "FiraEmacs")
        )
        font["name"].setName(
            new_name, name.nameID, name.platformID, name.platEncID, name.langID
        )

    # Get the mapping from code points to glyph names.
    cmap = font.getBestCmap()

    # Reverse the mapping.
    inv_cmap = {name: chr(code) for code, name in cmap.items()}

    # Get the list of all glyph names.
    glyph_order = font.getGlyphOrder()

    # Convert the font to XML, because I can't figure out how to make the necessary
    # updates using the fontTools API.
    sio = io.StringIO()
    font.saveXML(sio)
    root = ET.fromstring(sio.getvalue())
    del sio

    # This list will be populated with tuples of (glyph name, code point, input string)
    # representing how strings are converted to ligatures and which code point the
    # ligature glyph is assigned to.
    output_glyphs = []

    # Look at every glyph, picking out the ones that look like ligatures.
    for glyph in glyph_order:
        input_string = None
        m = re.match(r"([^.]+)\.(.*)$", glyph)
        if m:
            # We assume the name of a ligature glyph is formed from the names of
            # the component glyphs separated by underscores.
            parts = m[1].split("_")
            if all(p in inv_cmap for p in parts):
                input_string = "".join(inv_cmap[p] for p in parts)
                output_glyphs.append(
                    Glyph(name=glyph, input_string=input_string, suffix=m[2])
                )

    output_glyphs.sort(key=lambda g: (g.input_string, g.suffix))

    # Start with 0xE100 instead of 0xE000 because for some reason the original
    # font already assigns some glyphs in the private-use area.
    code_point = 0xE100
    for g in output_glyphs:
        g.code_point = code_point
        code_point += 1

    # Echo the list of strings which can be pasted into fira-code.el to update it.
    fill_column = 70
    prefix = ""
    with open("fira-code-data.el", "wt") as data_stream:
        data_stream.write(";; -*- coding: utf-8 -*-\n(defconst fira-code--data\n  '(")
        for g in output_glyphs:
            line = prefix + '[{} {} "\\{}"]  ;   {}'.format(
                ElispLiteral(g.name),
                ElispLiteral(g.input_string),
                hex(g.code_point)[1:],
                chr(g.code_point),
            )
            prefix = "    "
            data_stream.write(line + "\n")
        data_stream.write("))\n")

    # Add extra output_glyphs from code points to glyphs in the relevant cmap variants.
    for fmt in ["12", "4"]:
        for table in root.find("cmap").findall("cmap_format_" + fmt):
            for g in output_glyphs:
                ET.SubElement(table, "map", dict(code=hex(g.code_point), name=g.name))

    # Serialize the XML data and convert it to a font file.
    bio = io.BytesIO()
    ET.ElementTree(root).write(bio)
    font = TTFont()
    bio.seek(0)
    font.importXML(bio)
    font.save(output_file)


Main()
