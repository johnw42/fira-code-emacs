#! /usr/bin/env python3

from fontTools.ttLib import TTFont
import io
import os
import sys
import xml.etree.ElementTree as ET


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
    ligatures = []

    # Start with 0xE100 instead of 0xE000 because for some reason the original
    # font already assigns some glyphs in the private-use area.
    code_point = 0xE100

    # Look at every glyph, picking out the ones that look like ligatures.
    for glyph in glyph_order:
        if glyph.endswith(".liga"):
            # We assume the name of a ligature glyph is formed from the names of
            # the component glyphs separated by underscores.
            parts = glyph[:-5].split("_")
            input_string = "".join(inv_cmap[part] for part in parts)
            ligatures.append((glyph, code_point, input_string))
            code_point += 1

    # Echo the list of strings which can be pasted into fira-code.el to update it.
    fill_column = 70
    elisp_indent = "   "
    elisp_lines = [elisp_indent]
    for _, _, input_string in ligatures:
        literal = ' "' + input_string.replace("\\", "\\\\").replace('"', '\\"') + '"'
        if len(literal) + len(elisp_lines[-1]) > fill_column:
            elisp_lines.append(elisp_indent)
        elisp_lines[-1] += literal
    print("Paste this code into the definition of fira-code--ligatures:")
    for line in elisp_lines:
        print(line)

    # Write the demo file, which can be loaded in Emacs to verify that ligatures are
    # displayed correctly.
    with open(demo_file, "wt") as demo_stream:
        for _, code_point, input_string in ligatures:
            demo_stream.write("{}: {}\n".format(hex(code_point), input_string))

    # Add extra mappings from code points to glyphs in the relevant cmap variants.
    for fmt in ["12", "4"]:
        for table in root.find("cmap").findall("cmap_format_" + fmt):
            for glyph, code_point, _ in ligatures:
                ET.SubElement(table, "map", dict(code=hex(code_point), name=glyph))

    # Serialize the XML data and convert it to a font file.
    bio = io.BytesIO()
    ET.ElementTree(root).write(bio)
    font = TTFont()
    bio.seek(0)
    font.importXML(bio)
    font.save(output_file)


Main()
