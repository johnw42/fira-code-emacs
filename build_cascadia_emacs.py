#! /usr/bin/env python3

from fontTools.ttLib import TTFont
import io
import os
import re
import sys
import xml.etree.ElementTree as ET

INPUT_STRINGS = {
    "glyph00054": "",
    "glyph00055": "!!",
    "glyph00076": "!!.",
    "glyph00077": "!=",
    "glyph00086": "!==",
    "glyph00093": "!=",
    "glyph00094": "##",
    "glyph00095": "###",
    "glyph00096": "####",
    "glyph00097": "#(",
    "glyph00098": "#:",
    "glyph00099": "#:",
    "glyph00100": "#=",
    "glyph00101": "#?",
    "glyph00102": "#[",
    "glyph00103": "#_",
    "glyph00104": "#_(",
    "glyph00105": "#{",
    "glyph00106": "$",
    "glyph00107": "$>",
    "glyph00108": "%%",
    "glyph00109": "&",
    "glyph00110": "&&",
    "glyph00111": "(",
    "glyph00112": "(",
    "glyph00113": "(",
    "glyph00114": ")",
    "glyph00115": ")",
    "glyph00116": ")",
    "glyph00117": "*",
    "glyph00118": "**",
    "glyph00119": "***",
    "glyph00120": "*/",
    "glyph00121": "*>",
    "glyph00122": "+",
    "glyph00123": "+",
    "glyph00124": "+",
    "glyph00125": "++",
    "glyph00126": "+++",
    "glyph00127": "+>",
    "glyph00128": "-",
    "glyph00129": "-",
    "glyph00130": "--",
    "glyph00131": "---",
    "glyph00132": "-->",
    "glyph00133": "-<",
    "glyph00134": "-<<",
    "glyph00135": "->",
    "glyph00136": "->>",
    "glyph00137": "-|",
    "glyph00138": "-~",
    "glyph00139": ".-",
    "glyph00140": "..",
    "glyph00141": "...",
    "glyph00142": "..<",
    "glyph00143": ".=",
    "glyph00144": "..?",
    "glyph00145": "/*",
    "glyph00146": "//",
    "glyph00147": "///",
    "glyph00148": "/=",
    "glyph00149": "/==",
    "glyph00150": "/>",
    "glyph00151": "/\\",
    "glyph00163": "0",
    "glyph00167": "0",
    "glyph00168": "0",
    "glyph00169": "0",
    "glyph00170": "0",
    "glyph00171": "0",
    "glyph00172": "1",
    "glyph00173": "1",
    "glyph00174": "1",
    "glyph00175": "2",
    "glyph00176": "2",
    "glyph00177": "2",
    "glyph00178": "3",
    "glyph00179": "3",
    "glyph00180": "3",
    "glyph00181": "4",
    "glyph00182": "4",
    "glyph00183": "4",
    "glyph00184": "5",
    "glyph00185": "5",
    "glyph00186": "5",
    "glyph00187": "6",
    "glyph00188": "6",
    "glyph00189": "6",
    "glyph00190": "7",
    "glyph00191": "7",
    "glyph00192": "7",
    "glyph00193": "8",
    "glyph00194": "8",
    "glyph00195": "8",
    "glyph00196": "9",
    "glyph00197": "9",
    "glyph00198": "9",
    "glyph00199": ":",
    "glyph00200": "::",
    "glyph00201": ":::",
    "glyph00202": "::=",
    "glyph00203": ":<",
    "glyph00204": ":=",
    "glyph00205": ":>",
    "glyph00206": ";;",
    "glyph00207": "<!--",
    "glyph00208": "<$",
    "glyph00209": "<$>",
    "glyph00210": "<*",
    "glyph00211": "<*>",
    "glyph00212": "<+",
    "glyph00213": "<+>",
    "glyph00214": "<-",
    "glyph00215": "<--",
    "glyph00216": "<-<",
    "glyph00217": "<->",
    "glyph00218": "<-|",
    "glyph00219": "</",
    "glyph00220": "</>",
    "glyph00221": "<:",
    "glyph00222": "<<",
    "glyph00223": "<<-",
    "glyph00224": "<<<",
    "glyph00225": "<<=",
    "glyph00226": "<=",
    "glyph00227": "<=",
    "glyph00228": "<=<",
    "glyph00229": "<==",
    "glyph00230": "<==>",
    "glyph00231": "<=>",
    "glyph00232": "<=|",
    "glyph00233": "<>",
    "glyph00234": "<|",
    "glyph00235": "<|>",
    "glyph00236": "<||",
    "glyph00237": "<|||",
    "glyph00238": "<~",
    "glyph00240": "<~>",
}


def ElispLiteral(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


class Glyph:
    def __init__(self, **kw):
        self.__dict__.update(kw)


def Main():
    input_file = "Cascadia.ttf"
    output_file = "CascadiaEmacs.ttf"
    el_file = "cascadia-emacs-data.el"
    font = TTFont(input_file, lazy=False)

    # Rename the font.
    for name in font["name"].names:
        new_name = (
            name.toUnicode()
            .replace("Cascadia Code", "Cascadia Emacs")
            .replace("CascadiaCode", "CascadiaEmacs")
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
        m = re.match(r"glyph\d+$", glyph)
        if m:
            output_glyphs.append(Glyph(name=glyph, input_string="", suffix=""))

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
    with open(el_file, "wt") as data_stream:
        data_stream.write(
            ";; -*- coding: utf-8 -*-\n(defconst cascadia-code--data\n  '("
        )
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
