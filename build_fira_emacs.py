#! /usr/bin/env python3

from fontTools.ttLib import TTFont
import os
import io
import xml.etree.ElementTree as ET
import re

LIGATURES = {}

LIGATURE_NAMES = {
    "www": "",
    "**": "",
    "***": "",
    "*/": "",
    # "**/": "",
    "*>": "",
    "*/": "",
    # "\\\\": "",
    # "\\\\\\": "",
    # "{-": "",
    # "[]": "",
    "::": "",
    ":::": "",
    ":=": "",
    "!!": "",
    "!=": "",
    "!==": "",
    # "-}": "",
    "--": "",
    "---": "",
    "-->": "",
    "->": "",
    "->>": "",
    "-<": "",
    "-<<": "",
    "-~": "",
    "#{": "",
    "#[": "",
    "##": "",
    "###": "",
    "####": "",
    "#(": "",
    "#?": "",
    "#_": "",
    "#_(": "",
    ".-": "",
    ".=": "",
    "..": "",
    "..<": "",
    "...": "",
    "?=": "",
    "??": "",
    ";;": "",
    "/*": "",
    # "/**": "",
    "/=": "",
    "/==": "",
    "/>": "",
    "//": "",
    "///": "",
    "&&": "",
    "||": "",
    "||=": "",
    "|=": "",
    "|>": "",
    "^=": "",
    "$>": "",
    "++": "",
    "+++": "",
    "+>": "",
    "=:=": "",
    "==": "",
    "===": "",
    "==>": "",
    "=>": "",
    "=>>": "",
    "<=": "",
    "=<<": "",
    "=/=": "",
    ">-": "",
    ">=": "",
    ">=>": "",
    ">>": "",
    ">>-": "",
    ">>=": "",
    ">>>": "",
    "<*": "",
    "<*>": "",
    "<|": "",
    "<|>": "",
    "<$": "",
    "<$>": "",
    "<!--": "",
    "<-": "",
    "<--": "",
    "<->": "",
    "<+": "",
    "<+>": "",
    "<=": "",
    "<==": "",
    "<=>": "",
    "<=<": "",
    "<>": "",
    "<<": "",
    "<<-": "",
    "<<=": "",
    "<<<": "",
    "<~": "",
    "<~~": "",
    "</": "",
    "</>": "",
    "~@": "",
    "~-": "",
    "~=": "",
    "~>": "",
    "~~": "",
    "~~>": "",
    "%%": "",
}


def GetMatch(glyph_set):
    for lig, parts in LIGATURES.items():
        look_for = set([lig] + parts)
        if look_for == look_for & glyph_set:
            return lig
    return None


def Main():
    for v in ["Bold", "Light", "Medium", "Regular", "Retina"]:
        BuildVariant(v)


def BuildVariant(variant):
    font = TTFont(
        f"/usr/local/google/home/jrw/Downloads/FiraCode_2/otf/FiraCode-{variant}.otf",
        lazy=False,
    )

    for name in font["name"].names:
        new_name = (
            name.toUnicode()
            .replace("Fira Code", "Fira Emacs")
            .replace("FiraCode", "FiraEmacs")
        )
        font["name"].setName(
            new_name, name.nameID, name.platformID, name.platEncID, name.langID
        )

    # code point to glyph name
    cmap = font.getBestCmap()

    inv_cmap = {name: chr(code) for code, name in cmap.items()}

    # list of all glyph names
    glyph_order = font.getGlyphOrder()

    base_path = f"/usr/local/google/home/jrw/.fonts/FiraEmacs-{variant}"
    otf_path = base_path + ".otf"
    xml_path = base_path + ".ttx"

    sio = io.StringIO()
    font.saveXML(sio)
    root = ET.fromstring(sio.getvalue())
    del sio
    for fmt in ["12", "4"]:
        for table in root.find("cmap").findall("cmap_format_" + fmt):
            # for node in table.iter("map"):
            # if re.match(r"^0x[Ee]...$", node.get("code")):
            #     print("removed", node.get("name"))
            #     table.remove(node)
            code_point = 0xE100
            for glyph in glyph_order:
                if glyph.endswith(".liga"):
                    parts = glyph[:-5].split("_")
                    string = "".join(inv_cmap[part] for part in parts)
                    string = string.replace("\\", "\\\\").replace('"', '\\"')
                    ET.SubElement(table, "map", dict(code=hex(code_point), name=glyph))
                    # print(f'"{string}"')
                    # for table in font["cmap"].tables:
                    #     if table.isUnicode():
                    #         table.cmap[code_point] = glyph
                    code_point += 1

    bio = io.BytesIO()
    ET.ElementTree(root).write(bio)
    # print(bio.getvalue().decode())
    font = TTFont()
    bio.seek(0)
    font.importXML(bio)
    font.save(otf_path)

    # out_path = "/usr/local/google/home/jrw/.fonts/FiraCode-Regular-Emacs"

    # font.saveXML(out_path + ".ttx")

    # os.system(f"rm -f {out_path}.ttx; ttx {out_path}.otf")

    # for chars, name in LIGATURE_NAMES.items():
    #     char_names = [cmap[ord(c)] for c in chars]
    #     if not name:
    #         name = "_".join(char_names) + ".liga"
    #     LIGATURES[name] = char_names

    # for name in LIGATURES:
    #     if name not in glyph_order:
    #         print("wrong name:", name)
    # print(LIGATURES)

    # gsub = font["GSUB"].table

    # ss_mappings = {}
    # bt_mappings = {}

    # for lookup_index, lookup in enumerate(gsub.LookupList.Lookup):
    #     if lookup.LookupType == 1:
    #         for subst in lookup.SubTable:
    #             if subst.Format == 1:  # single subst
    #                 for key, val in subst.mapping.items():
    #                     ss_mappings.setdefault(key, []).append(val)

    # results = {}
    # for lookup_index, lookup in enumerate(gsub.LookupList.Lookup):
    #     if lookup.LookupType != 6:
    #         continue
    #     for subst in lookup.SubTable:
    #         if subst.Format != 3:
    #             continue
    #         input_glyphs = [g for c in subst.InputCoverage for g in c.glyphs]

    #         for cov_type, cov_list in [
    #             # ("la", subst.LookAheadCoverage),
    #             ("bt", subst.BacktrackCoverage)
    #         ]:
    #             for cov in cov_list:
    #                 for glyph in cov.glyphs:
    #                     for cov in subst.SubstLookupRecord:
    #                         cov_index = cov.LookupListIndex
    #                         cov_lookup = gsub.LookupList.Lookup[cov_index]
    #                         lookup_type = cov_lookup.LookupType
    #                         for cov_subst in cov_lookup.SubTable:
    #                             fmt = cov_subst.Format
    #                             if not hasattr(cov_subst, "mapping"):
    #                                 continue
    #                             mapping = cov_subst.mapping
    #                             found_glyphs = (
    #                                 set(mapping.keys())
    #                                 | set(mapping.values())
    #                                 | set([glyph])
    #                             )
    #                             lig = GetMatch(found_glyphs)
    #                             if lig:
    #                                 results.setdefault(lig, []).append(
    #                                     f"{cov_index} {cov_type}{lookup_type}{fmt} input={input_glyphs} mapping={mapping}"
    #                                 )
    # # for lig in sorted(results):
    # #     print(lig)
    # #     for data in sorted(results[lig]):
    # #         print(data)
    # #     print()

    # for lig in LIGATURES:
    #     if lig not in results:
    #         print("missing", lig)

    # # for key, vals in ss_mappings.items():
    # #     for val in vals:
    # #         print(key, val)


# at 86460
# <Lookup index="268">
#   <LookupType value="1"/>
#   <LookupFlag value="0"/>
#   <!-- SubTableCount=1 -->
#   <SingleSubst index="0" Format="1">
#     <Substitution in="plus" out="plus_plus.liga"/>
#   </SingleSubst>
# </Lookup>


# asciitilde_at.liga 1640
#
# <GSUB> <LookupList>
# <!-- LookupCount=278 -->
# <Lookup index="0">
#   <LookupType value="1"/>
#     <SingleSubst index="0" Format="2">
#       <Substitution in="at" out="asciitilde_at.liga"/>
#       offset 21
# <Lookup index="167">
#   <LookupType value="6"/>
#     <!-- SubTableCount=4 -->
#     <ChainContextSubst index="0" Format="3">
#        <!-- BacktrackGlyphCount=1 -->
#        <BacktrackCoverage index="0" Format="2">
#        offset 1626 0x65a
#
# <Lookup index="167">
#   <LookupType value="6"/>
#     <!-- SubTableCount=4 -->
#     <ChainContextSubst index="1" Format="3">
#       <LookAheadCoverage index="1" Format="2">
#         <!-- BacktrackGlyphCount=1 -->
#         <Glyph value="asciitilde_at.liga"/>
#         offset 1626

# <Lookup index="179">
#   <LookupType value="6"/>
#   <LookupFlag value="0"/>
#   <!-- SubTableCount=2 -->
#   <ChainContextSubst index="0" Format="3">
#     <!-- BacktrackGlyphCount=0 -->
#     <!-- InputGlyphCount=1 -->
#     <InputCoverage index="0" Format="1">
#       <Glyph value="LIG"/>
#     </InputCoverage>
#     <!-- LookAheadGlyphCount=1 -->
#     <LookAheadCoverage index="0" Format="1">
#       <Glyph value="asciitilde_at.liga"/>
#     </LookAheadCoverage>
#     <!-- SubstCount=1 -->
#     <SubstLookupRecord index="0">
#       <SequenceIndex value="0"/>
#       <LookupListIndex value="277"/>
#     </SubstLookupRecord>
#   </ChainContextSubst>
#   <ChainContextSubst index="1" Format="3">
#     <!-- BacktrackGlyphCount=1 -->
#     <BacktrackCoverage index="0" Format="1">
#       <Glyph value="asciitilde"/>
#     </BacktrackCoverage>
#     <!-- InputGlyphCount=1 -->
#     <InputCoverage index="0" Format="1">
#       <Glyph value="asciitilde_at.liga"/>


# <Lookup index="244">
#   <LookupType value="1"/>
#   <LookupFlag value="0"/>
#   <!-- SubTableCount=1 -->
#   <SingleSubst index="0" Format="2">
#     <Substitution in="asciitilde" out="LIG"/>
#     <Substitution in="asterisk" out="asterisk_asterisk.liga"/>
#     <Substitution in="at" out="asciitilde_at.liga"/>


# <Lookup index="277">
#   <LookupType value="1"/>
#   <LookupFlag value="0"/>
#   <!-- SubTableCount=1 -->
#   <SingleSubst index="0" Format="2">
#     <Substitution in="LIG" out="asciitilde"/>
#     <Substitution in="asciitilde_at.liga" out="at.ss06"/>


Main()
