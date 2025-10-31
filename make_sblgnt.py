import json

file = open("sblgnt.json")
gnt = json.load(file)
file.close()

comma=[" ",","]
outfile = open("src/SBLGNT.elm", "w")
print("module SBLGNT exposing (..)", file=outfile)
print(file=outfile)
print("import Array exposing (..)", file=outfile)
print("import MorphGNT exposing (..)", file=outfile)
print(file=outfile)
print("type alias GNTType = Array (Array (Array (Array GNTWordType)))", file=outfile)
print(file=outfile)
print("gntText : GNTType", file=outfile)
print("gntText = fromList [", file=outfile)
firstBook = 0
for book in gnt:
    print("    {} fromList [".format(comma[firstBook]), file=outfile)
    firstBook=1
    firstChapter = 0
    for chapter in book:
        print("      {} fromList [".format(comma[firstChapter]), file=outfile)
        firstChapter = 1
        firstVerse = 0
        for verse in chapter:
            print("        {} fromList [".format(comma[firstVerse]), file=outfile)
            firstVerse = 1
            firstWord = 0
            for word in verse:
                if len(word) < 9:
                    continue
                print("          {} parseGNTWord {} {} {} \"{}\" \"{}\" \"{}\" \"{}\" \"{}\" \"{}\"".format(comma[firstWord], word[0], word[1], word[2], word[3], word[4], word[5], word[6], word[7], word[8]), file=outfile)
                firstWord=1
            print("          ]", file=outfile)
        print("        ]", file=outfile)
    print("      ]", file=outfile)
print("      ]", file=outfile)

outfile.close()
