#
a = None
with open("Mus_Homologues.txt", "w") as out:
    for l in open("Mus_Homologues-Complete"):
        s = l[:-1].split("\t")
        if a == s[0]:
            print(l)
        else:
            a = s[0]
            out.write(l)



