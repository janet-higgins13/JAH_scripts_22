key = open("51banana_13gp.txt","r")
my_sam = key.readlines()

colourdict = {
	"cl11_Plantain": "#163bf2",#blue
	"cl9_AAAB": "#961d12",  # darkred
	"wild_species": "#d77eca",  #pink
	"cl10_Popoulu": "#FF8C00", # dark orange
	"cl1_Sucrier": "#8B008B",  #dark magenta
	"cl7_Mutika": "#e0bfdb", #  violet
	"cl12_ABB_Bluggoe": "#1811ef", #dark blue
	"cl5_Red": "#FF0000",  #red
	"cl13_ABB_Pelipita": "#817ed7",#bright blue
	"cl3_GrosMichel": "#A52A2A",  #brown
	"cl4_GrosMichel" :"#f0c48d", # salmon
	"cl8_Plantain-like": "#75f08a", #light green
	"cl2_Cavendish": "#09851e",  #green
	"cl6_AAAA": "#f7b059", # orange
}

widthScaleFactor=2

f = open("treekey51.txt", "w")
f.write("TREE_COLORS \nSEPERATOR COMMA\nDATA\n")

for line in my_sam:
	line = line.rstrip("\n")
	pos = line.split("\t")
	sample = pos[0]
	pop = pos[1]
	print(sample + "\t" + pop)

	f.write(sample + ",branch," + colourdict[pop] + ",normal," + str(widthScaleFactor) + "\n")
	
	