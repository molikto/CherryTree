document = (heading: text, content: seq[paragraphical])

paragraphical = 
	section
	paragraph
	block
	pragraph_list
	block_image
	block_latex
	block_code
	nest_list
	definition_list
	record
	table

paragraph_list = seq[seq[paragraphical]]

section = (heading: text, content: seq[paragraphical])

block = (heading: optional(text), content: seq[paragraphical])
	// can be block quote, aside etc.

paragraph = seq[line]

textline = (content: text)

nest_list = seq[nest_list_item]

nest_list_item = (heading: text, content: seq[nest_list_item])

definition_list = seq[(heading: text, content: seq[paragraphical])]

record = seq[(heading: text, content: seq[paragraphical])]

line =
	textline
	nest_list
	block (simple)
	record (simple)
	table (simple)



