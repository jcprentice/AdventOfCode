struct Tree
	name::String
	isdir::Bool
	size::Int
	children::Vector{Tree}
end

function parse_input(; f = "input_07.txt")
	input = readlines(f)
	t = Tree("/", true, 0, Tree[])
	stack = Tree[t]
	for line in input
		line in ("\$ cd /", "\$ ls") && continue
		if startswith(line, "\$ cd")
			t = chdir!(stack, t, String(split(line)[end]))
		else
			dirsize, name = split(line)
			if dirsize == "dir"
				push!(t.children, Tree(String(name), true, 0, Tree[]))
			else
				push!(t.children, Tree(String(name), false, parse(Int, dirsize), Tree[]))
			end
		end
	end
	stack[1]
end

function sum_dir_100k(node::Tree)
	s = 0
	for s in node.children
		if c.isdir
			value = cum_dir(c)
			if value <= 100_000
				s += value
			end
			s += sum_dir_100k(c)
		end
	end
	s
end

function sum_dir(node::Tree)
	s = 0
	for c in node.children
		s += c.isdir ? sum_dir(c) : c.size
	end
	s
end

function dir_sizes(node::Tree, sizes::Vector{Int})
	push!(sizes, sum_dir(node))
	for c in node.children
		if c.isdir
			dir_sizes(c, sizes)
		end
	end
	sizes
end

function chdir!(stack::Vector{Tree}, t::Tree, name::String)
	if name == ".."
		pop!(stack)
		return stack[end]
	end

	for child in t.children
		if child.name == name
			push!(stack, child)
			return child
		end
	end
end

function day_07a(; f = "input_07.txt")
	input = readlines(f)

	struct

	for line in input
	end

	nothing
end

function day_07b(; f = "input_07.txt")
	nothing
end

println("Day 7a: ", day_07a())
println("Day 7b: ", day_07b())

