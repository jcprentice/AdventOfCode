function day_05a(; f = "input_05.txt")
	input = readlines(f)

	# split on the "" line
	id = findfirst(isempty, input)
	crates = input[1:id-1]
	instructions = input[id+1:end]

	# index into the crates and make stacks (letter indices are 2:4:...)
	s1 = [reverse([crates[i][n] for i in 1:9]) for n in 2:4:length(crates[1])]
	# remove the empty vlaues
	s2 = [s1[i][s1[i] .!= ' '] for i in 1:9]

	for i in instructions
		move, from, to = parse.(Int, split(i, " ")[2:2:6])
		[push!(s2[to], pop!(s2[from])) for _ in 1:move]
	end

	# turn the tops of each stack into a string
	String([s[end] for s in s2])
end

function day_05b(; f = "input_05.txt")
	input = readlines(f)

	id = findfirst(isempty, input)
	crates = input[1:id-1]
	instructions = input[id+1:end]

	s1 = [reverse([crates[i][n] for i in 1:9]) for n in 2:4:length(crates[1])]
	s2 = [s1[i][s1[i] .!= ' '] for i in 1:9]

	for i in instructions
		move, from, to = parse.(Int, split(i, " ")[2:2:6])
		# need to pop onto a new stack then reverse it
		tmpstack = Char[]
		[push!(tmpstack, pop!(s2[from])) for _ in 1:move]
		append!(s2[to], reverse(tmpstack))
	end

	String([s[end] for s in s2])
end

println("Day 5a: ", day_05a())
println("Day 5a: ", day_05b())

