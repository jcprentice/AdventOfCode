using IterTools: partition

function day_03a(; f = "input_03.txt")
	input = readlines(f)

	alphas = ['a':'z'; 'A':'Z']
	values = Dict(alphas[i] => i for i in 1:52)

	# for all strings s
	# find the intersection between the 1st and 2nd halves
	# convert to priority values and sum
	sum(values[(s[1:end ÷ 2] ∩ s[(end ÷ 2 + 1):end])[1]] for s in input)
end

function day_03b(; f = "input_03.txt")
	input = readlines(f)

	alphas = ['a':'z'; 'A':'Z']
	values = Dict(alphas[i] => i for i in 1:52)

	# partition input into groups of 3
	# use collect to split the strings in each group into characters
	# find the intersect of the 3 groups
	# convert to priority values and sum
	sum(values[∩(collect.(s)...)[1]] for s in partition(input, 3))
end

println("Day 3a: ", day_03a())
println("Day 3b: ", day_03b())

