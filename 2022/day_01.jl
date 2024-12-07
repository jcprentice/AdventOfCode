function day_01(; f = "input_01.txt", best = 1)
	data = readlines(f)
	push!(data, "") # need a trailing newline

	calories = Int[]
	acc = 0
	for str in data
		if isempty(str)
			push!(calories, acc)
			acc = 0
		else
			acc += parse(Int, str)
		end
	end

	sort!(calories, rev = true)
	sum(calories[1:best])
end

println("Day 1a: ", day_01(best = 1))
println("Day 1b: ", day_01(best = 3))

