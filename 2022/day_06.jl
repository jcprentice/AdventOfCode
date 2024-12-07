function day_06(; f = "input_06.txt", n = 4)
	input = read(f, String)

	x = fill(' ', n)
	i = 0
	while i ≤ length(input)
		i += 1
		x[mod1(i, n)] = input[i]
		i < n && continue
		allunique(x) && break
	end
	i
end

println("Day 6a: ", day_06(n = 4))
println("Day 6b: ", day_06(n = 14))
