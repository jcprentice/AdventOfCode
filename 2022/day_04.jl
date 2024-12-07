using Chain

function day_04a(; f = "input_04.txt")
	input = readlines(f)

	data = @chain input begin
		split.(_, r"-|,")
		vcat(_...)
		parse.(Int, _)
		reshape(_, 4, :)'
		collect
	end

	function fn(row)
		a, b, c, d, = row
		(a - c) * (b - d) ≤ 0
	end

	count(fn(row) for row in eachrow(data))
end

function day_04b(; f = "input_04.txt")
	input = readlines(f)

	data = @chain input begin
		split.(_, r"-|,")
		vcat(_...)
		parse.(Int, _)
		reshape(_, 4, :)'
		collect
	end

	function fn(row)
		a, b, c, d = row
		(a ≤ d && b ≥ c) || (a ≥ d && b ≤ c)
	end

	count(fn(row) for row in eachrow(data))
end

println("Day 4a: ", day_04a())
println("Day 4b: ", day_04b())

