using DelimitedFiles

function day_02a(; f = "input_02.txt")
	input = readdlm(f)

	scores = Dict("X" => 1, "Y" => 2, "Z" => 3)
	score = 0

	for row in eachrow(input)
		elf, me = row[1:2]

		if (elf == "A" && me == "Y") || (elf == "B" && me == "Z") || (elf == "C" && me == "X")
			score += 6
		elseif (elf == "A" && me == "X") || (elf == "B" && me == "Y") || (elf == "C" && me == "Z")
			score += 3
		end

		score += scores[me]
	end

	score
end

function day_02b(; f = "input_02.txt")
	input = readdlm(f)

	win  = Dict("A" => "P", "B" => "S", "C" => "R")
	lose = Dict("A" => "S", "B" => "R", "C" => "P")
	draw = Dict("A" => "R", "B" => "P", "C" => "S")

	scores = Dict("R" => 1, "P" => 2, "S" => 3)

	score = 0
	for row in eachrow(input)
		elf, strat = row[1:2]

		if strat == "X" # lose
			me = lose[elf]
		elseif strat == "Y" # draw
			score += 3
			me = draw[elf]
		else # win
			score += 6
			me = win[elf]
		end

		score += scores[me]
	end

	score
end

println("Day 2a: ", day_02a())
println("Day 2b: ", day_02b())
