from functools import reduce
import re

animals = """
(#S(ANIMAL :X 49 :Y 19 :ENERGY 111 :DIR 5 :GENES (3 11 53 155 36 13 3 1)) #S(ANIMAL :X 99 :Y 5 :ENERGY 128 :DIR 5 :GENES (115 142 8 5 3 4 29 137))
 #S(ANIMAL :X 52 :Y 15 :ENERGY 183 :DIR 2 :GENES (1 11 51 155 36 12 4 1)) #S(ANIMAL :X 50 :Y 14 :ENERGY 104 :DIR 0 :GENES (1 11 52 155 36 12 5 1))
 #S(ANIMAL :X 48 :Y 9 :ENERGY 103 :DIR 1 :GENES (3 11 53 153 36 13 3 1)) #S(ANIMAL :X 32 :Y 9 :ENERGY 116 :DIR 2 :GENES (115 139 6 5 2 5 31 132))
 #S(ANIMAL :X 46 :Y 17 :ENERGY 96 :DIR 6 :GENES (2 9 52 153 35 9 2 1)) #S(ANIMAL :X 51 :Y 14 :ENERGY 106 :DIR 7 :GENES (2 10 52 153 35 8 2 2))
 #S(ANIMAL :X 51 :Y 9 :ENERGY 168 :DIR 0 :GENES (2 7 55 151 33 11 5 1)) #S(ANIMAL :X 46 :Y 11 :ENERGY 95 :DIR 1 :GENES (1 9 51 154 35 11 2 1))
 #S(ANIMAL :X 15 :Y 15 :ENERGY 159 :DIR 1 :GENES (116 139 8 4 3 1 30 135)) #S(ANIMAL :X 51 :Y 19 :ENERGY 87 :DIR 5 :GENES (1 11 51 155 34 12 4 2))
 #S(ANIMAL :X 52 :Y 6 :ENERGY 168 :DIR 1 :GENES (1 9 51 153 35 13 3 2)) #S(ANIMAL :X 38 :Y 27 :ENERGY 74 :DIR 1 :GENES (113 139 6 4 1 5 31 131))
 #S(ANIMAL :X 80 :Y 28 :ENERGY 99 :DIR 7 :GENES (114 138 5 5 1 5 30 132)) #S(ANIMAL :X 44 :Y 21 :ENERGY 73 :DIR 6 :GENES (114 141 8 5 3 4 29 137))
 #S(ANIMAL :X 86 :Y 28 :ENERGY 69 :DIR 1 :GENES (118 139 8 4 3 1 30 135)) #S(ANIMAL :X 75 :Y 14 :ENERGY 156 :DIR 2 :GENES (114 142 7 4 3 2 24 130))
 #S(ANIMAL :X 46 :Y 16 :ENERGY 153 :DIR 4 :GENES (1 12 53 155 36 12 2 1)) #S(ANIMAL :X 48 :Y 20 :ENERGY 59 :DIR 6 :GENES (1 9 51 154 36 13 2 1))
 #S(ANIMAL :X 89 :Y 15 :ENERGY 149 :DIR 3 :GENES (115 142 8 5 3 4 29 137)) #S(ANIMAL :X 40 :Y 1 :ENERGY 58 :DIR 4 :GENES (115 141 8 5 2 4 29 137))
 #S(ANIMAL :X 54 :Y 14 :ENERGY 58 :DIR 6 :GENES (1 11 51 154 36 12 4 1)) #S(ANIMAL :X 53 :Y 16 :ENERGY 104 :DIR 6 :GENES (1 11 52 155 36 12 4 1))
 #S(ANIMAL :X 47 :Y 7 :ENERGY 133 :DIR 5 :GENES (1 9 51 154 36 11 2 1)) #S(ANIMAL :X 64 :Y 8 :ENERGY 131 :DIR 7 :GENES (116 142 8 6 3 4 29 137))
 #S(ANIMAL :X 48 :Y 12 :ENERGY 137 :DIR 7 :GENES (2 10 52 153 34 9 1 1)) #S(ANIMAL :X 94 :Y 28 :ENERGY 99 :DIR 3 :GENES (114 138 5 4 1 5 30 132))
 #S(ANIMAL :X 29 :Y 27 :ENERGY 144 :DIR 0 :GENES (113 139 6 3 1 5 30 132)) #S(ANIMAL :X 53 :Y 11 :ENERGY 88 :DIR 1 :GENES (1 9 51 153 36 13 3 2))
 #S(ANIMAL :X 89 :Y 3 :ENERGY 44 :DIR 3 :GENES (117 139 8 4 3 1 30 135)) #S(ANIMAL :X 11 :Y 16 :ENERGY 46 :DIR 2 :GENES (114 140 8 6 3 3 30 137))
 #S(ANIMAL :X 50 :Y 17 :ENERGY 111 :DIR 3 :GENES (3 11 53 155 36 13 2 1)) #S(ANIMAL :X 52 :Y 12 :ENERGY 71 :DIR 4 :GENES (1 10 52 153 35 10 2 2))
 #S(ANIMAL :X 53 :Y 17 :ENERGY 38 :DIR 1 :GENES (2 7 54 151 33 11 5 1)) #S(ANIMAL :X 51 :Y 6 :ENERGY 136 :DIR 6 :GENES (1 11 51 155 35 12 4 1))
 #S(ANIMAL :X 52 :Y 19 :ENERGY 71 :DIR 7 :GENES (1 10 52 153 35 10 2 2)) #S(ANIMAL :X 46 :Y 15 :ENERGY 28 :DIR 6 :GENES (1 12 53 155 36 12 2 1))
 #S(ANIMAL :X 53 :Y 14 :ENERGY 116 :DIR 1 :GENES (1 11 52 156 35 11 4 1)) #S(ANIMAL :X 4 :Y 27 :ENERGY 23 :DIR 7 :GENES (117 139 8 4 3 1 30 135))
 #S(ANIMAL :X 45 :Y 11 :ENERGY 76 :DIR 3 :GENES (114 143 7 4 3 2 24 130)) #S(ANIMAL :X 21 :Y 26 :ENERGY 144 :DIR 0 :GENES (113 139 6 4 1 5 30 132))
 #S(ANIMAL :X 50 :Y 19 :ENERGY 16 :DIR 1 :GENES (1 9 51 154 36 10 4 1)) #S(ANIMAL :X 15 :Y 25 :ENERGY 17 :DIR 3 :GENES (117 139 9 4 3 1 30 135))
 #S(ANIMAL :X 98 :Y 24 :ENERGY 97 :DIR 5 :GENES (117 139 8 4 3 1 30 135)) #S(ANIMAL :X 67 :Y 4 :ENERGY 69 :DIR 4 :GENES (118 139 8 4 3 1 30 135))
 #S(ANIMAL :X 45 :Y 22 :ENERGY 35 :DIR 2 :GENES (114 143 6 4 4 3 24 130)) #S(ANIMAL :X 50 :Y 19 :ENERGY 90 :DIR 3 :GENES (2 11 52 153 35 10 2 2))
 #S(ANIMAL :X 46 :Y 4 :ENERGY 105 :DIR 4 :GENES (1 11 51 154 35 12 4 1)) #S(ANIMAL :X 83 :Y 10 :ENERGY 23 :DIR 7 :GENES (117 139 8 4 3 1 30 135))
 #S(ANIMAL :X 51 :Y 16 :ENERGY 67 :DIR 7 :GENES (3 11 53 155 36 13 2 1)) #S(ANIMAL :X 99 :Y 0 :ENERGY 109 :DIR 1 :GENES (114 139 6 4 1 5 30 132))
 #S(ANIMAL :X 49 :Y 13 :ENERGY 58 :DIR 7 :GENES (1 11 51 155 36 12 4 1)) #S(ANIMAL :X 61 :Y 10 :ENERGY 6 :DIR 5 :GENES (2 11 53 156 35 13 4 1))
 #S(ANIMAL :X 41 :Y 18 :ENERGY 1 :DIR 7 :GENES (1 9 52 154 36 13 3 1)) #S(ANIMAL :X 56 :Y 11 :ENERGY 24 :DIR 2 :GENES (2 11 52 153 36 13 3 1))
 #S(ANIMAL :X 35 :Y 9 :ENERGY 90 :DIR 1 :GENES (115 142 7 7 3 4 29 137)) #S(ANIMAL :X 53 :Y 14 :ENERGY 58 :DIR 0 :GENES (1 11 51 154 36 12 4 1))
 #S(ANIMAL :X 83 :Y 21 :ENERGY 67 :DIR 0 :GENES (114 138 6 4 1 5 30 132)) #S(ANIMAL :X 44 :Y 13 :ENERGY 164 :DIR 0 :GENES (1 10 51 154 36 11 4 1))
 #S(ANIMAL :X 12 :Y 11 :ENERGY 169 :DIR 6 :GENES (115 139 6 6 2 5 31 132)) #S(ANIMAL :X 70 :Y 15 :ENERGY 13 :DIR 2 :GENES (114 142 8 6 3 4 29 137))
 #S(ANIMAL :X 68 :Y 27 :ENERGY 157 :DIR 1 :GENES (116 142 8 6 3 5 29 137)) #S(ANIMAL :X 64 :Y 12 :ENERGY 11 :DIR 2 :GENES (1 11 51 155 35 12 4 1))
 #S(ANIMAL :X 74 :Y 9 :ENERGY 124 :DIR 0 :GENES (117 139 7 4 3 1 30 135)) #S(ANIMAL :X 87 :Y 28 :ENERGY 60 :DIR 0 :GENES (116 142 8 7 3 4 29 137))
 #S(ANIMAL :X 13 :Y 28 :ENERGY 79 :DIR 6 :GENES (117 139 8 4 3 1 30 135)) #S(ANIMAL :X 51 :Y 13 :ENERGY 43 :DIR 4 :GENES (1 9 51 153 36 13 3 2))
 #S(ANIMAL :X 51 :Y 9 :ENERGY 137 :DIR 4 :GENES (1 10 51 155 36 12 4 1)) #S(ANIMAL :X 76 :Y 8 :ENERGY 65 :DIR 2 :GENES (114 141 8 5 2 2 29 137))
 #S(ANIMAL :X 86 :Y 29 :ENERGY 84 :DIR 2 :GENES (114 143 6 4 3 4 24 130)) #S(ANIMAL :X 49 :Y 20 :ENERGY 153 :DIR 4 :GENES (1 12 53 155 36 12 2 1))
 #S(ANIMAL :X 51 :Y 5 :ENERGY 46 :DIR 0 :GENES (3 12 51 155 36 14 4 1)) #S(ANIMAL :X 56 :Y 10 :ENERGY 125 :DIR 3 :GENES (1 11 51 156 35 12 4 1))
 #S(ANIMAL :X 92 :Y 26 :ENERGY 65 :DIR 0 :GENES (113 140 6 5 1 5 31 132)) #S(ANIMAL :X 12 :Y 0 :ENERGY 90 :DIR 7 :GENES (115 142 7 6 3 4 29 137))
 #S(ANIMAL :X 54 :Y 21 :ENERGY 146 :DIR 1 :GENES (4 12 51 155 36 14 4 1)) #S(ANIMAL :X 11 :Y 10 :ENERGY 93 :DIR 4 :GENES (115 142 8 6 3 4 29 137))
 #S(ANIMAL :X 59 :Y 8 :ENERGY 11 :DIR 3 :GENES (1 11 51 155 35 12 4 1)) #S(ANIMAL :X 55 :Y 5 :ENERGY 44 :DIR 3 :GENES (2 11 53 156 35 12 3 1))
 #S(ANIMAL :X 34 :Y 11 :ENERGY 116 :DIR 5 :GENES (115 139 6 5 2 5 31 132)) #S(ANIMAL :X 50 :Y 13 :ENERGY 128 :DIR 4 :GENES (113 139 6 5 1 5 31 132))
 #S(ANIMAL :X 50 :Y 19 :ENERGY 128 :DIR 1 :GENES (2 10 52 153 35 10 2 2)) #S(ANIMAL :X 48 :Y 25 :ENERGY 16 :DIR 5 :GENES (1 9 51 154 36 10 4 1))
 #S(ANIMAL :X 26 :Y 10 :ENERGY 106 :DIR 4 :GENES (114 140 8 5 3 3 29 137)) #S(ANIMAL :X 59 :Y 22 :ENERGY 46 :DIR 6 :GENES (3 12 51 155 36 14 4 1))
 #S(ANIMAL :X 13 :Y 20 :ENERGY 74 :DIR 1 :GENES (113 139 6 4 1 5 31 131)) #S(ANIMAL :X 51 :Y 17 :ENERGY 104 :DIR 6 :GENES (2 11 52 153 37 13 3 1))
 #S(ANIMAL :X 86 :Y 18 :ENERGY 69 :DIR 2 :GENES (115 142 8 6 3 4 29 137)) #S(ANIMAL :X 20 :Y 15 :ENERGY 19 :DIR 4 :GENES (113 143 6 4 3 3 24 129))
 #S(ANIMAL :X 54 :Y 15 :ENERGY 183 :DIR 7 :GENES (1 11 51 155 36 12 4 1)) #S(ANIMAL :X 62 :Y 26 :ENERGY 0 :DIR 0 :GENES (113 141 8 5 2 4 29 137))
 #S(ANIMAL :X 55 :Y 20 :ENERGY 4 :DIR 6 :GENES (1 9 51 154 36 11 4 1)) #S(ANIMAL :X 46 :Y 7 :ENERGY 85 :DIR 6 :GENES (2 11 53 157 36 13 3 1))
 #S(ANIMAL :X 57 :Y 14 :ENERGY 44 :DIR 4 :GENES (3 12 51 155 36 14 4 1)) #S(ANIMAL :X 43 :Y 10 :ENERGY 137 :DIR 0 :GENES (2 10 52 153 35 9 1 1))
 #S(ANIMAL :X 44 :Y 26 :ENERGY 36 :DIR 1 :GENES (1 11 52 156 35 12 4 1)) #S(ANIMAL :X 98 :Y 4 :ENERGY 68 :DIR 2 :GENES (114 139 5 4 1 5 30 132))
 #S(ANIMAL :X 50 :Y 10 :ENERGY 103 :DIR 1 :GENES (2 11 53 153 36 13 3 1)) #S(ANIMAL :X 47 :Y 17 :ENERGY 59 :DIR 5 :GENES (1 9 51 154 36 13 3 1))
 #S(ANIMAL :X 56 :Y 14 :ENERGY 67 :DIR 3 :GENES (2 12 51 155 36 14 4 1)) #S(ANIMAL :X 47 :Y 15 :ENERGY 55 :DIR 0 :GENES (3 11 53 156 36 13 4 1))
 #S(ANIMAL :X 87 :Y 8 :ENERGY 73 :DIR 3 :GENES (113 140 5 5 1 5 31 131)) #S(ANIMAL :X 36 :Y 2 :ENERGY 54 :DIR 0 :GENES (113 143 6 4 3 3 24 130))
 #S(ANIMAL :X 68 :Y 17 :ENERGY 114 :DIR 3 :GENES (116 141 8 7 3 4 29 137)) #S(ANIMAL :X 53 :Y 9 :ENERGY 88 :DIR 6 :GENES (2 10 52 153 35 9 1 1))
 #S(ANIMAL :X 38 :Y 14 :ENERGY 65 :DIR 5 :GENES (1 11 53 156 36 13 3 1)) #S(ANIMAL :X 77 :Y 2 :ENERGY 45 :DIR 1 :GENES (115 141 8 7 3 4 29 137))
 #S(ANIMAL :X 36 :Y 2 :ENERGY 58 :DIR 3 :GENES (114 141 8 5 2 4 29 137)) #S(ANIMAL :X 50 :Y 12 :ENERGY 106 :DIR 0 :GENES (2 10 52 153 35 9 2 2))
 #S(ANIMAL :X 30 :Y 3 :ENERGY 145 :DIR 6 :GENES (113 141 4 5 3 2 24 130)) #S(ANIMAL :X 45 :Y 16 :ENERGY 96 :DIR 2 :GENES (2 10 52 153 35 9 2 1))
 #S(ANIMAL :X 79 :Y 27 :ENERGY 4 :DIR 7 :GENES (117 139 8 4 3 2 30 134)) #S(ANIMAL :X 57 :Y 8 :ENERGY 6 :DIR 0 :GENES (2 11 53 156 35 13 4 1))
 #S(ANIMAL :X 57 :Y 4 :ENERGY 167 :DIR 1 :GENES (1 9 51 154 36 12 3 1)) #S(ANIMAL :X 47 :Y 17 :ENERGY 106 :DIR 0 :GENES (2 11 53 153 37 13 3 1))
 #S(ANIMAL :X 53 :Y 15 :ENERGY 55 :DIR 2 :GENES (2 11 53 156 36 13 4 1)) #S(ANIMAL :X 41 :Y 7 :ENERGY 185 :DIR 1 :GENES (1 11 51 154 35 12 4 1))
 #S(ANIMAL :X 95 :Y 2 :ENERGY 46 :DIR 4 :GENES (114 140 8 6 3 3 29 137)) #S(ANIMAL :X 99 :Y 17 :ENERGY 64 :DIR 2 :GENES (113 141 5 5 3 2 25 130))
 #S(ANIMAL :X 45 :Y 13 :ENERGY 95 :DIR 6 :GENES (1 9 51 154 36 11 2 1)) #S(ANIMAL :X 60 :Y 15 :ENERGY 155 :DIR 3 :GENES (2 11 52 155 35 12 4 2))
 #S(ANIMAL :X 35 :Y 18 :ENERGY 23 :DIR 2 :GENES (2 10 52 154 36 10 3 1)) #S(ANIMAL :X 49 :Y 10 :ENERGY 19 :DIR 3 :GENES (1 9 51 154 36 11 2 1))
 #S(ANIMAL :X 45 :Y 16 :ENERGY 197 :DIR 6 :GENES (1 9 51 154 36 12 3 1)) #S(ANIMAL :X 50 :Y 16 :ENERGY 87 :DIR 6 :GENES (2 11 51 155 34 12 4 2))
 #S(ANIMAL :X 79 :Y 17 :ENERGY 60 :DIR 1 :GENES (116 142 8 6 3 4 29 137)) #S(ANIMAL :X 57 :Y 23 :ENERGY 65 :DIR 2 :GENES (113 139 6 5 1 5 31 132))
 #S(ANIMAL :X 89 :Y 12 :ENERGY 10 :DIR 0 :GENES (117 139 8 4 3 1 30 135)) #S(ANIMAL :X 50 :Y 6 :ENERGY 7 :DIR 1 :GENES (1 9 51 154 36 11 3 1))
 #S(ANIMAL :X 56 :Y 10 :ENERGY 107 :DIR 6 :GENES (1 9 51 154 36 11 2 1)) #S(ANIMAL :X 63 :Y 22 :ENERGY 130 :DIR 0 :GENES (113 139 5 5 1 5 31 131))
 #S(ANIMAL :X 12 :Y 9 :ENERGY 25 :DIR 1 :GENES (113 139 6 4 1 5 31 132)) #S(ANIMAL :X 81 :Y 24 :ENERGY 48 :DIR 7 :GENES (113 139 6 4 1 5 31 132))
 #S(ANIMAL :X 72 :Y 22 :ENERGY 171 :DIR 5 :GENES (114 142 5 4 3 2 26 131)) #S(ANIMAL :X 8 :Y 29 :ENERGY 29 :DIR 5 :GENES (114 141 8 5 3 4 29 137))
 #S(ANIMAL :X 49 :Y 29 :ENERGY 109 :DIR 7 :GENES (114 143 6 4 3 2 24 130)) #S(ANIMAL :X 46 :Y 17 :ENERGY 33 :DIR 3 :GENES (2 12 51 155 36 14 4 1))
 #S(ANIMAL :X 48 :Y 11 :ENERGY 125 :DIR 6 :GENES (1 11 51 156 35 12 4 1)) #S(ANIMAL :X 51 :Y 11 :ENERGY 88 :DIR 0 :GENES (2 7 54 151 33 11 5 1))
 #S(ANIMAL :X 13 :Y 29 :ENERGY 115 :DIR 2 :GENES (114 143 6 4 3 3 24 130)) #S(ANIMAL :X 45 :Y 29 :ENERGY 85 :DIR 3 :GENES (114 142 5 4 3 2 26 131))
 #S(ANIMAL :X 48 :Y 18 :ENERGY 137 :DIR 0 :GENES (1 11 51 155 36 12 4 1)) #S(ANIMAL :X 55 :Y 21 :ENERGY 97 :DIR 0 :GENES (114 139 6 4 1 5 31 131))
 #S(ANIMAL :X 1 :Y 22 :ENERGY 51 :DIR 0 :GENES (116 142 8 6 3 4 29 137)) #S(ANIMAL :X 47 :Y 18 :ENERGY 37 :DIR 1 :GENES (2 11 53 155 36 13 2 1))
 #S(ANIMAL :X 1 :Y 1 :ENERGY 128 :DIR 0 :GENES (115 142 8 6 3 4 29 137)) #S(ANIMAL :X 5 :Y 26 :ENERGY 14 :DIR 4 :GENES (117 139 8 4 3 1 30 135))
 #S(ANIMAL :X 54 :Y 9 :ENERGY 110 :DIR 1 :GENES (1 11 52 155 35 12 2 1)) #S(ANIMAL :X 41 :Y 12 :ENERGY 153 :DIR 6 :GENES (114 141 8 5 3 3 29 137)))
"""


def mapl(fn, iter):
    return list(map(fn, iter))


genomes = mapl(lambda x: mapl(int, x.split(' ')), re.findall(r"\d+ \d+ \d+ \d+ \d+ \d+ \d+ \d+", animals))
print(reduce(lambda acc, x: mapl(sum, zip(acc, x)), genomes, [0, 0, 0, 0, 0, 0, 0, 0]))
