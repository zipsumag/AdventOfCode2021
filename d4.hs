{--
--- Day 4: Giant Squid ---

You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.

Maybe it wants to play bingo?

Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)

The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:

  7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
  
  22 13 17 11  0
  8  2 23  4 24
  21  9 14 16  7
  6 10  3 18  5
  1 12 20 15 19
  
  3 15  0  2 22
  9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6
  
  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
  2  0 12  3  7

After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):
  
  22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
  21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
  
After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:
  
  22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
  21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
  
Finally, 24 is drawn:
  
  22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
  8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
  21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
  6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
  1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
  
At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).

The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.

To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?

Your puzzle answer was 33462.

--- Part Two ---
On the other hand, it might be wise to try a different strategy: let the giant squid win.

You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.

In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.

Figure out which board will win last. Once it wins, what would its final score be?

Your puzzle answer was 30070.
--}

import qualified Data.List   as L
import qualified Data.Map    as M
import           CommonStuff (chunksOf, separateBy)

input = "92,12,94,64,14,4,99,71,47,59,37,73,29,7,16,32,40,53,30,76,74,39,70,88,55,45,17,0,24,65,35,20,63,68,89,84,33,66,18,50,38,10,83,75,67,42,3,56,82,34,90,46,87,52,49,2,21,62,93,86,25,78,19,57,77,26,81,15,23,31,54,48,98,11,91,85,60,72,8,69,6,22,97,96,80,95,58,36,44,1,51,43,9,61,41,79,5,27,28,13\n\
\ \n\
\ 60 79 46  9 58\n\
\ 97 81  6 94 84\n\
\ 38 40 17 61 29\n\
\ 11 28  0 91 15\n\
\ 24 77 34 59 36\n\
\ \n\
\ 52 75 98 41 92\n\
\ 73  5 29 45 62\n\
\ 17 57  0 37 93\n\
\ 59 36 35 84 18\n\
\  7 66 72 55  9\n\
\ \n\
\ 51 83 36  4 46\n\
\ 99  3 34  2  1\n\
\ 37  5 35 32 24\n\
\ 98 82 75  0 22\n\
\ 12 91 45 39 69\n\
\ \n\
\ 98 52 56 74  8\n\
\  7 29 20 85 58\n\
\ 51 95 32 77 72\n\
\ 54 81 37  1 71\n\
\  2  5 66 83 42\n\
\ \n\
\  3 60 86 77 21\n\
\ 70 53  4 57 88\n\
\ 95 50 41 34 67\n\
\  7 13 45 80 33\n\
\ 42  0 22 63 71\n\
\ \n\
\ 56 35 68 58 84\n\
\ 62 74 81 79 64\n\
\ 10 12 73 51 76\n\
\ 36 41 37 17 70\n\
\ 18 50 87 69 95\n\
\ \n\
\ 68 98 72 45 33\n\
\ 21 58 93 55  5\n\
\ 59 51 88 81  0\n\
\  1 14 69 66 34\n\
\ 64 42 11 39 83\n\
\ \n\
\ 80  8 12 47 54\n\
\ 74 71 30  5 78\n\
\ 86 11 58 35  7\n\
\ 63 44 10 51 85\n\
\ 69 91 56 40 89\n\
\ \n\
\ 76 70 95 67 23\n\
\ 98 65  3 53 80\n\
\ 14 22 71  4 60\n\
\ 26 87 94 89 13\n\
\ 82  7 32 86 12\n\
\ \n\
\ 59 52 58 93 88\n\
\ 28 45 84 64 34\n\
\ 13 23 19 60 80\n\
\ 11 38 54 27 76\n\
\ 68 74  1 36 55\n\
\ \n\
\ 46 89 75 79 19\n\
\ 67 45 34 47 90\n\
\ 72 60 68 42  8\n\
\  0 88 12 29 40\n\
\  9 99 66 43 94\n\
\ \n\
\ 74 39 77 10 11\n\
\ 14 94 13 27 61\n\
\ 21 31 87 16 52\n\
\ 95 33 71 98  5\n\
\  0 38 12 96 54\n\
\ \n\
\ 77 15 50 52 79\n\
\ 98 71 48 68 93\n\
\ 41 97 39 58 99\n\
\ 40  3 91 96  7\n\
\ 82 89 33 65  1\n\
\ \n\
\ 51 38 12 83 74\n\
\ 37 52 29  2 35\n\
\ 99 54  3 20 44\n\
\ 23 66 59 72 50\n\
\ 88  6 79 24 67\n\
\ \n\
\ 69 90 33 28 22\n\
\ 98 62 52 50 12\n\
\  8 77 61 54 59\n\
\ 11 18 49 42 17\n\
\ 20 85 48 82 27\n\
\ \n\
\ 31 77 92 20 14\n\
\ 52 83 50  6 97\n\
\ 70 76 53 67 79\n\
\  1 98 96 65 34\n\
\ 19 66 13 56 89\n\
\ \n\
\ 59 22 70 86 99\n\
\ 39 14 89 75 42\n\
\ 12 87 55 67 28\n\
\ 71 26 11 31 65\n\
\ 73 74 58 46 94\n\
\ \n\
\  8 62 56 29 42\n\
\ 83 35 53 78 26\n\
\ 73 60 49 98 18\n\
\ 84 19 16 10 25\n\
\ 80 48  0 74 63\n\
\ \n\
\ 32  5 55 44 71\n\
\  2 29 67 93 64\n\
\ 18 58  7  4 13\n\
\ 28 51 91 79 53\n\
\ 77 80 20 19 30\n\
\ \n\
\ 63 95 73 55 68\n\
\ 88 37 62 85 61\n\
\ 13 18 99 17 94\n\
\  9 49 32 76 83\n\
\  7 84 45 59  5\n\
\ \n\
\ 35 37 38 28 91\n\
\ 58 12 89 10 49\n\
\ 60  2 36 18 15\n\
\ 69 26 99 63 97\n\
\ 90 20 95 61 51\n\
\ \n\
\ 94 54 69 46 36\n\
\ 27 41 19 95 39\n\
\ 89 18 76 12 35\n\
\ 80 10 56 62 30\n\
\ 90 91 28 64 81\n\
\ \n\
\ 18 59 38 71 82\n\
\  7 15 24 89 25\n\
\ 95 70 87 34 63\n\
\ 83 33 64 50 69\n\
\ 57 32 56 93  1\n\
\ \n\
\  2 35 87 96 11\n\
\ 40 62 39 22 74\n\
\ 19  8 58 33  6\n\
\ 26 79 37  0 82\n\
\ 30 88  5 47 72\n\
\ \n\
\ 23 72 44 17 80\n\
\ 71 94  6  9 68\n\
\ 81 53 87 46 40\n\
\ 88 15 97 98 63\n\
\ 45  0 43 21 28\n\
\ \n\
\  8 33  6 38 12\n\
\ 32 75 19 13 27\n\
\  5 58 51 61 42\n\
\ 81 72  3 91 55\n\
\ 37 97 79 87 30\n\
\ \n\
\ 46 55 24 32 63\n\
\ 14 60 81 28 58\n\
\ 10 91 13 69 57\n\
\ 92 94 38 83 86\n\
\ 84 82 11 35 16\n\
\ \n\
\ 33 86 85 61 66\n\
\ 51 46  7 12 19\n\
\ 92 32 69  5 26\n\
\ 64 99 79 63 37\n\
\  4 52 54 38 16\n\
\ \n\
\ 34 48 19 54 46\n\
\ 31 82 11 91 77\n\
\ 98 38 30 69 26\n\
\ 71 84 63 24 83\n\
\ 60 53 50 20 57\n\
\ \n\
\ 31 54 46 50 65\n\
\ 21 61 89 32 94\n\
\ 28  7 72 23 18\n\
\ 42 83 60 59 98\n\
\  3 82 30 75 14\n\
\ \n\
\ 39 96 58 25 86\n\
\ 89 43 16 59 34\n\
\ 19 33 82 93 74\n\
\ 66 90 98 53 94\n\
\ 36 81 84 99 17\n\
\ \n\
\  8 80  2 82 46\n\
\ 62 28 99 29 70\n\
\ 78  6 13 53 72\n\
\ 26 47 39 95 86\n\
\ 76 91 19 41 84\n\
\ \n\
\ 36  8  4 50 25\n\
\ 56 83 72 28 95\n\
\ 43 74 46 70  5\n\
\ 55 62 81 11 35\n\
\ 40 44 82 69 71\n\
\ \n\
\ 54 26 15 74  0\n\
\  7 71 63 61 86\n\
\ 18 79 65 57 14\n\
\  9 82 81 47 78\n\
\ 24 12 20 10 76\n\
\ \n\
\ 54 37  6 41 31\n\
\ 15 24 73 48 23\n\
\ 84 53 94 28 26\n\
\ 95  4  2 32 63\n\
\ 99 74 20 64 57\n\
\ \n\
\ 18 82 17 12 50\n\
\ 49 32 63 67 40\n\
\  1 31 61 26 24\n\
\ 15 91 11 22 95\n\
\ 44 87 19 43 48\n\
\ \n\
\ 18 75  5 59 74\n\
\ 81 11 23 55 22\n\
\ 20  3 32 30 40\n\
\ 41 67 78 45 90\n\
\ 92 72 71 48 28\n\
\ \n\
\ 11 83 57 66 68\n\
\ 81 94 44 19 16\n\
\ 62 77 46 64 13\n\
\ 85 50 20 22 61\n\
\ 12 76 86 17 97\n\
\ \n\
\ 52 71 57 82 38\n\
\ 11 67 25 43 18\n\
\ 42 34 19  1 50\n\
\ 22 53  0 62 69\n\
\ 27 77 88 33 65\n\
\ \n\
\ 87 31 73 71  3\n\
\  4 39 64 13 93\n\
\ 91 82 75 43 35\n\
\ 72 89 24 11 36\n\
\ 32 79 80 17 98\n\
\ \n\
\ 56 50 78 98 37\n\
\  8 14 30 74 42\n\
\ 10 95 21 44 48\n\
\ 12 54 85 29 60\n\
\ 65  1  3 89 25\n\
\ \n\
\ 85 93 31 59 44\n\
\ 90  5 20  4 28\n\
\  6 63 83 10 27\n\
\ 40 84 48 78 68\n\
\ 72 82 71 61 52\n\
\ \n\
\ 72 84 48 77 86\n\
\ 78 69 47 82 73\n\
\ 45 70  0 11 49\n\
\ 26 95 88  7  8\n\
\ 62 12 96 15 44\n\
\ \n\
\ 86 92 29  0 49\n\
\ 17 61  1 28 75\n\
\ 46 13 42 14 23\n\
\ 65 82 44 52  9\n\
\ 41 19 37 63 83\n\
\ \n\
\ 27 70 79 26 36\n\
\ 78 23 73 84 85\n\
\ 75 32 41 14 87\n\
\ 68 99 34 93 61\n\
\ 62 86 49  9 77\n\
\ \n\
\ 88 50 27 48 23\n\
\ 96 90 30 58 55\n\
\ 29 17  3 22 49\n\
\ 99 32  8 37 42\n\
\ 13 81 87  2 53\n\
\ \n\
\ 15 51 10 87 88\n\
\ 30 19 33 46  3\n\
\ 67 13 44  7 91\n\
\ 40 83 98 86 50\n\
\ 95 80  4 35 11\n\
\ \n\
\ 79 26 92 61 24\n\
\ 73 96 86 19 88\n\
\ 54 98 46 14 48\n\
\ 78 30 39  9 66\n\
\ 56 40 11 41  1\n\
\ \n\
\ 37  0 63 71 57\n\
\ 21  6 14 52 20\n\
\ 16 36 83 76 92\n\
\ 32 25 79  2 84\n\
\ 27 89 34 99  3\n\
\ \n\
\ 74 31 87 53 20\n\
\ 32 68 12 54 73\n\
\ 97 70 57 83  7\n\
\ 96 88 11 84 29\n\
\ 23  8  9 48 65\n\
\ \n\
\ 95  0 91 41 39\n\
\  1 31 52 40 98\n\
\ 32 59 45 84 61\n\
\ 56 64 22 78 24\n\
\ 51 94 50 74 35\n\
\ \n\
\ 31 39 36  5 92\n\
\ 72 66 28  8 63\n\
\  7 20 85 55 79\n\
\ 89 90 81 94 12\n\
\ 59 44 40 65 46\n\
\ \n\
\ 80  2  7 61 34\n\
\ 31 69 15 68 77\n\
\ 85 17  8 26 38\n\
\ 55 29  1 42 86\n\
\ 28 78 66 59 23\n\
\ \n\
\ 77 56  1 73  7\n\
\ 91 55 95  0 38\n\
\ 90 19 40 74 52\n\
\ 23 59 53 34 80\n\
\ 72 50 54 96  5\n\
\ \n\
\ 88 44 64 57  9\n\
\ 68 97 81  2 75\n\
\ 37 27 25 96 42\n\
\ 72 89 24 30 45\n\
\ 93 69 86 76 19\n\
\ \n\
\ 78 46 55 84 77\n\
\ 24 52  8 97  9\n\
\  5 73  4  3 71\n\
\  0 76 70 21 15\n\
\ 33 87 38 98 63\n\
\ \n\
\ 68  4 32 82 34\n\
\ 95 43 70 74 77\n\
\ 23 89 94 61 22\n\
\ 88 51 39 90 75\n\
\ 56  9 65 29 79\n\
\ \n\
\ 23 11 51 45  1\n\
\ 40 63 21 43 37\n\
\ 64 28 84 22  9\n\
\ 86 39 77 48 61\n\
\ 10 13 15 60 79\n\
\ \n\
\ 72 34 84 54 65\n\
\ 53 58 22 38 46\n\
\ 42 33 60 85  9\n\
\ 97 59 28 40 10\n\
\ 12  6 19 92  4\n\
\ \n\
\ 39 43 23 79 29\n\
\ 33  1 24 88  8\n\
\ 61 90 73 92 97\n\
\ 95 86 77 99 27\n\
\ 25 41 69 70 64\n\
\ \n\
\ 92 57 85 51 75\n\
\ 43 38 83 47 94\n\
\ 52  5 98  1 25\n\
\ 35 46 15 34 64\n\
\ 71 31 53 65  7\n\
\ \n\
\ 89 74 98 37 64\n\
\ 51 85 33 38 96\n\
\ 54 41 11 52 86\n\
\  2 99 25 17 26\n\
\ 35 57 95 79  6\n\
\ \n\
\ 67  2 10 69 32\n\
\ 16 27 76 92 90\n\
\ 49 79 14 61 84\n\
\ 43  8 75 56 31\n\
\ 21 77 36 86 13\n\
\ \n\
\ 35 60 79 33 43\n\
\ 72  3 20 74 57\n\
\ 98 76 38 17 51\n\
\ 14  4 12 10 90\n\
\  6 55  5 93 36\n\
\ \n\
\ 48 99 10 61 98\n\
\ 62 24  3 21 30\n\
\  0 11 32  9 29\n\
\ 41 38 16 88 74\n\
\ 22  1  7  5 40\n\
\ \n\
\ 70 41 95 11 57\n\
\ 40 16  0 21 80\n\
\ 30  9 34 92 98\n\
\ 72 60 64 85 61\n\
\ 31 33 93 58 90\n\
\ \n\
\ 81 18 72 37 52\n\
\ 62  0 46 57 64\n\
\  6 34 33 20 22\n\
\ 86 87 83 31 75\n\
\ 50 84  8  5 73\n\
\ \n\
\ 50 86 31 25 27\n\
\ 20  5 54 95 53\n\
\ 89 68 41 70 32\n\
\ 93 84 11 74 22\n\
\ 44 83  8 60 36\n\
\ \n\
\ 70 99 54 57 80\n\
\ 87 75 38 37 46\n\
\ 31 56 61 44 82\n\
\ 63 74 27 19 78\n\
\ 69  6 36 62 83\n\
\ \n\
\ 94  9 73 97  6\n\
\ 55 90 60 11 89\n\
\ 18 65 68 45 35\n\
\ 88 14 10 58 93\n\
\ 77 75 96 69 25\n\
\ \n\
\ 46 25  9 57 38\n\
\ 74 86 69 62 63\n\
\ 65 44 47 77 17\n\
\  6 72 66 78  3\n\
\ 27 84 85 94 30\n\
\ \n\
\ 52 32 41 55 76\n\
\ 93 22 69 36 26\n\
\ 31 49 72 33 46\n\
\ 10  4 11 34 37\n\
\ 75 23 29 85 57\n\
\ \n\
\ 15 38 98 76 95\n\
\  4 48 39 35 58\n\
\ 19 44 55 17 94\n\
\ 37 16 61 28 49\n\
\ 33 79 67 54 96\n\
\ \n\
\ 64  6 81 22 83\n\
\ 28  3 89 49  1\n\
\ 41 25  8 18 17\n\
\ 14 26 76 48 75\n\
\ 56 24 60 68 67\n\
\ \n\
\ 18 84  1 37 97\n\
\ 92 38 79 23 45\n\
\ 39 59 17 62 77\n\
\ 13 36  5 90 71\n\
\ 51 21 94 34 70\n\
\ \n\
\ 24 76 65 37 64\n\
\ 75  0 79 53  6\n\
\ 32 46 55 27 81\n\
\ 23 45 86 96 15\n\
\ 99  5 95 59 82\n\
\ \n\
\  8 36 61 47 91\n\
\ 75 57 92 21 60\n\
\ 63  2 83 99 32\n\
\ 16 82 26 43 68\n\
\ 48 52 20 90 64\n\
\ \n\
\ 20 64 15  9 98\n\
\ 54 18 19 28 46\n\
\ 87 36 84 99 80\n\
\  5 66 68 60  7\n\
\ 56 94 52 74 77\n\
\ \n\
\ 96 54 11  8 70\n\
\ 14 97 23 30  9\n\
\ 75 83 35 32 41\n\
\ 64 85 37  2 66\n\
\ 56 89 71 44 25\n\
\ \n\
\ 44 67 40 37 82\n\
\ 87 54  6 65 92\n\
\ 59 50 17 90 85\n\
\ 73 41 16 69 98\n\
\ 24 21 94 80 13\n\
\ \n\
\ 94 67  5 91 47\n\
\ 59 23  0 10 54\n\
\ 41 16 92 74 86\n\
\ 87 60 56 34 82\n\
\ 75 15 40 33 28\n\
\ \n\
\  1  6 53 31 42\n\
\ 46 57 40 75 92\n\
\ 10 39 27 48  5\n\
\ 91 72 68 41 90\n\
\ 13 70 44 58 93\n\
\ \n\
\ 96 15 55 98 85\n\
\ 45 68 24 57 31\n\
\ 12 94 35 84 41\n\
\ 80 10 66 38 25\n\
\ 48  8 26 40 69\n\
\ \n\
\ 11 64  5  4 68\n\
\ 44 97 52 96 87\n\
\  6 22 27 47 94\n\
\ 78 79 84 54 24\n\
\ 89 16 77 37 20\n\
\ \n\
\ 80 73 21 62 64\n\
\ 30 86 15 26 91\n\
\ 45 69 37 17 84\n\
\ 78 41 47 67 33\n\
\ 82  6 79 20 83\n\
\ \n\
\ 85 43 18 19 76\n\
\  1 88 80 89 27\n\
\ 11 67 63 20 68\n\
\ 56 23 10 95  6\n\
\ 32 70 64  0 15\n\
\ \n\
\  1  2 64 84 52\n\
\ 85 36 22 72 68\n\
\ 91 19 24 25 17\n\
\  0 30 74 77 76\n\
\ 60 75 57 45 95\n\
\ \n\
\ 61 39 37 58 45\n\
\ 40 75 10 71 32\n\
\ 74 57  4 30 19\n\
\  6 70 49 11 56\n\
\ 64 36 60 66 63\n\
\ \n\
\ 67  7 36 94 34\n\
\ 92 86 74 59 62\n\
\ 72 43 40 28 21\n\
\ 52 63 96 93  2\n\
\  8 82 50 53 42\n\
\ \n\
\ 16 64 31  0 23\n\
\  3 65 59 46 54\n\
\ 52 78 20 29 50\n\
\ 91 21 93 34  7\n\
\  8 28 30 86 37\n\
\ \n\
\ 27 76 75 60 99\n\
\ 63 26  4 59 31\n\
\ 10  5 98 39 78\n\
\ 24 43 48  6 45\n\
\  0 18 82 22 35\n\
\ \n\
\ 21 48 58  4 31\n\
\ 50  6 98 43 41\n\
\ 88 80 24 35 40\n\
\ 14 45 61 10 81\n\
\ 97 27 46  8 20\n\
\ \n\
\ 36 17 86 84 56\n\
\ 82 55 99 35 75\n\
\ 64 94 92 53 31\n\
\ 42 19 29  1 23\n\
\ 76 21 11 28 66\n\
\ \n\
\ 90 92 40 41 68\n\
\ 37 89  5 60 16\n\
\ 48 25 59 45 29\n\
\ 15 75 11 30 72\n\
\  6 61 74 23 85\n\
\ \n\
\ 37 47 13 68 40\n\
\ 38 92 78 83 27\n\
\ 82 81 85 35 31\n\
\ 25 80  2 77 67\n\
\ 55  7 72 26 22\n\
\ \n\
\ 30 13 80 26 86\n\
\  4 69 62 72 87\n\
\ 97 21 99 10 24\n\
\ 61 53 42 93 32\n\
\ 67 64 15 16 27\n\
\ \n\
\ 95 13 68 86 39\n\
\  2 14 81 60 93\n\
\ 45 69 20 23 59\n\
\ 46 35 36 89  3\n\
\  5 42 29 71 49\n\
\ \n\
\ 99 49 50 92 56\n\
\ 48 59 76 82 20\n\
\ 11 53 51 86 10\n\
\  8 91 21 52 94\n\
\ 12 39 31 40 22\n\
\ \n\
\ 50 17  6 12 19\n\
\ 45 47 22 73 36\n\
\ 16 78 98 89 72\n\
\ 90 96 56 68 54\n\
\ 28 29 51 85 25\n\
\ \n\
\ 91 72 10 32 20\n\
\ 23 18 44 78 61\n\
\ 46 36 77 60 75\n\
\ 47 49 16 89  8\n\
\  2 95 48 38 85"

data BingoField = BingoField { value :: Int
                             , marked :: Bool } deriving(Eq, Show)

type BingoTable = M.Map (Int, Int) BingoField

toBingoTable :: [Int] -> BingoTable
toBingoTable v = let coordValZip   = zip [(r, c) | r <- [0..4], c <- [0..4]] v
                     coordFieldZip = map (\(c, v) -> (c, BingoField v False)) coordValZip
                 in M.fromList coordFieldZip

getRows :: BingoTable -> [[BingoField]]
getRows m = [[ m M.! (r, c) | c <- [0..4]] | r <- [0..4]]

getCols :: BingoTable -> [[BingoField]]
getCols m = [[ m M.! (r, c) | r <- [0..4]] | c <- [0..4]]

markAllFields :: BingoTable -> BingoTable
markAllFields = M.map (\f -> f { marked = True })

markField :: Int -> BingoTable -> BingoTable
markField v table = M.union markedTab table
  where matching  = M.filter ((==v) . value) table
        markedTab = M.map (\f -> f { marked = True }) matching

unmarkField :: Int -> BingoTable -> BingoTable
unmarkField v table = M.union unmarkedTab table
  where matching    = M.filter ((==v) . value) table
        unmarkedTab = M.map (\f -> f { marked = False }) matching

hasBingo :: BingoTable -> Bool
hasBingo m = any (==True) . map (all (==True) . map marked) $ (getCols m ++ getRows m)

getScore :: BingoTable -> Int -> Int
getScore table lastNum = 
  let unmarked = M.filter (not . marked) table
      sum      = M.foldl (\sum f -> sum + value f) 0 unmarked
  in sum * lastNum

runBingoFirst :: [BingoTable] -> [Int] -> Int
runBingoFirst tabs nums@(n:ns) =
  case filter hasBingo updatedTabs of
    winner:[] -> getScore winner n
    otherwise -> runBingoFirst updatedTabs ns
  where updatedTabs = map (markField n) tabs

runBingoLast :: [BingoTable] -> [Int] -> Int
runBingoLast tabs nums@(n:ns) =
  case filter (not . hasBingo) updatedTabs of
    winner:[] -> getScore (markField n winner) n
    otherwise -> runBingoLast updatedTabs ns
  where updatedTabs = map (unmarkField n) tabs

main :: IO ()
main = do
  let (drawn:rest) = lines input
  let numbers      = map (read :: String -> Int) . separateBy ',' $ drawn
  let tables       = chunksOf 25 . concat . map (map (read :: String -> Int) . words) $ rest
  let bingoTables  = map toBingoTable tables
  print (runBingoFirst bingoTables numbers)
  print (runBingoLast (map markAllFields bingoTables) (reverse numbers))