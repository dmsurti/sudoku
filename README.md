### Loading

#### With Quicklisp

1. Clone sudoku into your quicklisp local-projects directory.
   ```code
   cd ~/quicklisp/local-projects
   git clone git@github.com:dmsurti/sudoku.git
   ```

2. Load with
   ```code
   (ql:quickload "sudoku")
   ```

#### ~~Without Quicklisp (But why?)~~

1. Ensure sudoku.asd is in your ASDF path.

2. Load with (asdf:oos 'asdf:load-op 'sudoku)

### Define an incomplete board and solve it

3. Create an incomplete, unsolved sudoku board as follows:
   ```code
   (setf board
         #(0 0 2 4 7 0 0 5 8 
           0 0 0 0 0 0 0 0 0 
           0 0 0 0 0 1 0 4 0 
           0 0 0 0 2 0 0 0 9 
           5 2 8 0 9 0 4 0 0 
           0 0 9 0 0 0 1 0 0 
           0 0 0 0 0 0 0 3 0 
           3 0 0 0 0 7 5 0 0 
           6 8 5 0 0 2 0 0 0))
   ```

4. To solve the sudoku board and print the solution run:
   ```code
   (sudoku:print-board (sudoku:solve board))
   ```

### Run the tests

1. You can run a test suite which tests 50 easy puzzles, 95 hard puzzles and 11
   hardest puzzles. 
   
   ```code
   (asdf:oos 'asdf:test-op :sudoku)
   ```
   
   These puzzles were downloaded from http://norvig.com/sudoku.html.
