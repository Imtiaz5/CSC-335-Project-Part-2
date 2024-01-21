; Made by Muhammad and Imtiaz

;'enumerate-interval': generates a list of integers from 'low to 'high', and uses recursion to increment the current number and adds that to the list.
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
;proof
;Precondition:low, the starting integer, and high, the ending integer
;Postconditon:list of each recursive call's "low" values
;Base case: low > high
;IH:The recursive call: (enumerate-interval (+ low 1) high), calls the enumerate-interval function with a new starting value, (+ low 1) and the same ending value, high. 
;IS:Assuming the IH holds, cons is used to create a new list consisting of each recursive call's "low" value until low is greater then high, at which point the final list is returned.
;Stopping Condition: low > high

;append-map: a function that applies 'proc'(procedure), to each element of 'seq'(seqence), and then it uses recursion to process each elements and appends the results.
(define (append-map proc seq)
  (if (null? seq)
      '()
      (append (proc (car seq)) (append-map proc (cdr seq)))))

;proof
;Precondition: a procedure proc and a list seq
;Postconditon: a list obtained by applying proc to each element of seq, and the results are appended together.
;Base case: if seq is null, return an empty list
;Inductive Hypothesis: Assuming the recursive call (append-map proc (cdr seq)) properly returns the result for any subsequence, the current implementation of append-map
;correctly processes the current element and appends the results.
;Inductive Step: For each recursive call, the function applies proc to the first element (car seq) and appends the result to the recursive call on the rest of the sequence (cdr seq).
;The list is constructed by recursively applying proc to each element and appending the results.
;Stopping Condition: The function stops when the sequence seq is empty.

;; Helper function to remove a specified element from a list
(define (remove item lst)
  (cond ((null? lst) '())
        ((equal? item (car lst)) (remove item (cdr lst)))
        (else (cons (car lst) (remove item (cdr lst))))))
;proof
;Precondition: An item item and a list lst.
;Postcondition: The result is a new list where all occurrences of the specified item have been removed from the input list lst.
;Base Case: If the list lst is empty, the function returns an empty list ('()).
;Inductive Hypothesis (IH): Assuming the recursive call (remove item (cdr lst)) properly removes the item for any sub-list,
;the current implementation of remove correctly processes the current list.
;Inductive Step (IS): The function checks whether the first element of the list is equal to the specified item. If true, it recursively calls itself with the rest of the list,
;effectively skipping the item. If false, it cons the first element with the result of the recursive call on the rest of the list.
;Stopping Condition: The function stops when it reaches the end of the list (null? lst), and it returns the modified list.

;queens: generates all possible arrangements of queens on a chessboard
;        without threatening each other
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list '())
        (filter
          (lambda (positions) (safe? k positions))
          (append-map
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

;Pre: board-size >= 4
;Post: will return a list of list that will show valid queen positions on a chessboard of size 'board-size'
;IH: assume that the function works and correctly for 'queen-cols', it will generate all valid configurations of the first 'k-1' columns 
;IS: Assuming 'queens-cols' correctly generates configurations for the first 'k-1' columns , it will explore all valid position for the queen in the kth column and will output a list of configurations for the first k columns 
;TA: As k is decremented by the recursive call, once k becomes 0, the function will terminate and returns a valid configuration
;SC: if k becomes 0 the function will stop

;adjoin-position: Adds a new queen position to the list of already placed queens and then returns the updated lists
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))

;safe?: checks if the queen in the current column is not threatened by the other queens
;safe-pair?: a helper function that compares the coordinates between 2 queens' position
;iter: iterates through the list of already placed queens, to check if the new placement is safe

(define (safe? k positions)
  (define (safe-pair? p1 p2)
    (let ((x1 (car p1))
          (y1 (cadr p1))
          (x2 (car p2))
          (y2 (cadr p2)))
      (not (or (= x1 x2)
               (= (- x1 y1) (- x2 y2))
               (= (+ x1 y1) (+ x2 y2))))))
  (define (iter items)
    (cond ((null? items) #t)
          ((safe-pair? (last positions) (car items)) (iter (cdr items)))
          (else #f)))
  (iter (reverse (cdr (reverse positions)))))

;last: returns the last element of 'lst'

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

;filter: takes elements from 'lst' that satisfies 'pred' and then uses recursion to process each elements in the list
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

;Precondition: A predicate function pred and a list lst.
;Postcondition: a new list containing only those elements from lst for which the predicate holds true.
;Base Case: If the list is empty, the function returns an empty list.
;Inductive Hypothesis: Assuming the recursive call (filter pred (cdr lst)) properly returns the filtered result for any sub-list,
;the current implementation of filter correctly processes the current element based on the predicate.
;Inductive Step: The function checks the predicate for the first element (car lst). If true, it includes that element in the result by
;cons-ing it with the recursive call on the rest of the list (cdr lst). If false, it skips that element and proceeds with the recursive call.
;Stopping Condition: The function stops when it reaches the end of the list (null? lst), and it returns the filtered result.


;subsets: generates all possible subsets of 'lst'

(define (subsets lst)
  (if (null? lst)
      '(())
      (let ((rest-subsets (subsets (cdr lst))))
        (append rest-subsets
                (map (lambda (subset) (cons (car lst) subset)) rest-subsets)))))

;Pre: lst is a list
;Post: will return all possible subsets of the elements in lst, including the empty list
;Base Case: If the list is empty, the function returns an empty list.
;Inductive Hypothesis (IH): Assuming the recursive call (subsets (cdr lst)) properly returns the subsets for any sub-list, the current implementation of subsets correctly
;processes the current element and generates subsets.
;Inductive Step (IS): The function calculates the subsets of the rest of the list (cdr lst) using the recursive call.
;It then appends these subsets with a modified set of subsets where the first element (car lst) is added to each subset.
;TA: The function will terminate once lst has been cdr'd down recursively into an empty list
;SC: The function will stop if lst is an empty list


;backtrack: a general purpose backtracking function

(define (backtrack base-case inital-state problem change placer)
  (cond ((base-case problem) inital-state)
        (else (let* ((new (change problem))
                    (rest (backtrack base-case inital-state new change placer)))                 

                (placer problem rest)))))

;Pre: base-case: defined by the problem we are solving, e.g, subsets and n-queens had their base-case passed through the 'backtrack' function
;     initial-state: A function that takes the first state of the problem
;     problem: the initial problem to be solved 
;     change: a function that will decrement the problem
;     placer: a function that takes the problem and changed problem and applys the backtracking logic

;Post: The function will return a solution of original problem

;IH: Assuming the recursive call (backtrack base-case initial-state new change placer) properly returns the result for the current problem,
;the current implementation of backtrack correctly processes the current problem state.

;IS: Assuming IH holds,  The function generates a new state by applying the change function to the current problem state. It then recursively calls itself with the new state,
;obtaining the result in rest. Finally, it combines the current problem state and the result of the recursive call using the placer function.

;TA: The Function will terminate once 'problem' reaches the 'base-case' and returns the correct result of a backtracking problem.
;SC: The Function will stop once 'problem' reaches 'base-case' and returns 'inital-state'.

;subsets
;This 'subsets' function is being fed through the 'backtrack' function,
;specifying the base case, inital state, problem change, placer function

(define (subsets lst)
(define (subset-base-case lst)
  (null? lst))
(define inital-state '(()))
(define (change-subset lst) (cdr lst)) 

(define (sub-k re para) (append para (map (lambda (subset) (cons (car re) subset)) para)))

(backtrack subset-base-case inital-state lst change-subset sub-k))
(display "Subsets: ")
(subsets '(1 2 3))
(newline)

;Pre: lst is a list

;Post: Using the backtracking function on the subsets problem,
;      it will return the subsets of the given list 'lst'

;Base case: if list is empty, return list.

;IH: Assume the recursive call '(backtrack subset-base-case inital-state lst change-subset sub-k))'
;   properly returns the lists of subsets for all elements of 'lst'

;IS: Assuming IH Holds, we pass 'lst' on 'change-subset' that will cdr down 'lst', 
;    and then combine the current element with all subset using the 'sub-k'

;T: The function will terminate when 'subset-base-case' is reached,
;    i.e, when 'lst' gets empty from each recursive call, because a valid solution is found and returned.

;SC: The function will stop once the base case is reached and a valid solution is found and returned.

;queens
;This 'queens' function is being passed through the 'backtrack' function,
;specifying the base case, initial state, problem change, and the placer function

(define (queens number)
 (define (queens-base-case n) (= n 0))
 (define inital-state '(()))
 (define (change-queens n) (- n 1))
 (define (killer prob rest) (filter
          (lambda (positions) (safe? prob positions))
          (append-map
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row prob rest-of-queens))
                   (enumerate-interval 1 number)))
            rest)))
(backtrack queens-base-case inital-state number change-queens killer))

(display "N-Queens: ")
(queens 4)
(newline)
;Pre: number >= 4

;Post: Using the backtracking function on the queens problem,
;      it will return the correct positions of n queens on an n by n chess board

;IH: Assume that the recursive call '(backtrack queens-base-case inital-state number change-queens killer)'
;    correctly returns the list of all valid positions of the queens given by 'number'

;IS: Assuming IH holds, we pass 'number' to 'change-queens' which will decrement 'number',
;    and then 'killer' will filter out invalid positions for the queen in the current column
;    by using 'safe?' function and generate new positions for the next column

;TA: With each recursive call, 'number' will decrease until the base case will be reached, at which point a valid solution is found and returned.

;SC: When the base case is reached, the recursion stops because a valid solution is found and returned.


; permutations
(define (permutations lst)
  (define (permutations-base-case lst)
    (null? lst))
  
  (define initial-state '(()))
  
  (define (change-permutations lst) (cdr lst))
  
  (define (placer-permutations lst rest)
    (append-map
     (lambda (x)
       (map (lambda (p) (cons x p))
            (permutations (remove x lst))))
     lst))
  
  (backtrack permutations-base-case initial-state lst change-permutations placer-permutations))

(display "Permutations: ")
(display (permutations '(1 2 3)))
(newline)

;Proof
;Precondition: A list lst.
;Postcondition: The result is a list of all permutations of the input list lst.
;Base Case: The base case is defined by the permutations-base-case predicate, which checks if the list lst is empty.
;Inductive Hypothesis (IH): Assuming the recursive call (backtrack permutations-base-case initial-state new change-permutations placer-permutations)
;properly returns the result, the current implementation of permutations correctly processes the current permutation.
;Inductive Step (IS): The function uses the backtrack algorithm to generate permutations iteratively. It considers the base case, generates new permutations using the
;change-permutations function, and combines permutations using the placer-permutations function.
;Stopping Condition: The function stops when the base case is reached (null? lst), and it returns the permutations of the empty set.